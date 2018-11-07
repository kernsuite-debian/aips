C STILL TO DO:
C POSSIBLE TO DO EASILY:
C  Improve KSTACK algorithm for gridding the data in time
C   to use a grid that minimzes the occupancy of all cells
C   [on all baselines? - for best stacking]
C  Add an extra step to determine the L-R phase offsets using a LS algorithm
C  Add extra SOLMODs = 'GEOS' and 'GEOX' to separately fringe fit
C     IFs 1-4 and 5-8 respectively for multi-band delays
LOCAL INCLUDE 'KRING.CONSTS'
      DOUBLE PRECISION SEC2NS, NS2SEC, VMIN, DY2SEC
      PARAMETER (NS2SEC = 1.0D-9)
      PARAMETER (SEC2NS = 1.0D9)
      PARAMETER (DY2SEC = 8.64D4)
      PARAMETER (VMIN = 1.0D-20)
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION CUTOF
      COMMON /CUTLIM/ CUTOF
LOCAL END
LOCAL INCLUDE 'KRING.INC'
C                                     Include KRING
C                                     Local include for KRING
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXPRM, XBFSZ
C                                     MAXPRM = maximum no. parms in
C                                     Least squares solutions
      PARAMETER (MAXPRM = MAXANT * 4)
C                                     XBFSZ = buffer size
      PARAMETER (XBFSZ = UVBFSL)

      INTEGER  INCAT(256), SEQIN, SEQ2, DISKIN, DISK2,
     *   CNOIN, CNOOUT, JBUFSZ, BUFFS(XBFSZ), NANT, CNOIN2, CCTVER,
     *   REFANT, NPOL, NVAL, SNVER, VER, NUMNOD, NUMTIM, PRTLEV,
     *   BLDO, LOCIF, LOCF, ASRCH, NOEIF, NOAIF, NFQ,
     *   GSOLVE(MAXANT), LBIF, LEIF, DOEVLA
      LOGICAL   TSMOTH, DOIF, ISIQUV,
     *   AVGPOL, DODRLS, DAVOUT, ZDEL, ZRAT, ZPHS, GOBACK,
     *   DODRFT, DORATE, DODLAY, DODLBY
      INTEGER   NCOMP(MAXFLD), REFUSE(MAXANT), PRIRTY(MAXANT), PRMAX,
     *   REFUSS(MAXANT,100)
      INTEGER   SRCHL(MXBASE), NSRCH, FFTFAX
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC, XNAME2(3),
     *   XCLAS2(2), XSOLTY, XSOLMO, XOPCOD, XCMETH, XCMOD
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XCALCO*4, NAME2*12,
     *   CLAS2*6, CMETH*4, CMOD*4
      REAL     XSI, XDI, XQUAL, XBAND, XFREQ, XFQID, XTIME(8), XBCHAN,
     *   XECHAN, XANTS(50), XSUBA, XUVRA(2), XWTUV, XWTIT, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH(3), XS2, XD2, XVER, XNCOMP(MAXAFL), XFLUX, XNMAP,
     *   SMODEL(7), XREFA, XSOLIN, XSOLS, XSOLM, CPARM(10), XSNVER,
     *   XANTWT(30), XBIF, XEIF, XBADD(10), DELTIM, BUFF1(XBFSZ),
     *   BUFF2(XBFSZ), ANTWT(MAXANT), IATOFF, TINT, TINTG, SOLINT,
     *   SNRFFT, SNRLS, NSRLS, NSRFFT, MXPABL, MNPABL, WTPABL, TRYHRD,
     *   XORD(10), XDOFIT(30), RWINDO, DWINDO, XPRTL, XDOIFS
      DOUBLE PRECISION  RANOD, DECNOD, DFINC(MAXIF)
      REAL FINC(MAXIF)
      INTEGER ISBAND(MAXIF)
      COMMON /CINFO/ RANOD, DECNOD, INCAT, DELTIM, TINT, IATOFF,ANTWT,
     *   TINTG, SOLINT, SNRFFT, SNRLS, NSRLS, NSRFFT, MXPABL, MNPABL,
     *   WTPABL, NCOMP, REFUSE, TSMOTH, DORATE, DODLAY, DODLBY, REFUSS,
     *   DOIF, AVGPOL, DODRLS, DODRFT, DAVOUT, PRIRTY, PRMAX,
     *   GSOLVE, ASRCH, NOEIF, NOAIF, NFQ, CNOIN, CNOOUT, NANT,
     *   NPOL, NVAL, REFANT, CNOIN2, CCTVER, SNVER, VER, NUMNOD,
     *   NUMTIM, PRTLEV, BLDO, LOCIF, LOCF, ZDEL, ZRAT, ZPHS,
     *   RWINDO, DWINDO, ISIQUV, SRCHL, NSRCH, TRYHRD, GOBACK, FFTFAX,
     *   DISKIN, DISK2, SEQIN, SEQ2, LBIF, LEIF, DOEVLA
      COMMON /BUFRS/ BUFF1, BUFF2, BUFFS, DFINC, FINC, ISBAND, JBUFSZ
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XCALCO, NAME2, CLAS2, CMETH,
     *   CMOD
      COMMON /XINPUT/ XNAMEI, XCLAIN, XSI, XDI, XXSOUR, XQUAL, XXCALC,
     *   XBAND, XFREQ, XFQID, XTIME, XBCHAN, XECHAN, XANTS, XDOFIT,
     *   XSUBA, XUVRA, XWTUV, XWTIT, XDOCAL, XGUSE, XDOPOL, XPDVER,
     *   XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XNAME2, XCLAS2, XS2,
     *   XD2, XVER, XNCOMP, XFLUX, XNMAP, XCMETH, XCMOD, SMODEL, XREFA,
     *   XORD, XSOLIN, XSOLS, XSOLM, XSOLTY,XSOLMO, XDOIFS, XOPCOD,
     *   CPARM, XSNVER, XANTWT, XBIF, XEIF, XPRTL, XBADD
LOCAL END
      PROGRAM KRING
C---------------------------------------------------------------------
C! fringe-fit interferometer data.
C# UV Calibration EXT-appl AP-appl
C---------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2003-2012, 2014-2015, 2017
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
C---------------------------------------------------------------------
C   This task determines the delay and rate calibrations for uv data
C   given a model of the source(s).  The output data will have the
C   corrections applied for a single source input file; and the
C   SN table will be updated for a multi source data set.
C   Adverbs:
C                                      Input uv data.
C   INNAME                                UV file name (name)
C   INCLASS                               UV file name (class)
C   INSEQ              0.0      9999.0    UV file name (seq. #)
C   INDISK             0.0         9.0    UV file disk drive #
C                                      Data selection:
C   CALSOUR                            Calibrator sources
C   QUAL                               Qualifier
C   CALCODE                            Calibrator code.
C   TIMERANG                           Time range to use.
C   BCHAN             0.0     2048.0   Lowest channel number 0=>all
C   ECHAN             0.0     2048.0   Highest channel number
C   ANTENNAS                           Antennas to solve for.
C   SUBARRAY          0.0     1000.0   Subarray, 0=>all
C                                      Cal. info for input:
C   DOCALIB          -1.0       10.0   If >0 calibrate data
C   FLAGVER                            Flag table version (0=none)
C   DOBAND           -1.0       10.0   if > 0 do bandpass calibr.
C   BPVER                              BP table to apply
C   SMOOTH                             Smoothing function.
C   GAINUSE                            CL table to apply
C                                      CLEAN map (optional)
C   IN2NAME                               Cleaned map name (name)
C   IN2CLASS                              Cleaned map name (class)
C   IN2SEQ            0.0     9999.0      Cleaned map name (seq. #)
C   IN2DISK           0.0        9.0      Cleaned map disk unit #
C   INVERS           -1.0      255.0   CC file version #.
C   NCOMP                              # comps to use for model.
C                                      1 value per field
C   FLUX                               Lowest CC component used.
C   NMAPS             0.0      512.0   No. Clean map files
C                                      Output uv data file.
C   OUTNAME                               UV file name (name)
C   OUTCLASS                              UV file name (class)
C   OUTSEQ            -1.0      9999.0    UV file name (seq. #)
C   OUTDISK            0.0         9.0    UV file disk drive #
C   CPARM                              Delay-rate parameters
C   SNVER                              Ouptut SN table.
C   ANTWT                              Ant. weights (0=>1.0)
C   GAINERR(30)                        Std. Dev. of antenna gains.
C   BADDISK            0.0         9.0 Disk no. not to use for
C                                         scratch files.
C---------------------------------------------------------------------
      CHARACTER PRGM*6, SOLMOD*4
      INTEGER   MBL, MFQ, MTM, MAN, MIF, MCH
      INTEGER   ISUB, IST, IRET, SUBBEG, SUBEND
      INCLUDE 'KRING.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'KRING '/
C---------------------------------------------------------------------
C                                     Get input parameters and
C                                     create output file if nec.
      CALL KRNGIN (PRGM, SUBBEG, SUBEND, MBL, MFQ, MTM, MAN, MIF, MCH,
     *   SOLMOD, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'KRNGIN Error processing input parameters'
         CALL MSGWRT (6)
         GO TO 990
         END IF
C                                     Loop over subarrays
      MSGTXT = 'Almost ready to start figuring out solutions...'
      CALL MSGWRT (2)
C                                     save subarray number
      IST = SUBARR
      DO 100 ISUB = SUBBEG, SUBEND
         WRITE (MSGTXT,1000) ISUB, SUBEND
         CALL MSGWRT (2)
         SUBARR = ISUB
C                                     Do solutions.
         CALL KALLOC (MBL, MFQ, MTM, MAN, MIF, MCH, ISUB, SOLMOD,
     *      IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'KALLOC Error while fringe-fitting.'
            CALL MSGWRT (6)
            GO TO 990
            END IF
 100     CONTINUE
C                                     Smooth solutions
      IF (CPARM(8).LE.1.0E-10) THEN
         CALL KRNADJ (SUBBEG, SUBEND, MIF, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'KRNADJ Error while re-referencing.'
            CALL MSGWRT (6)
            GO TO 990
            END IF
         END IF
C                                     Restore input subarray number
      SUBARR = IST
C                                     Write history.
      CALL KRNHIS (SOLMOD)
C                                     Close down files, etc.
 990  CONTINUE
      CALL DIE (IRET, BUFF1)
C
 999  STOP
C---------------------------------------------------------------------
 1000 FORMAT (' PROCESSING SUBARRAY ',I4,' OF ',I4)
      END
      SUBROUTINE KRNGIN (PRGN, SUBBEG, SUBEND, MBL, MFQ, MTM, MAN,
     *   MIF, MCH, SOLMOD, IRET)
C---------------------------------------------------------------------
C   KRNGIN gets input parameters for KRING and creates an output file
C   if necessary.
C   Inputs:  PRGN    C*6       Program name
C   Output:  MBL     I         Maximum number of baselines
C            MFQ     I         Maximum number of frequency channels.
C            MTM     I         Max number of integrations per solint
C            MAN     I         Maximum number of antenna codes
C            MIF     I         Maximum number of IFs
C            SUBBEG  I         Beginning subarray
C            SUBEND  I         Ending subarray
C            IRET    I         Error code: 0 => ok
C                                1 => too few frequency channels.
C                                5 => catalog troubles
C                                7 => Too many ant. for ls.
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in KRING for more details.
C---------------------------------------------------------------------
      INCLUDE 'KRING.CONSTS'
      CHARACTER CHTM2*2, PRGN*6, STAT*4, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   MBL, MFQ, MTM, MAN, MIF, MCH, IRET, SUBBEG, SUBEND
      INTEGER   IERR, NPARM, I, MXFLD, IROUND,
     *   ANVER, MAXA
      INTEGER   J, K, CURANT, LUNTB
      CHARACTER SOLTYP*4, SOLMOD*4, OPCODE*4, SOLCOD*4
      INTEGER INDSK, INCNO, IVER
      LOGICAL   T, TABLE, EXIST, FITASC, MATCH, SINGLE, HAVENX
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128), DLAMCH
      INCLUDE 'KRING.INC'
      INTEGER NASEL, I1, I2
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DANT.INC'
      EQUIVALENCE (CATR, CATBLK, CATH, CATD)
      DATA T /.TRUE./
C---------------------------------------------------------------------
      LUNTB = 29
C                                     Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSL * 2
C                                     Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
      NSOUWD = 1
C                                     Get input parameters.
      MXFLD = MAXAFL
      NPARM = 346 + MXFLD - 7 - 10
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
C                                     Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                     Crunch input parameters.
      SEQIN = IROUND (XSI)
      SEQ2 = IROUND (XS2)
      DISKIN = IROUND (XDI)
      DISK2 = IROUND (XD2)
      CCTVER = IROUND (XVER)
      CCTVER = MAX (0, CCTVER)
C                                     Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6,  1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6,  1, XCLAS2, CLAS2)
      CALL H2CHR (4,  1, XXCALC, XCALCO)
      CALL H2CHR (4,  1, XSOLTY, SOLTYP)
      CALL H2CHR (4,  1, XSOLMO, SOLCOD)
      CALL H2CHR (4,  1, XCMETH, CMETH)
      CALL H2CHR (4,  1, XCMOD, CMOD)
      CALL H2CHR (4,  1, XOPCOD, OPCODE)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 10      CONTINUE

C                                     Get CATBLK from old file.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         CALL MSGWRT (6)
         GO TO 990
         END IF
C                                     save original CATBLK and
C                                     other info!
      INDSK = DISKIN
      INCNO = CNOIN
C                                     put input file in read
      CALL CATIO ('READ', INDSK, INCNO, CATBLK, 'REST', BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         CALL MSGWRT (6)
         GO TO 990
         END IF
C                                     The file must be multi-source
      IERR = 0
      IVER = 1
      MSGSUP = 32000
      CALL MULSDB (CATBLK, SINGLE)
      IF (SINGLE) THEN
         CALL ISTAB ('SU', INDSK, INCNO, IVER, LUNTB, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         SINGLE = EXIST .AND. (IERR.EQ.0)
         END IF
      SINGLE = .NOT.SINGLE
      MSGSUP = 0
      IF (SINGLE) THEN
         IRET = 1
         MSGTXT = 'Sorry, KRING only runs on multi-source files.'
C        CALL MSGWRT (6)
         MSGTXT = 'Please run MULTI to convert this data.'
C        CALL MSGWRT (6)
C        GO TO 990
         END IF
C                                     The file must have an NX table
      IERR = 0
      IVER = 1
      MSGSUP = 32000
      CALL ISTAB ('NX', INDSK, INCNO, IVER, LUNTB, BUFF1, TABLE,
     *   EXIST, FITASC, IERR)
      HAVENX = EXIST .AND. (IERR.EQ.0)
      IERR = 0
      MSGSUP = 0
      IF (.NOT.HAVENX) THEN
         IRET = 1
         MSGTXT = 'Sorry, KRING requires an NX table.'
         CALL MSGWRT (6)
         MSGTXT = 'Please run INDXR to create an NX table.'
         CALL MSGWRT (6)
         GO TO 990
         END IF
C                                     Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C
      IF (ISORT(1:1) .NE.'T') THEN
         IRET = 1
         MSGTXT = ' KRING requires T* sorted data.'
         CALL MSGWRT (6)
         MSGTXT = '  Please run UVSRT or MSORT.'
         CALL MSGWRT (6)
         GO TO 990
         END IF
C                                     Save IF and freq pointers
      LOCIF = JLOCIF
      LOCF = JLOCF
C                                     Freq id
      SELBAN = MAX (XBAND, 0.0)
      SELFRQ = MAX (XFREQ, 0.0)
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      CALL FQMATC (INDSK, INCNO, CATBLK, LUNTB, SELBAN, SELFRQ,
     *     MATCH, FRQSEL, IRET)
      IF (IRET.GT.0) GO TO 999
      IF (.NOT.MATCH) THEN
         MSGTXT = ' Sorry, no match to SELBAND/SELFREQ'
         CALL MSGWRT (6)
         MSGTXT = '  Please check inputs and try again.'
         IRET = 1
         CALL MSGWRT (6)
         GO TO 990
         END IF
C
C                                     Set antenna weights
      CALL RFILL (MAXANT,1.0,ANTWT)
      DO 20 I = 1,30
         IF (XANTWT(I).GT.0.0) ANTWT(I) = XANTWT(I)
 20      CONTINUE
C
C                                     Set solution interval
      IF (XSOLIN.LE.0.0) THEN
         SOLINT = 600.0
      ELSE
         SOLINT = 60*XSOLIN
         IF ((SOLINT.GT.600.0).AND.(CPARM(9) .LE.1.0E-10)) THEN
            SOLINT = MIN (SOLINT, 600.0)
            WRITE (MSGTXT, 1041) XSOLIN
            CALL MSGWRT (6)
            WRITE (MSGTXT, 1042)
            IRET = 1
            CALL MSGWRT (6)
            GO TO 990
         END IF
         END IF
C
C                                     Set range of UV baselines
      MXPABL = XUVRA(2)
      IF (MXPABL.LE.1.0E-20) MXPABL = 1.0E15
      MNPABL = XUVRA(1)
C
C                                     Set UV weight
      WTPABL = XWTUV
      PRTLEV = IROUND (XPRTL)
C
C                                     Set FFT SNR threshold
      SNRFFT = CPARM(4)
      IF (SNRFFT.LT.   -0.5) SNRFFT = -1.0
      IF (ABS(SNRFFT).LE. 1.0E-5) SNRFFT =  3.0
C                                     do FT solutions?
      DODRFT = SOLTYP(1:4).NE.'NOFT'
      IF (.NOT.DODRFT) SNRFFT = -1.0
C                                     Set FFT default SNR threshold
C                                     for antennas _not_ to be
C                                     solved for
      NSRFFT = 1./(11.0*ABS(SNRFFT))
C
C                                     do LS solutions?
      DODRLS = SOLTYP(1:4).NE.'NOLS'
C                                     Set LS SNR threshold
      SNRLS = CPARM(4)
      IF (SNRLS.LT.   -0.5) SNRLS = -1.0
      IF (ABS(SNRLS).LE. 1.0E-5) SNRLS =  10.0
      IF (.NOT.DODRLS) SNRLS = -1.0
      NSRLS = 1./SNRLS
C
C                                     Set extrapolation threshold
C                                     SNRs
      TRYHRD = CPARM(10)
      IF (CPARM(10).LT. 0.0001) TRYHRD = -1.0

      CUTOF = DLAMCH ('Epsilon')
C
C                                     Set reverse traversal flag
      GOBACK = (CPARM(10).GT.0).AND.(TRYHRD.GT.0.0)
C                                     Set data zeroing options
      ZPHS = (OPCODE(1:4).EQ.'ZPHS')
     *     .OR.(OPCODE(1:3).EQ.'ZPR')
     *     .OR.(OPCODE(1:3).EQ.'ZPD')
      ZRAT = (OPCODE(1:4).EQ.'ZRAT')
     *     .OR.(OPCODE(1:3).EQ.'ZPR')
     *     .OR.(OPCODE(1:3).EQ.'ZRD')
      ZDEL = (OPCODE(1:4).EQ.'ZDEL')
     *     .OR.(OPCODE(1:3).EQ.'ZPD')
     *     .OR.(OPCODE(1:3).EQ.'ZRD')
C                                     set BADDISK
      DO 30 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 30      CONTINUE
C                                     Set source/data selection
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DO 40 I = 1,30
         SOURCS(I) = XSOUR(I)
         CALSOU(I) = XSOUR(I)
 40      CONTINUE
      SELQUA = IROUND (XQUAL)
      SELCOD = XCALCO
C                                     Set timerange
      CALL RCOPY (8, XTIME, TIMRNG)
C                                     Set Freq selection
      BCHAN = IROUND (XBCHAN)
      IF (BCHAN.GT.CATBLK(KINAX+JLOCF)) BCHAN = CATBLK(KINAX+JLOCF)
      BCHAN = MAX (1, BCHAN)
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      IF (ECHAN.GT.CATBLK(KINAX+JLOCF)) ECHAN = CATBLK(KINAX+JLOCF)
      MCH = ECHAN - BCHAN + 1
C                                       Set IF selection
      BIF = 1
      EIF = 1
      IF (JLOCIF.GE.0) EIF = CATBLK(KINAX+JLOCIF)
      MIF = EIF - BIF + 1
C                                     Map possible codes to standard
C                                     positions in SOLMOD
      IF (SOLCOD(1:4).EQ.'NRS ') SOLCOD = 'NRD'
      IF (SOLCOD(1:4).EQ.'NS  ') SOLCOD = 'ND'
      IF (SOLCOD(1:4).EQ.'    ') SOLCOD = 'NRD'
      SOLMOD = '    '
      FFTFAX = 8
      DO 45 I = 1,4
         IF (SOLCOD(I:I).EQ.'N') SOLMOD(1:1) = 'N'
         IF (SOLCOD(I:I).EQ.'R') SOLMOD(2:2) = 'R'
         IF (SOLCOD(I:I).EQ.'D') SOLMOD(3:3) = 'D'
C                                     'S' IS THE SAW-TOOTH DELAY FUNCTION
         IF (SOLCOD(I:I).EQ.'S') SOLMOD(4:4) = 'S'
C                                     'I' IS THE IONOSPHERIC APPROXIMATION FUNCTION
         IF (SOLCOD(I:I).EQ.'I') SOLMOD(4:4) = 'I'
C                                     'T' IS THE TOTAL IONOSPHERIC FUNCTION
         IF (SOLCOD(I:I).EQ.'T') SOLMOD(4:4) = 'T'
         IF (SOLCOD(I:I).EQ.'4') FFTFAX = 4
         IF (SOLCOD(I:I).EQ.'2') FFTFAX = 2
         IF (SOLCOD(I:I).EQ.'1') FFTFAX = 1
 45      CONTINUE
      DOEVLA = XDOIFS + 0.1
      DOEVLA = MAX (1, DOEVLA)
      IF (SOLMOD(4:4).EQ.'T') DOEVLA = 1
      IF (SOLMOD(1:1).EQ.'N') DOEVLA = 0
      IF (DOEVLA.GT.1) SOLMOD(1:1) = 'M'
C
C                                     DOIF=T => solve for each IF
C                                     separately
      DOIF   = SOLMOD(1:1).EQ.'N'
      IF (DOIF) THEN
         LBIF = 1
         LEIF = EIF
      ELSE
         LBIF = XBIF + 0.1
         LBIF = MAX (1, MIN (EIF, LBIF))
         LEIF = XEIF + 0.1
         IF (LEIF.LT.LBIF) LEIF = EIF
         LEIF = MAX (1, MIN (EIF, LEIF))
         END IF
C                                       make 0 relative
      LBIF = LBIF - 1
      LEIF = LEIF - 1
C                                       test EVLA modes
      I = 0
      IF (SOLMOD(1:1).EQ.'M') I = MOD (EIF,DOEVLA)
      IF (I.NE.0) THEN
         WRITE (MSGTXT,1045) EIF, DOEVLA
         CALL MSGWRT (8)
         IRET = 10
         GO TO 999
         END IF
C                                     DORATE=T => solve for rates
      DORATE = SOLMOD(2:2).NE.' '
C                                     DODLAY=T => solve for delays
      DODLAY = SOLMOD(3:3).NE.' '
C                                     DODLBY=T => solve for extra delays
      DODLBY = SOLMOD(4:4).NE.' '
C                                     DOIF=T || MIF=1 => DODLBY=F
      DODLBY = DODLBY.AND. (.NOT.DOIF) .AND. (MIF.GT.1)
C
      AVGPOL = CPARM(7).GT.0.0
C                                     how many freq left to FFT?
      MFQ = MCH * MIF
C                                     Check against std array sizes
      IF ( (MCH.GT.MAXCHA) .OR.
     *     (MIF.GT.MAXIF) .OR.
     *     (MFQ.GT.MAXCIF)      ) THEN
         MSGTXT = ' Freq buffers too small!'
         CALL MSGWRT(6)
         MSGTXT = '  Please restrict BCHAN/ECHAN to a smaller range'
         IERR = 1
         CALL MSGWRT (6)
         GO TO 990
         END IF
C                                     how many IFs to fringe-fit?
C                                     how many IFs were averaged?
      IF (DOIF) THEN
         NOAIF = 1
         NOEIF = MIF
      ELSE IF (SOLMOD(1:1).EQ.'M') THEN
         NOAIF = MIF / DOEVLA
         NOEIF = DOEVLA
      ELSE
         NOAIF = MIF
         NOEIF = 1
         END IF
C                                     # freq left per eff. IF?
C                                     two equivalent calculations:
      NFQ = MFQ / NOEIF
C     MFQ = MCH * MIF = MCH * NOAIF * NOEIF = NFQ * NOEIF
C     NFQ = MCH * NOAIF
C                                     Set delay window
C                                     [Full width in nanoseconds]
      DWINDO = CPARM(2)
      IF (DWINDO.LE.VMIN) DWINDO = SEC2NS / CATR(KRCIC+JLOCF)
      IF (.NOT.DODLAY) DWINDO = -1.0D0
C
C                                     Close delay window if only one
C                                     channel selected!
C      IF (BCHAN.EQ.ECHAN) DWINDO = -1
C                                     Set calibration options
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      SNVER = IROUND (XSNVER)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      BLVER = IROUND (XBLVER)
C                                     Set spectral processing options
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                     Set stokes type
      STOKES = '    '
      ISIQUV = CATD(KDCRV+JLOCS).GT.0.0
      IF (ISIQUV) THEN
         STOKES = 'I   '
      ELSE
         IF (CATBLK(KINAX+JLOCS).GE.2) STOKES = 'HALF'
         END IF
C                                       at this point, only
C                                       STOKES = 'I', 'HALF', or one
C                                       STOKES = '    '
C                                            [passes one of -1 to -8]
C                                       is allowed
C
C                                     check stacking parm
      BLDO = IROUND (CPARM(5))
      IF ((BLDO.NE.1).AND.(BLDO.NE.2)) BLDO = 3
C
      IRET = 0
      IATOFF = 0.0
C                                     Set subarray range
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.GT.0) THEN
         SUBBEG = SUBARR
         SUBEND = SUBARR
      ELSE
         SUBBEG = 1
         SUBEND = 1
         DO 50 I = 1,KIEXTN
            CALL H2CHR (2, 1, CATH(KHEXT+I-1), CHTM2)
            IF (CHTM2.EQ.'AN') SUBEND = MAX (SUBEND,
     *                                       CATBLK(KIVER+I-1))
 50         CONTINUE
         END IF
C                                     Need two numbers
C                                     MANTNO = max antenna number

C                                     NANT   = max # antennas
C                                     Get max antenna # in AN tables
      MAXA = 0
      DO 60 ANVER = SUBBEG, SUBEND
         CALL ANMAXA (DISKIN, CNOIN, ANVER, CATBLK, I, IRET)
         IF (IRET.NE.0) THEN
         ELSE
            MAXA = MAX (MAXA, I)
            END IF
 60      CONTINUE
      IRET = 0
C                                     process ANTENNAS list supplied
C                                     by user, remove zeros,
C                                     duplicates, sort entries
C
C                                     NASEL is the # of entries in
C                                     the list entries are negative
C                                     if deselection is in effect.
      DO 70 I = 1,50
         ANTENS(I) = IROUND(XANTS(I))
 70      CONTINUE
      CALL CNVANT (ANTENS, 50, NASEL)
C                                     NANT= max # antennas expected
C                                     MAN = max antenna # expected
C                                     MBL = max # baselines expected
C                                     NUMANT = MAN
      IF (ANTENS(1).LT.0) THEN
         NANT = MAXA - NASEL
C                                     find max not-deselected
C                                     antenna number
         MAN = MAXA
         DO 80 I=NASEL,1,-1
            IF (ANTENS(I).EQ.MAN) THEN
               MAN = MAN - 1
               END IF
 80         CONTINUE
      ELSE
         IF (NASEL.GT.0) THEN
            NANT = NASEL
            MAN  = ANTENS(NASEL)
         ELSE
            MAN  = MAXA
            NANT = MAXA
            END IF
         END IF
      NUMANT = MAN
      MBL = (NANT * ( NANT - 1 ))/2
C
      IF (NANT.LE.1) THEN
         MSGTXT = ' Problem determining number of antennas:'
         CALL MSGWRT (8)
         MSGTXT = '   try setting ANTENNAS adverb.'
         IRET = 8
         CALL MSGWRT (6)
         GO TO 990
         END IF
C
      REFANT = IROUND(XREFA)
      IF (REFANT.GT.MAN) REFANT = 0
C
      CALL FILL (MAXANT, 0, REFUSE)
      DO 90 I = 1,30
         REFUSE(I) = IROUND(XDOFIT(I))
 90      CONTINUE
      CALL CNVANT (REFUSE, 30, NASEL)
      K = SIGN (1, -REFUSE(1) )
      CALL FILL (MAXANT, K, GSOLVE)
C                                       DOFIT option chosen
      IF (NASEL.GT.0) THEN
         DO 100 I = 1,NASEL
            J = ABS(REFUSE(I))
            IF (J.GT.0) GSOLVE(J) = -K
 100        CONTINUE
         IF (REFANT.LE.0) THEN
            MSGTXT = 'REFANT MUST BE > 0 WITH DOFIT OPTION'
            IRET = 8
            GO TO 990
         ELSE IF (GSOLVE(REFANT).GT.0) THEN
            MSGTXT = 'REFANT MUST NOT BE IN THE DOFIT LIST'
            IRET = 8
            GO TO 990
            END IF
         END IF
C     IF (REFANT.GT.0) GSOLVE(REFANT) = -1
C                                     use XORD, make priority list
C                                     reject duplicate entries
C                                     and zeros
      CALL FILL (MAXANT, 0, REFUSE)
      DO 110 I = 1,10
         J = IROUND(XORD(I))
         REFUSE(I) = ABS(J)
 110     CONTINUE
      IF (REFANT.GT.0) REFUSE(11) = REFANT
      PRMAX = 0
      CALL FILL (MAXANT, 0, PRIRTY)
      DO 130 I = 1,MAXANT
         CURANT = REFUSE(I)
         IF (CURANT.GT.0) THEN
            DO 120 K = 1,PRMAX
               IF (CURANT.EQ.PRIRTY(K)) CURANT = 0
 120        CONTINUE
            END IF
         IF (CURANT.GT.0) THEN
            PRMAX = PRMAX + 1
            PRIRTY(PRMAX) = CURANT
            END IF
 130     CONTINUE
C                                     ASRCH = # of antennas to
C                                     search to
      IF ((CPARM(6).LT.0.5).OR.(PRMAX.EQ.0)) THEN
         ASRCH = MAN
      ELSE
         ASRCH = PRMAX
         END IF
C                                     assign priorities to rest
C                                     of antennas
      DO 150 I = 1,MAN
         DO 140 K = 1,PRMAX
            IF (I.EQ.PRIRTY(K)) GO TO 150
 140        CONTINUE
         PRMAX = PRMAX + 1
         PRIRTY(PRMAX) = I
 150     CONTINUE
      CALL FILL (MXBASE, 0, SRCHL)
      NSRCH = 0
      DO 170 I = 1,ASRCH
         DO 160 J = I+1,MAN
            I1 = MIN ( PRIRTY(I), PRIRTY(J) )
            IF (I1.GT.0) THEN
               I2 = MAX ( PRIRTY(I), PRIRTY(J) )
               NSRCH = NSRCH + 1
               SRCHL(NSRCH) = I1 * (MAN+1) + I2
               END IF
 160        CONTINUE
 170     CONTINUE
C                                     invert f/ priority t/ antenna
      CALL FILL (MAXANT, 0, REFUSE)
      DO 180 J = 1,MAN
         I = PRIRTY(J)
         REFUSE(I) = J
 180     CONTINUE
      CALL COPY (MAN, REFUSE, PRIRTY)
      CALL FILL (MAXANT,0,REFUSE)
      I = 100 * MAXANT
      CALL FILL (I, 0, REFUSS)
C                                     DONE!!!
C                                     Baseline search list prepared
C                                     Antennas to solve for list
C                                     prepared

C
C                                     Set global default integration
C                                     time [sec]
      IF (CPARM(1).GT.1.0E-20) THEN
         TINTG = CPARM(1)
      ELSE
C                                     Default to 2 'VLBA' seconds
         TINTG = 16 * 0.131072
         END IF

C                                     how many times expected per
C                                     solution interval?
C                                     include a fudge factor because
C                                     an integral # of solints
C                                     may not fit into a single scan
C                                     [add some paranoia]
      MTM = 1.5 * SOLINT / TINTG
      MTM = 1 + MAX(MTM,1)
C                                     GOT MTM
C
C                                     GET RATE WINDOW [full width]
      RWINDO = CPARM(3)
      IF (RWINDO.LE.VMIN) RWINDO = VMIN/2.0
      IF (.NOT.DORATE) RWINDO = VMIN/2.0
C
C                                     save original CATBLK
      CALL COPY (256, CATBLK, INCAT)
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C---------------------------------------------------------------------
 1000 FORMAT ('KRNGIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,' DISK=',I3,' USID=',I4)
 1041 FORMAT (' If you really want SOLINT = ',F6.1, ', set CPARM(9).')
 1042 FORMAT ('  This could cause your machine to hang - be careful.')
 1045 FORMAT ('NUMBER IFS=',I4,' NOT COMPATIBLE WITH ',I2,' PIECES')
 1050 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE CNVANT (ANTLST, NLST, NSEL)
C---------------------------------------------------------------------
C  This subroutine sorts the elements of ANTLST, removes zeros and
C  duplicates and returns the number of non-zero elements in the list.
C  If the antennas are deselected, they are returned as negatives
C---------------------------------------------------------------------
      INTEGER NLST, ANTLST(NLST), NSEL
      INTEGER I, J, K
C---------------------------------------------------------------------
C                                     are any elements negative?
      J = 1
      DO 10 I = 1,NLST
         K = ANTLST(I)
         IF (K.LT.0) J = -1
         ANTLST(I) = ABS(K)
 10      CONTINUE
C                                     sort the list
      CALL ISORT(NLST,ANTLST)
C
      NSEL = 0
      DO 20 I = 1, NLST
C                                     squeeze out zeros
         IF (ANTLST(I).NE.0) THEN
            NSEL = NSEL + 1
            ANTLST(NSEL) = J*ANTLST(I)
            END IF
C                                     squeeze out duplicate entries
         IF (NSEL.GT.1) THEN
            IF (ANTLST(NSEL).EQ.ANTLST(NSEL-1)) NSEL = NSEL - 1
            END IF
 20      CONTINUE
C                                     zero the rest of the array
      DO 30 I = NSEL+1,NLST
         ANTLST(I) = 0
 30      CONTINUE
      RETURN
      END
      SUBROUTINE KALLOC (MBL, MFQ, MTM, MAN, MIF, MCH, ISUB, SOLMOD,
     *     IRET)
C---------------------------------------------------------------------
C   KALLOC declares various arrays outside of the main routine, for
C   the benefit of machines that require overlaying.
C---------------------------------------------------------------------
      INTEGER   MBL, MFQ, MTM, MAN, MIF, MCH, IRET, ISUB
      CHARACTER SOLMOD*4
C
      INCLUDE 'KRING.INC'
      INCLUDE 'INCS:DGDS.INC'
C                                     dynamic array declarations
      INTEGER   NWD, JERR, SOLSUB, SOLMIN
      REAL      TIMB(2), DTIMB(2), VVIS(2), WTB(2)
      LONGINT   OBLCOD, OVVIS, OTIMB, ODTIMB, OWTB, OCPHAZ, OCDELY,
     *    OCRATE, OCSGMA
      DOUBLE PRECISION CPHAZ(2), CDELY(2), CRATE(2), CSGMA(2)
C                                     here are some declarations
C                                     that only appear here to keep
C                                     things tidy.  these are all
C                                     'over-declared'
C                                     but the cost should be minimal
C
C                                     only need MAN
      REAL      SWT(MAXANT)
      DOUBLE PRECISION CMBDEL(MAXANT,2), CDISP(MAXANT,2)
      LOGICAL   GOTANT(MAXANT)
      INTEGER   GOTSLN(MAXANT)
C                                     only need MAN*MAN+MAN-1
      INTEGER   BLINDX(2), PBL
C                                     only need MBL
      INTEGER   BLCODE(MXBASE), NTIME(MXBASE)
C                                     only need MIF
      INTEGER   REFAN(2,MAXIF)
      DOUBLE PRECISION FREQIF(MAXIF)
      REAL      FRREAL(2,MAXIF), FRIMAG(2,MAXIF), FRDELY(2,MAXIF),
     *   FRRATE(2,MAXIF), FRWGHT(2,MAXIF)
C                                     only need MFQ
      INTEGER   IFRQ(MAXCIF)
      DOUBLE PRECISION FREQY(MAXCIF), FREQZ(MAXCIF),
     *   FREQS(MAXCIF)

      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C---------------------------------------------------------------------
      MSGTXT = 'attempting to allocate memory...'
      CALL MSGWRT (2)
C                                       sub interval parameters
      SOLMIN = XSOLM + 0.1
      SOLSUB = XSOLS + 0.1
      IF (SOLSUB.LE.0) SOLSUB = 1
      IF (SOLSUB.GT.10) SOLSUB = 10
      IF ((SOLMIN.LE.0) .OR. (SOLMIN.GT.SOLSUB)) SOLMIN = SOLSUB
C                                     allocate memory
C                                     this is for BLINDX
C                                     allow room for fringe-fitting
C                                     cross-hand auto-corr functions
      PBL = MAN*MAN + 2*MAN
      NWD = PBL
      NWD = (NWD - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'KALLOC', NWD, BLINDX, OBLCOD, IRET)
C                                     this is for VVIS
      NWD = 2 * MTM * MFQ * MBL * 2
      NWD = (NWD - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'KALLOC', NWD, VVIS,
     *   OVVIS, IRET)
C                                     this is for TIMB and DTIMB
      NWD = MTM * MBL
      NWD = (NWD - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'KALLOC', NWD, TIMB, OTIMB,
     *   IRET)
      NWD = MTM * MBL
      NWD = (NWD - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'KALLOC', NWD, DTIMB,
     *     ODTIMB, IRET)
      NWD = MTM * MBL
      NWD = (NWD - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'KALLOC', NWD, WTB, OWTB,
     *   IRET)
C                                       this is for CPHAZ,CDELY,etc.
C                                       note that they are real*8
      NWD = 2 * MIF * MAN * 2
      NWD = (NWD - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'KALLOC', NWD, CPHAZ,
     *   OCPHAZ, IRET)
      OCPHAZ = OCPHAZ / 2 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'KALLOC', NWD, CDELY,
     *     OCDELY, IRET)
      OCDELY = OCDELY / 2 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'KALLOC', NWD, CRATE,
     *   OCRATE, IRET)
      OCRATE = OCRATE / 2 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'KALLOC', NWD, CSGMA,
     *   OCSGMA, IRET)
      OCSGMA = OCSGMA / 2 + 1
      IF (IRET.NE.0) THEN
         MSGTXT = ' Sorry, there is insufficient available memory:'
         CALL MSGWRT (6)
         MSGTXT = '  a - Reduce SOLINT.'
         CALL MSGWRT (6)
         MSGTXT = '  b - Wait until computer is less busy.'
         CALL MSGWRT (6)
         MSGTXT = '  c - Move to a computer with more memory. '
         CALL MSGWRT (6)
      ELSE
         CALL KFIT (MBL, MFQ, MTM, MAN, MIF, MCH, VVIS(1+OVVIS),
     *      TIMB(1+OTIMB), DTIMB(1+ODTIMB), CPHAZ(1+OCPHAZ),
     *      CDELY(1+OCDELY), CRATE(1+OCRATE), CSGMA(1+OCSGMA), CMBDEL,
     *      CDISP, WTB(1+OWTB), FREQY, FREQZ,FREQS, BLCODE,
     *      BLINDX(1+OBLCOD), PBL, SWT,NTIME, IFRQ, REFAN, GOTANT,
     *      GOTSLN, FREQIF, IRET, FRREAL, FRIMAG, FRDELY, FRRATE,
     *      FRWGHT, ISUB, SOLMOD, SOLSUB, SOLMIN)
         END IF
C                                     clear allocations
      CALL ZMEMRY ('FRAL', 'KALLOC', NWD, VVIS, OVVIS, JERR)
C
 999  RETURN
      END
      SUBROUTINE KFIT (MBL, MFQ, MTM, MAN, MIF, MCH, VVIS, TIMB, DTIMB,
     *   CPHAZ, CDELY, CRATE, CSGMA, CMBDEL, CDISP, WTB, FREQY, FREQZ,
     *   FREQS, BLCODE, BLINDX, PBL, SWT, NTIME, IFRQ, REFAN, GOTANT,
     *   GOTSLN, FREQIF, IERR, FRREAL, FRIMAG, FRDELY, FRRATE, FRWGHT,
     *   ISUB, SOLMOD, SOLSUB, SOLMIN)
C---------------------------------------------------------------------
C   KFIT reads thru a data file which has been divided by the model
C   and makes the requested solns which are written into a solution
C   (SN) table.
C   Input:
C    MIF              I    Max. number of IFs.
C    MAN              I    Max. antenna code.
C    MBL              I    Max. number of baselines.
C    MTM              I    Maximum number of time integrations.
C    MFQ              I    Maximum number of frequency channels.
C    VVIS(2,MTM,MFQ,MBL)     R    Work array.
C    TIMB(MTM,MBL)          R    Work array.
C    DTIMB(MTM,MBL)         R    Work array.
C    CPHAZ(MAN,2,MIF)       R    Work array.
C    CDELY(MAN,2,MIF)       R    Work array.
C    CRATE(MAN,2,MIF)       R    Work array.
C    CSGMA(MAN,2,MIF)       R    Work array.
C    CMBDEL(MAN,2)          R    Work array
C    CDISP(MAN,2)           R    Work array
C   From common:
C    SOLINT        R    Solution interval (sec).
C    TINTG         R    Integration time (sec)
C    DWINDO        R    Delay window (nsec)
C    RWINDO        R    Rate window (mHz)
C    REFANT        I    Ref ant to use.
C    DODRLS        L    True if least squares solution wanted.
C    AVGPOL        L    True if RR and LL to be averaged
C    DOIF          L    If true fringe-fit independently in each IF
C    NUMTIM        I    Number of time intervals
C    MIF         I    Number of IFs
C    SNRFFT        R    Minimum acceptable SNR for FFT
C    SNRLS         R    Minimum acceptable SNR for LS
C    PRTLEV         I    Print level
C    BLDO          I    the number of baseline combinations to try for
C                       coarse KRINGe search.
C    ANTWT(20)     R    Antenna weights.
C    CATBLK(256)   I    Output catalog header.
C    INCAT(256)    I    Input catalog header.
C    CNOIN         I    Input data cat. #.
C    CNOOUT        I    Output data cat #.
C    DISKIN        I    Input data disk number.
C    DISOUT        I    Output data disk number.
C    SNVER         I    Version of SN table to use
C    JBUFSZ        I    Buffer size.
C    BUFF1(*)      I    Work buffer
C    BUFF2(*)      I    Work buffer. Used for EQUIVALENCEs.
C   Output:
C    IERR          I    Return code, 0=>OK, otherwise error.
C                                    5=> solution interval too long
C-----------------------------------------------------------------------
      INCLUDE 'KRING.CONSTS'
      INTEGER   MBL, MFQ, MTM, MAN, MIF, MCH, IERR, ISUB, SOLSUB,
     *   SOLMIN
      CHARACTER SOLMOD*4
C     VVIS is indexed by [real,imag] x time x freq x baseline x pol
      REAL      VVIS(2,MTM,MFQ,MBL,2)
      REAL      BPCORE(2), BPSCOR(2), WRK1(2), WRK2(2)
      LONGINT   OBPCOR, OBPSCO, OWRK1, OWRK2
      INTEGER   SIZET, SIZEF, SIZEFT, NEEDT, NEEDF, NEEDFT,
     *   MFD, MTD, MTMIN, MFMIN, LIMTOT, LIMFFT
      REAL      TIMB(MTM,MBL), DTIMB(MTM,MBL)
      DOUBLE PRECISION CPHAZ(MAN,MIF,2),
     *     CRATE(MAN,MIF,2), CDELY(MAN,MIF,2), CSGMA(MAN,MIF,2)
C                                     these are 'over' declared!
      DOUBLE PRECISION CMBDEL(MAN,2), CDISP(MAN,2)
      REAL      WTB(MTM,MBL)
      DOUBLE PRECISION FREQY(MFQ), FREQZ(MFQ), FREQS(MFQ)
      REAL      SWT(MAN)
      REAL FRREAL(2,MIF), FRIMAG(2,MIF), FRDELY(2,MIF), FRRATE(2,MIF),
     *   FRWGHT(2,MIF)
      INTEGER BLCODE(MBL), PBL, BLINDX(PBL),
     *   NTIME(MBL), IFRQ(MFQ), REFAN(2,MIF)
      LOGICAL GOTANT(MAN)
      INTEGER GOTSLN(MAN)
      DOUBLE PRECISION FREQIF(MIF)

      INTEGER IRET

      INCLUDE 'INCS:PUVD.INC'
      CHARACTER KEYWRD*8
      INTEGER   LUNSN, BINDI, J, NBL, NBLANK, I, INDEX, JBL, KBL, ECOR,
     *   BCOR, I1, I2, KDAY1, KHR1, KMN1, KSEC1, KDAY2, KHR2, KMN2,
     *   KSEC2, IROUND, IST, EW, SCNSUB, NUMINT, SNKOLS(MAXSNC),
     *   SNNUMV(MAXSNC), NODENO, JFRQ, JIF, FREQID, NNSOU,  ISNRNO,
     *   CNTOK, CNTBAD, SCNSOU, KEYLOC, KEYTYP, ORIGIN, NUMKEY, NUMEST,
     *   K, Q, NT, MT, NR, ND, MF, NXROW, NXMROW, NF, IIF, IFP, VSTART,
     *   VEND, DUM, NXDIR, MSUB, NQ, LWT, KIF
      REAL      DELT, CATR(256), WT1, CURTIM, GETIME, TIMEX, AETIME,
     *   ABTIME, AMP, CATIR(256), GBTIME, SIUSE, BSLOP, ESLOP, SBTIME,
     *   SDTIME, SETIME, BASEL, MX2BAS, MN2BAS, BLFACT, IFRM, TAU, DT,
     *   DFRAC, RFRAC, GDSGMA, SPCNS, SPCMH, WTS(MXBASE), CURSUB
      DOUBLE PRECISION DF, SUMWT, SMBDEL(MAXANT), SPHAZ(MAXANT),
     *   SDELY(MAXANT), SRATE(MAXANT),  SSGMA(MAXANT)
      LOGICAL   T, F, ISAPPL, NOSRCH, TOOBIG, NEWSCN, BEGSOL, FOPEN
      LOGICAL   ALGOOD, SNGOOD, ENDIT
      CHARACTER*2 STORD(3)
      CHARACTER*8 COLTYT(6), COLUNI(6)
      INTEGER SNROW
      DOUBLE PRECISION TIMEC, CATD(128), TIMRA(2), STIMRA(2)
      INCLUDE 'KRING.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (INCAT, CATIR), (CATBLK, CATR, CATD)
      DATA ISAPPL /.FALSE./
      DATA FOPEN /.FALSE./
      DATA KEYWRD /'SNORIGIN'/
      DATA T,F /.TRUE.,.FALSE./
      DATA STORD /'RR', 'LL', 'I '/
      DATA COLTYT /'ase-line', '   Phase', '    Rate',
     *             '   Delay', 'Dispersi', '     SNR'/
      DATA COLUNI /'Antenna]', '   (deg)', '   (mHz)',
     *             '    (ns)', '(ns/m/m)', '        '/
C-----------------------------------------------------------------------
      LWT = XWTIT + 0.1
      LUNSN = 27
C                                     get timerange [sec],
C                                     check limits
      STIMRA(1) = ((TIMRNG(1)*24 + TIMRNG(2))*60 + TIMRNG(3))*60 +
     *      TIMRNG(4)
      STIMRA(2) = ((TIMRNG(5)*24 + TIMRNG(6))*60 + TIMRNG(7))*60 +
     *      TIMRNG(8)
      IF (STIMRA(2).LT.(STIMRA(1)+0.01))  STIMRA(2) = 4.7E17
C
C                                     convert to days
      TIMRA(1) = STIMRA(1) / 86400.0
      TIMRA(2) = STIMRA(2) / 86400.0
C                                     Purge old SN table?
      IF (SNVER.GT.0) THEN
         MSGTXT = ' Checking/purging records from existant SN table'
         CALL MSGWRT (3)
         NNSOU = NSOUWD
         CALL CALSEL (DISKIN, CNOIN, 'SN', SNVER, INCAT, CLBUFF,
     *      NNSOU, SOUWAN, DOSWNT, NANTSL, ANTENS, DOAWNT, TIMRA,
     *      SUBARR, FRQSEL, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'while purging records'
            CALL MSGWRT (3)
            GO TO 999
            END IF
         END IF
C                                     Init. Gain. file.
      NUMNOD = 0
      NODENO = 0
      GMMOD = 1.0
C                                     IQUV?
      ISIQUV = CATD(KDCRV+JLOCS).GT.0.0
      NUMPOL = 1
      IF ((NCOR.GT.1).AND.(.NOT.ISIQUV)) NUMPOL = 2
      CALL SNINI ('WRIT', CLBUFF, DISKIN, CNOIN, SNVER, INCAT, LUNSN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, MIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1000) IERR, 'initializing UV data file.'
         IERR = 2
         GO TO 999
         END IF
C                                     add ORIGIN keyword to SN table
      KEYLOC = 1
      KEYTYP = 4
      ORIGIN = 0
      NUMKEY = 1
      CALL TABKEY ('WRIT', KEYWRD, NUMKEY, CLBUFF, KEYLOC, ORIGIN,
     *   KEYTYP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1000) IERR, 'putting ORIGIN keywd in SN table'
         IERR = 2
         CALL MSGWRT (6)
         GO TO 999
         END IF
      CALL TABIO ('CLOS', 0, ISNRNO, TIMEC, CLBUFF, IERR)
      IF (IERR.NE.0) THEN
         IERR = 2
         GO TO 999
         END IF
      WRITE (MSGTXT,1010) SNVER
      CALL MSGWRT (3)
      IERR = 0

      MSGTXT = 'Will do FFTs only'
      IF (DODRLS) MSGTXT = 'Will do FFTs followed by LS for SB delays'
      IF (DODLBY.AND.DODRLS)
     *     MSGTXT = 'Will do FFTs, then LS for MB and peculiar delays'
      CALL MSGWRT (2)
C                                     init good and bad soln count
      CNTOK = 0
      CNTBAD = 0
C                                     prep baseline limits
      MX2BAS = MXPABL * MXPABL * 1.0E6
      MN2BAS = MNPABL * MNPABL * 1.0E6
C                                     Setup STOKES
      BCOR = ICOR0
      IF (ISIQUV) THEN
         NPOL = 1
         ECOR = ICOR0
      ELSE
         NPOL = MIN(NCOR,2)
         IF (NPOL.EQ.1) THEN
            ECOR = BCOR
         ELSE
            ECOR = -1 + ICOR0
            END IF
         END IF
      IF (AVGPOL) THEN
         NUMEST = 1
      ELSE
         NUMEST = NPOL
         END IF
C                                     Subarray/source selection occurs here.
      CALL UVGET ('INIT', DUM, DUM, IERR)
      CALL UVGET ('CLOS', DUM, DUM, IERR)
C                                     Init scan info, scan#,
C                                     max scan#,
C                                     NX table exists?
      NXROW = 0
      NEWSCN = .TRUE.
C                                     get max row number here
      CALL GETNXR (DISKIN, CNOIN, INCAT, NXROW, NXMROW, SBTIME, SDTIME,
     *   SCNSOU, SCNSUB, VSTART, VEND, FREQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1000) IERR, 'getting max row# of NX table'
         IERR = 2
         CALL MSGWRT (6)
         GO TO 999
         END IF

      NXROW = 0
C                                     start by assuming that
C                                     all solutions found so far
C                                     were good ones.
      ALGOOD = .TRUE.
      NXDIR = 1
      SNROW = 0
C                                     its the beginning of a soln!
      BEGSOL = T
C                                     init the scan boundary epsilon time
      TAU = TINTG
C                                     no-scan default is Hubble time
C                                     [sec]
      SBTIME = -5.0E17
C                                     no-scan default = Hubble time
C                                     [sec]
      SETIME = 5.0E17
C                                     no scan default is SOLINT
      SIUSE = SOLINT
C                                     no-scan default = select all data
      VSTART = 1
      VEND = 0
C
      GDSGMA = NSRFFT
C                                       proceed!
      SIZEF = 0
      SIZET = 0
      SIZEFT = 0
C                                       BEGIN WITH ALL SOLUTIONS
      IF (PRTLEV.GE.20) WRITE (*,*) 'KFIT start soln loop now'
 100  CONTINUE
      IF (PRTLEV.GE.20) WRITE (*,*) 'at line 100', NEWSCN
C                                       get scan info,
C                                           init scratch file
      IF (NEWSCN) THEN
C                                       trap beyond last scan
C                                       past last scan and some
C                                                solns failed
         IF ((NXROW.EQ.NXMROW).AND.(NXDIR.EQ.1)) THEN
            IF (PRTLEV.GE.20) WRITE (*,*) 'At last scan'
C                                       if some bad,
C                                         try going backwards
C                                        (if so requested)
            IF ((.NOT.ALGOOD).AND.GOBACK) THEN
               NXDIR = -1
               NXROW = NXMROW + 1
C                                       if all solns good, exit now
            ELSE
               IRET = 0
               GO TO 800
               END IF
            END IF
C                                       trap before first scan
C                                        [when we are going backwards]
         IF ((NXROW.EQ.1).AND.(NXDIR.EQ.-1)) THEN
            IF (PRTLEV.GE.20) WRITE (*,*) 'At first scan scan'
            IRET = 0
            GO TO 800
            END IF
C                                       get NX table record
         IF (PRTLEV.GE.20) WRITE (*,*) 'Going to get NX record'
         NXROW = NXROW + NXDIR
         CALL GETNXR (DISKIN, CNOIN, INCAT, NXROW, NXMROW,
     *      SBTIME, SDTIME, SCNSOU, SCNSUB, VSTART, VEND, FREQID,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'reading NX table record'
            IERR = 2
            CALL MSGWRT (6)
            GO TO 999
            END IF
         IF (PRTLEV.GE.20) WRITE (*,*) 'Got NX record'
C                                       trap subarray mismatch
         IF (SCNSUB.NE.ISUB) GO TO 100
C                                       trap freqid match
         IF ((FREQID.NE.FRQSEL).AND.(FRQSEL.GT.0)) GO TO 100
C                                       trap source match
         IF (NOSRCH(SCNSOU, DOCWNT, NCALWD, CALWAN)) GO TO 100
C                                       check scan end,begin  against
C                                            timerang
         IF ((SBTIME+SDTIME/2.).LT.TIMRA(1)) GO TO 100
         IF ((SBTIME-SDTIME/2.).GT.TIMRA(2)) GO TO 100
C                                       get scan duration time [secs]
         SDTIME = SDTIME * DY2SEC
C                                       get scan begin time [secs]
         SBTIME = SBTIME * DY2SEC - SDTIME/2
C                                       get scan end time [secs]
         SETIME = SBTIME + SDTIME
C*********************************************************************
C                                       ADJUST SCAN TIMES assuming
C                                       that half an integration time
C                                       adjustments are necessary!!!!
         SBTIME = SBTIME - TAU/2
         SETIME = SETIME + TAU/2
C*********************************************************************
C                                       truncate scan if necessary!
         SBTIME = MAX ( DBLE(SBTIME), STIMRA(1) )
         SETIME = MIN ( DBLE(SETIME), STIMRA(2) )
C                                       recompute SDTIME
         SDTIME = SETIME - SBTIME
C                                       SIUSE = # of SOLINTS in scan
         IF (SOLINT.GE.SDTIME) THEN
            NUMINT = 1
         ELSE
            NUMINT = 0.5 + SDTIME/SOLINT
            END IF
         SIUSE = SDTIME / NUMINT
C                                       turn off new scan request
         NEWSCN = F
         END IF
      CURSUB = SIUSE / SOLSUB
C                                       init scratch file
      CALL KGTDAT ('INIT', SOLMOD, FOPEN, BINDI, FREQIF, IFRQ, FREQY,
     *   FREQZ, FREQS, MFQ, DF, NF, MIF, MCH, DFRAC, VSTART, VEND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1000) IERR, 'initializing scratch file'
         CALL MSGWRT (6)
         IERR = 3
         GO TO 999
         END IF
C                                       get data
 200  CALL KGTDAT ('READ', SOLMOD, FOPEN, BINDI, FREQIF, IFRQ, FREQY,
     *   FREQZ, FREQS, MFQ, DF, NF, MIF, MCH, DFRAC, VSTART,VEND, IERR)
C                                       current time, int.time [sec]
      CURTIM = BUFF1(BINDI+ILOCT) * DY2SEC
C                                       EOF => do soln!
      IF ((IERR.EQ.5) .OR. (IERR.EQ.6)) THEN
C                                       new scan if NX table exists
         NEWSCN = (NXROW.LT.NXMROW) .OR. ((NXROW.EQ.NXMROW).AND.GOBACK)
C                                       for new scan, we need new soln
         BEGSOL = NEWSCN
         ENDIT = .NOT.NEWSCN
         GO TO 300
         END IF
C                                       anything else, error!
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1000) IERR, 'reading from scratch file'
         CALL MSGWRT (6)
         IERR = 3
         GO TO 999
         END IF
C                                       check timerange limitations
C                                          possibly out of bounds!!!
      IF (CURTIM.LT.SBTIME) GO TO 200
C                                       Set integration time either
C                                        from the vis record, or
C                                        by using the global default.
      IF (ILOCIT.GE.0) THEN
         TAU = BUFF1(BINDI+ILOCIT)
      ELSE
         TAU = TINTG
         END IF
C
 210  CONTINUE
      IF (BEGSOL) THEN
C                                       determine gridded begin time
C                                       start from current timestamp

C                                       roll GBTIME back to beginning
C                                       of current SIUSE time interval
         GBTIME = CURTIM - MOD ( CURTIM - SBTIME , SIUSE )
C                                       determine gridded end time
         GETIME = GBTIME + SIUSE
C                                       init actual time
         ABTIME = CURTIM
         AETIME = CURTIM
C                                       Clear "Got data" flags
         NBLANK = MAN
         CALL LFILL (NBLANK, F, GOTANT)
C                                       initialize solution values
         NBLANK = 2*MAN
         CALL DFILL (NBLANK, 0.0D0, CMBDEL)
         CALL DFILL (NBLANK, 0.0D0, CDISP)
         NBLANK = 2 * MIF * MAN
         CALL DFILL (NBLANK, 0.0D0, CPHAZ)
         CALL DFILL (NBLANK, 0.0D0, CDELY)
         CALL DFILL (NBLANK, 0.0D0, CRATE)
         CALL DFILL (NBLANK, 0.0D0, CSGMA)
C                                       initialize all antennas as
C                                       self-referenced
         DO 1 Q = 1,MAN
            GOTSLN(Q) = Q
 1          CONTINUE
C                                       Blank data/bookkeeping arrays
         NBLANK = MBL
         CALL FILL (NBLANK, 0, NTIME)
         CALL FILL (NBLANK, 0, BLCODE)
         NBLANK = PBL
         CALL FILL (NBLANK, 0, BLINDX)
         NBLANK = MTM * MBL
         CALL RFILL (NBLANK, 0.0, WTB)
         CALL RFILL (NBLANK, 0.0, TIMB)
         CALL RFILL (NBLANK, 0.0, DTIMB)
         NBLANK = NBLANK * 2 * MFQ * 2
         CALL RFILL (NBLANK, 0.0, VVIS)
C                                       init # of baselines/times
C                                               found
         NBL = 0
         NT = 0
         MSUB = 0
         END IF
C                                       reset BEGSOL flag
      BEGSOL = CURTIM.GT.GETIME
C                                       allow NEWSCN flag to be reset
      NEWSCN = CURTIM.GE.SETIME
      ENDIT = .FALSE.
C                                       if new data pt is at beg of
C                                           sol, leave now!
      IF (BEGSOL) GO TO 300
C                                       -----PROCESS DATA POINT-----
      Q = (CURTIM - GBTIME) / CURSUB + 1.0
      MSUB = MAX (MSUB, Q)
C                                       update actual times [seconds]
      ABTIME = MIN (ABTIME, CURTIM - ( TAU/2.0 ))
      AETIME = MAX (AETIME, CURTIM + ( TAU/2.0 ))
C                                       Determine, decompose baseline
C                                       code
      IF (ILOCB.GE.0) THEN
         JBL = BUFF1(BINDI+ILOCB) + 0.1
         I1 = JBL / 256
         I2 = MOD ( JBL , 256 )
      ELSE
         I1 = BUFF1(BINDI+ILOCA1) + 0.1
         I2 = BUFF1(BINDI+ILOCA2) + 0.1
         END IF
C                                       construct new baseline code
      KBL = I1 *(MAN+1) + I2
C                                       store baseline code, location
      IF (BLINDX(KBL).EQ.0) THEN
         NBL = NBL + 1
         BLCODE(NBL) = KBL
         BLINDX(KBL) = NBL
         END IF
C                                       get baseline location
      Q = BLINDX(KBL)
C                                       increment baseline time
      IF (DORATE) THEN
         NTIME(Q) = NTIME(Q) + 1
C                                       or do time averaging
      ELSE
         NTIME(Q) = 1
         END IF
      K = NTIME(Q)
      NT = MAX (NT, K )
C                                       too many times
      IF (NT.GT.MTM) THEN
C                                       this error condition must be
C                                           checked!
         DELT = (CURTIM-GBTIME) / 60.
         MSGTXT = 'KFIT More times found than expected.'
         CALL MSGWRT (6)
         MSGTXT = ' CPARM(1) should be turned down.'
         IERR = 5
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       already have data? -  error!
      IF ((TIMB(K,Q).GT.0) .AND. (DORATE)) THEN
         IERR = 1
         WRITE (MSGTXT, 1000) IERR, 'found two identical timestamps'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       save current time [secs]
      TIMB(K,Q) = CURTIM
      DTIMB(K,Q) = TAU
C                                       Baseline factors
      GOTANT(I1) = T
      GOTANT(I2) = T
      BASEL = BUFF1(BINDI+ILOCU) * BUFF1(BINDI+ILOCU) +
     *   BUFF1(BINDI+ILOCV) * BUFF1(BINDI+ILOCV)
      IF ((BASEL.LT.MN2BAS) .OR. (BASEL.GT.MX2BAS)) THEN
         BLFACT = ANTWT(I1) * ANTWT(I2) * WTPABL
      ELSE
         BLFACT = ANTWT(I1) * ANTWT(I2)
         END IF
C                                       Accumulate
      DO 270 EW = 0,NPOL-1
         IST = 1 + EW*(NUMEST-1)
         DO 262 JIF = 0,MIF-1
            IF ((JIF.GE.LBIF) .AND. (JIF.LE.LEIF)) THEN
               DO 261 JFRQ = 0, MCH-1
                  INDEX = BINDI + NRPARM
     *               + JFRQ * INCF
     *               + JIF * INCIF
     *               + EW * INCS
                  IF ((BUFF1(INDEX+2).GT.0.0)) THEN
                     J = JIF * MCH + JFRQ + 1
                     WT1 = BLFACT * BUFF1(INDEX+2)
                     AMP = BUFF1(INDEX)*BUFF1(INDEX) +
     *                  BUFF1(INDEX+1)*BUFF1(INDEX+1)
                     WT1 = WT1 * AMP
                     CALL REWAIT (LWT, WT1)
                     VVIS(1,K,J,Q,IST) = VVIS(1,K,J,Q,IST)
     *                  + WT1*BUFF1(INDEX)
                     VVIS(2,K,J,Q,IST) = VVIS(2,K,J,Q,IST)
     *                  + WT1*BUFF1(INDEX+1)
                     WTB(K,Q)          = WTB(K,Q) + WT1
                     END IF
 261              CONTINUE
               END IF
 262        CONTINUE
 270     CONTINUE
      GO TO 200
C                                       fall through to end of soln
C                                         interval
C                                       when CURTIM>GETIME or
C                                         on source change
C                                       End of soln interval TIME LOOP
 300  CONTINUE
      IF (PRTLEV.GE.20) PRINT *,'KFIT Data loaded start solving now'
      IERR = 0
C                                       Adjust time to center time
      TIMEC = (ABTIME + AETIME) / 2.0

      I = 0
      DO 302 Q = 1,NBL
         DO 301 K = 1,NTIME(Q)-1
            DT = DT + (TIMB(K+1,Q) - TIMB(K,Q))
            I = I + 1
 301        CONTINUE
 302     CONTINUE
C                                       Use them if we got them
      IF (I.GT.0) THEN
         DT = DT / I
      ELSE
C                                       Otherwise, use something more
C                                       primitive
C
         DT = (AETIME-ABTIME) / NT
C                                       what if data is sparse?
C                                       [burst mode?]
         END IF
      IF ((DT/TINTG).LT.0.67) THEN
         I = DT
         WRITE (MSGTXT, 1000) I, '/60 or lower required for CPARM(1)'
         CALL MSGWRT (6)
         IERR = 1
         GO TO 999
         END IF
C                                       encode fundamental delta-t
C                                       [in units of 1000 seconds]
      DT = DT / 1000.0

C                                       BEGIN DOING SOLUTION HERE
C                                       [force at least one point here]
      IF ((.NOT.DORATE) .AND. (NT.EQ.1)) NT = 2
      IF (NT.GT.1) THEN
         BSLOP = (ABTIME - TIMEC) / 1000.0
         ESLOP = (AETIME - TIMEC) / 1000.0
C
C                                       reference all times to TIMEC
C                                       divide durations in half
C                                       switch time units to radians/mHz
         DO 310 Q = 1,NBL
            DO 305 K = 1,NTIME(Q)
               TIMB(K,Q) = (TIMB(K,Q) - TIMEC) / 1000.0
               DTIMB(K,Q) = (DTIMB(K,Q) / 2.0) / 1000.0
 305        CONTINUE
 310     CONTINUE
C                                       normalize data & baseline wgts
         SUMWT = 0.0D0
         DO 340 Q = 1,NBL
            WTS(Q) = 0.0
            DO 335 IST = 1,NUMEST
               DO 330 J = 1, MFQ
                  DO 320 K = 1,NTIME(Q)
                     IF (WTB(K,Q).GT.0.0) THEN
                        AMP = SQRT (VVIS(1,K,J,Q,IST)**2 +
     *                     VVIS(2,K,J,Q,IST)**2  )
                        IF (AMP.GT.VMIN) THEN
                           WTS(Q) = WTS(Q) + WTB(K,Q)
                           VVIS(1,K,J,Q,IST) = VVIS(1,K,J,Q,IST) / AMP
                           VVIS(2,K,J,Q,IST) = VVIS(2,K,J,Q,IST) / AMP
C                                       make amplitude ==0 for
C                                       later ease of testing!
                        ELSE
                           VVIS(1,K,J,Q,IST) = 0.0
                           VVIS(2,K,J,Q,IST) = 0.0
                           END IF
                        END IF
                     IF ((.NOT.DORATE) .AND. (NTIME(Q).EQ.1)) THEN
                        VVIS(1,2,J,Q,IST) = VVIS(1,1,J,Q,IST)
                        VVIS(2,2,J,Q,IST) = VVIS(2,1,J,Q,IST)
                        END IF
 320                 CONTINUE
 330              CONTINUE
               SUMWT = SUMWT + WTS(Q)
 335           CONTINUE
            IF ((.NOT.DORATE) .AND. (NTIME(Q).EQ.1)) THEN
               NTIME(Q) = 2
               TIMB(2,Q) = TIMB(1,Q) + DTIMB(1,Q)/2.
               DTIMB(2,Q) = DTIMB(1,Q)
               END IF
 340        CONTINUE
C                                       not enough good data,
C                                       dont even try it!
         IF (SUMWT.LT.NBL*VMIN) GO TO 1002
C     try to normalize the weight of each baseline to unity
         SUMWT = SUMWT / (NBL*NUMEST)
         DO 350 Q = 1,NBL
            WTS(Q) = WTS(Q) / SUMWT
 350        CONTINUE
C

C
C                                       frac. of time FFT to search
C                                       [RWINDO is mHz, DT is ksec]
         IF (RWINDO.LT.VMIN) THEN
            RFRAC = 1.0
         ELSE
C                                       encode fundamental delta-t
C                                       [in units of 1000 seconds]
            RFRAC = RWINDO * DT
            END IF
         RFRAC = MIN ( RFRAC, 1.0)
C                                       turn off the search window?
C                                       turn off search window in rate
C                                         if five or less data points
C                                         in the rate direction.
         IF (NT.LE.2) RFRAC = 0.0
         IF (NF.LE.2) DFRAC = 0.0
C                                       compute min size of the prob.
         IF (RFRAC.GT.0.0) THEN
            MT = LOG(1.0*NT) / LOG(2.0)
            MT = 2 ** MT
            IF (MT.LT.NT) MT = 2*MT
         ELSE
            MT = NT
            END IF

         IF (DFRAC.GT.0.0) THEN
            MF = LOG(1.0*NF) / LOG(2.0)
            MF = 2 ** MF
            IF (MF.LT.NF) MF = 2*MF
         ELSE
            MF = NF
            END IF
C                                       RFRAC, DFRAC are non-negative
C                                       MT and MF are powers of 2!
C                                       preserve the min size problem
         MT = MT * 2
         MF = MF * 2
         MTMIN = MT
         MFMIN = MF
C                                       set max size of the problem
         LIMFFT = 8192000
         LIMTOT = KAPWRD * 1024
C                                       start at upper part of  scale
C                                       for each dimension!
         MT = MT * FFTFAX
         MF = MF * FFTFAX
         MTD = 2
         MFD = 1
 343     CONTINUE
C                                       compute search size
         NR = 1
         ND = 1
         IF (DODRFT) THEN
            NR = MT * RFRAC
            ND = MF * DFRAC
            NR = MAX (NR, 1)
            ND = MAX (ND, 1)
            END IF
C                                       compute needed storage memory
         NEEDT = NT
         NEEDF = NFQ
         IF ((NR.GT.NT).AND.(ND.GT.NFQ).AND.(NR.LE.ND)) NEEDT = NR
         IF ((NR.GT.NT).AND.(ND.GT.NFQ).AND.(NR.GT.ND)) NEEDF = ND
C         WRITE (*,*) 'Find AN AP of ',APSIZE,NR,ND,MT,MF,NEEDFT,FFTFAX
C                                       compute needed FFT memory
         NEEDFT = MAX (MT, MF)
C                                       check problem size
         TOOBIG = (NEEDFT.GT.LIMFFT).OR.
     *            ((NEEDT*NEEDF+3*NEEDFT).GT.LIMTOT)
         IF (TOOBIG) THEN
C                                       reduce problem and swap divisors
            MT = MT / MTD
            MF = MF / MFD
            I = MTD
            MTD = MFD
            MFD = I
            GO TO 343
            END IF
C                                       oh no! problem doesnt fit
C                                                in memory!
         IF ((MT.LT.MTMIN).OR.(MF.LT.MFMIN)) THEN
            KDAY1 = TIMEC/DY2SEC
            TIMEX = (TIMEC/DY2SEC - KDAY1) * 24.
            KHR1 = TIMEX
            TIMEX = (TIMEX - KHR1) * 60.
            KMN1 = TIMEX
            TIMEX = (TIMEX - KMN1) * 60.
            KSEC1 = IROUND (TIMEX)
            WRITE (MSGTXT,2001) KDAY1, KHR1, KMN1, KSEC1
            CALL MSGWRT (6)
            IF (MT.LT.MTMIN) WRITE (MSGTXT, 2002)
            CALL MSGWRT (6)
            IF (MF.LT.MFMIN) WRITE (MSGTXT, 2003)
            CALL MSGWRT (6)
            IERR = 2
            GO TO 999
            END IF
C                                       check the amount of memory
         IF ((SIZET.LT.NEEDT).OR.(SIZEF.LT.NEEDF).OR.
     *                           (SIZEFT.LT.NEEDFT)  ) THEN
            CALL REMEM (SIZET, NEEDT, SIZEF, NEEDF, SIZEFT, NEEDFT,
     *           BPCORE, OBPCOR, BPSCOR, OBPSCO,
     *           WRK1, OWRK1, WRK2, OWRK2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 1000) IERR,
     *              'unable to allocate sufficient memory'
               CALL MSGWRT (6)
               IERR = 3
               GO TO 999
               END IF
            END IF
C
         IF (PRTLEV.GE.20) WRITE (*,*) 'Start pol/IF loop ',NUMEST,NOEIF
C                                       some consistency checks
         IF ((NT.GT.MTM) .OR.
     *      (NF.GT.SIZEFT) .OR.
     *      (NR.GT.MT) .OR.
     *      (ND.GT.MF)) THEN
            IERR = 100
            WRITE (MSGTXT, 1000) IERR,
     *        'time/freq calculation corrupted - this is bad.'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       think positive
         SNGOOD = .TRUE.
         DO 1001 IST = 1,NUMEST
C                                       Loop over effective IFs
            DO 360 IIF = 1,NOEIF
               IFP = (IIF-1) * NFQ
               KIF = (IIF-1) * NOAIF + 1
               IF (PRTLEV.GE.1) THEN
                  WRITE (MSGTXT, 2100)
                  IF (PRTLEV.NE.10) CALL MSGWRT (2)
                  I     = INT (ABTIME + 0.5)
                  KDAY1 = INT (I/ 86400)
                  I     = MOD (I, 86400)
                  KHR1  = INT (I/ 3600)
                  I     = MOD (I, 3600)
                  KMN1  = INT (I/ 60)
                  KSEC1 = MOD (I, 60)
C
                  I     = INT (AETIME + 0.5)
                  KDAY2 = INT (I/ 86400)
                  I     = MOD (I, 86400)
                  KHR2  = INT (I/ 3600)
                  I     = MOD (I, 3600)
                  KMN2  = INT (I/ 60)
                  KSEC2 = MOD (I, 60)
C
                  WRITE (MSGTXT,2010) IIF, STORD(IST),
     *                 KDAY1, KHR1, KMN1, KSEC1,
     *                 KDAY2, KHR2, KMN2, KSEC2, SIUSE
                  IF (PRTLEV.NE.10) CALL MSGWRT (2)
                  END IF
               IF (PRTLEV.GE.2) THEN
C                                       print FFT spacing message
                  SPCNS = 0.0
                  IF (DF*MF.GT.0.0) SPCNS = 1.0 / (DF * MF)
                  SPCMH = 0.0
                  IF (DT*MT.GT.0.0) SPCMH = 1.0 / (DT * MT)
                  WRITE (MSGTXT, 2020) NR/2, SPCMH, ND/2, SPCNS
                  IF (PRTLEV.NE.10) CALL MSGWRT (2)
                  IF (.NOT.DORATE) THEN
                     COLTYT(3) = ' '
                     COLUNI(3) = ' '
                     END IF
                  IF (.NOT.DODLAY) THEN
                     COLTYT(4) = ' '
                     COLUNI(4) = ' '
                     END IF
                  IF (.NOT.DODLBY) THEN
                     COLTYT(5) = ' '
                     COLUNI(5) = ' '
                     END IF
                  WRITE (MSGTXT, 2030) (COLTYT(I), I=1,6)
                  MSGTXT(2:2) = 'B'
                  IF (PRTLEV.NE.10) CALL MSGWRT (2)
                  WRITE (MSGTXT, 2030) (COLUNI(I), I=1,6)
                  MSGTXT(2:2) = '['
                  IF (PRTLEV.NE.10) CALL MSGWRT (2)
                  WRITE (MSGTXT, 2110)
                  IF (PRTLEV.NE.10) CALL MSGWRT (2)
                  END IF
C                                     prepare SWT
               CALL PRPWTS (MAN, MTM, MFQ, NBL, MBL, WTS, NFQ, IFP,
     *                 IST, SWT, BLCODE, NTIME, VVIS)
C                                     set antennas not to be solved = 0
C                                     set antennas to be solved for = -1
               DO 365 Q = 1,MAN
                  IF ((SWT(Q).GT.0.0).AND.(GSOLVE(Q).LT.0)) THEN
                     GOTSLN(Q) = 0
                  ELSE
                     GOTSLN(Q) = -1
                     END IF
 365              CONTINUE
               IF (TRYHRD.GT.0.0) THEN
C                                     look for previous good solutions
                  CALL SNGET (TIMEC, MAN, MIF, FREQIF,
     *                 SNKOLS, SNNUMV, KIF, IST, GOTSLN,
     *                 CMBDEL(1,IST), CDISP(1,IST), CPHAZ(1,KIF,IST),
     *                 CDELY(1,KIF,IST), CRATE(1,KIF,IST),
     *                 CSGMA(1,KIF,IST),
     *                 FRREAL, FRIMAG, FRDELY, FRRATE, FRWGHT, IERR)
                  IF (IERR.NE.0) THEN
                     MSGTXT = 'KFIT Error extrapolating solutions'
                     IERR = 2
                     CALL MSGWRT (6)
                     GO TO 999
                     END IF
C                                     convert them from standard form
                  CALL CNVTD (SOLMOD, -1, MAN, NOAIF, FREQ, FREQIF(KIF),
     *               SPHAZ, SRATE, SMBDEL, SDELY, SSGMA,
     *               CPHAZ(1,KIF,IST), CRATE(1,KIF,IST),
     *               CDELY(1,KIF,IST), CMBDEL(1,IST), CDISP(1,IST),
     *               CSGMA(1,KIF,IST))
               ELSE
                  CALL DFILL (MAN, 0.0D0, SPHAZ)
                  CALL DFILL (MAN, 0.0D0, SDELY)
                  CALL DFILL (MAN, 0.0D0, SMBDEL)
                  CALL DFILL (MAN, 0.0D0, SRATE)
                  CALL DFILL (MAN, 0.0D0, SSGMA)
                  END IF
C                                     init rest as self-referenced
C                                     [w/ zero errors]
               DO 367 Q = 1, MAN
                  IF (GOTSLN(Q).LT.0) THEN
                     GOTSLN(Q) = Q
                     SSGMA(Q) = 0.0
                     END IF
 367              CONTINUE
C                                     init reference antenna
               REFAN(IST,KIF) = REFANT
               CALL KFFT (BLINDX, VVIS(1,1,1,1,IST), NR, ND, NTIME, NT,
     *            MT, MTM, PBL, MBL, DF, MF, MFQ, NFQ, IFP,
     *            IFRQ(IFP+1), TIMB, DTIMB, MAN, SWT, WTS, SNRFFT,
     *            TRYHRD, PRTLEV, BLDO, SRCHL, NSRCH, PRIRTY, SIZET,
     *            SIZEF, SIZEFT, BPCORE(1+OBPCOR), BPSCOR(1+OBPSCO),
     *            WRK1(1+OWRK1), WRK2(1+OWRK2), GOTSLN, SPHAZ, SMBDEL,
     *            SRATE, SSGMA,
C    *            CPHAZ(1,IIF,IST), CMBDEL(1,IST),
C    *            CRATE(1,IIF,IST), CSGMA(1,IIF,IST),
     *            BSLOP, ESLOP)
C                                     get REFANT
               CALL KRNREF (REFAN(IST,KIF), MAN, GOTSLN, PRIRTY,
C     *           GDSGMA, CSGMA(1,IIF,IST), CPHAZ(1,IIF,IST),
C     *           CRATE(1,IIF,IST), CMBDEL(1,IST), PRTLEV)
     *            GDSGMA, SSGMA, SPHAZ, SRATE, SMBDEL, PRTLEV)
C                                     refine using LS
               IF (DODRLS) THEN
                  CALL KLSF (VVIS(1,1,1,1,IST), DORATE, DODLAY, DODLBY,
     *               BLCODE, MBL, MAN, NBL, NTIME, MTM, MFQ, NFQ, IFP,
     *               SWT, WTS, NSRLS, GDSGMA,
C     *               CPHAZ(1,IIF,IST), CRATE(1,IIF,IST), TIMB,
C     *               CMBDEL(1,IST), FREQZ, CDELY(1,IIF,IST), FREQY,
C     *               CSGMA(1,IIF,IST), REFAN(IST,IIF),
     *               SPHAZ, SRATE, TIMB, SMBDEL, FREQZ, SDELY, FREQY,
     *               SSGMA, REFAN(IST,KIF), NT, DF, DT, PRTLEV, TIMEC,
     *               FREQ, IERR)
                  END IF
C                                       convert to std form [extend but
C                                       do not enforce min SNR]
               CALL CNVTD (SOLMOD, 1, MAN, NOAIF, FREQ, FREQIF(KIF),
     *            SPHAZ, SRATE, SMBDEL, SDELY, SSGMA,
     *            CPHAZ(1,KIF,IST), CRATE(1,KIF,IST),
     *            CDELY(1,KIF,IST), CMBDEL(1,IST), CDISP(1,IST),
     *            CSGMA(1,KIF,IST))
C                                       end loop over IFs
 360           CONTINUE
C                                       Enforce minimum SNR requirement
C                                       [blank MB-delay if even one
C                                       missing IF is found]
C            DO 470 I = 1, NOAIF
            DO 470 I = 1,MIF
               KIF = ((I-1)/NOAIF) + 1
               DO 465 J = 1,MAN
                  IF ((CSGMA(J,I,IST).GT.0.0).AND.
     *               ((CSGMA(J,I,IST).LE.NSRLS) .OR.
     *               (NSRLS.LT.0.))) THEN
                  ELSE
                     CPHAZ(J,I,IST) = FBLANK
                     CDELY(J,I,IST) = FBLANK
                     CRATE(J,I,IST) = FBLANK
                     CMBDEL(J,IST)  = FBLANK
                     CDISP(J,IST)   = FBLANK
                     CSGMA(J,I,IST) = -1.0D0
                     ALGOOD = .FALSE.
                     SNGOOD = .FALSE.
                     END IF
 465              CONTINUE
               REFAN(IST,I) = REFAN(IST,KIF)
 470           CONTINUE
C
            IF (PRTLEV.GE.1) THEN
               WRITE (MSGTXT, 2100)
               IF (PRTLEV.NE.10) CALL MSGWRT (2)
               END IF
C                                       end STOKES loop here
 1001          CONTINUE
C                                       escape here if not enough
C                                       data to form a solution
 1002       CONTINUE
C                                       copy soln from last eff stokes
C                                       to second stokes
         DO 485 J = 1, MAN
            DO 475 I = 1,MIF
               CMBDEL(J,2)  = CMBDEL(J,NUMEST)
               CDISP(J,2)   = CDISP(J,NUMEST)
               CPHAZ(J,I,2) = CPHAZ(J,I,NUMEST)
               CDELY(J,I,2) = CDELY(J,I,NUMEST)
               CRATE(J,I,2) = CRATE(J,I,NUMEST)
               CSGMA(J,I,2) = CSGMA(J,I,NUMEST)
               REFAN(2,I) = REFAN(NUMEST,I)
 475           CONTINUE
 485        CONTINUE
C                                       Write solution record.
         DELT = MAX ( AETIME-ABTIME , 0.0 ) / DY2SEC
         IFRM = 0.0
C                                       first time around, get initial
C                                       record number to write to
         IF (SNROW.EQ.0) THEN
            CALL SNPUT (TIMEC, DELT, SCNSOU, NODENO, SCNSUB, MAN, MIF,
     *         FREQIF, GOTANT, SNROW, SNKOLS, SNNUMV, CNTOK, CNTBAD,
     *         CMBDEL, CDISP, CPHAZ, CDELY, CRATE, CSGMA, REFAN, IERR,
     *         FRREAL, FRIMAG, FRDELY, FRRATE, FRWGHT)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT, 1000) IERR, 'on first write to SN table'
               CALL MSGWRT (2)
               IERR = 2
               GO TO 999
               END IF
            END IF
C                                       write solution to SN table
         CALL SNPUT (TIMEC, DELT, SCNSOU, NODENO, SCNSUB, MAN, MIF,
     *      FREQIF, GOTANT, SNROW, SNKOLS, SNNUMV, CNTOK, CNTBAD,
     *      CMBDEL, CDISP, CPHAZ, CDELY, CRATE, CSGMA, REFAN, IERR,
     *      FRREAL, FRIMAG, FRDELY, FRRATE, FRWGHT)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1000) IERR, 'writing to SN table'
            CALL MSGWRT (2)
            IERR = 2
            GO TO 999
            END IF
C                                       END DOING SOLUTION HERE
         END IF
C                                       shift subintervals
      IF (MSUB.GT.1) THEN
         NBLANK = MAN
         CALL LFILL (NBLANK, F, GOTANT)
C                                       initialize all antennas as
C                                       self-referenced
         DO 501 Q = 1,MAN
            GOTSLN(Q) = Q
 501        CONTINUE
C                                       initialize solution values
         NBLANK = 2*MAN
         CALL DFILL (NBLANK, 0.0D0, CMBDEL)
         CALL DFILL (NBLANK, 0.0D0, CDISP)
         NBLANK = 2 * MIF * MAN
         CALL DFILL (NBLANK, 0.0D0, CPHAZ)
         CALL DFILL (NBLANK, 0.0D0, CDELY)
         CALL DFILL (NBLANK, 0.0D0, CRATE)
         CALL DFILL (NBLANK, 0.0D0, CSGMA)
C                                       shift time interval
         GBTIME = GBTIME + CURSUB
         GETIME = GETIME + CURSUB
         GETIME = MIN (GETIME, SETIME)
         NT = 0
         DO 550 Q = 1,NBL
            NQ = 0
            DO 510 KBL = 1,NBL
               IF (BLINDX(KBL).EQ.Q) THEN
                  I1 = KBL / (MAN+1)
                  I2 = KBL - I1 * (MAN+1)
                  END IF
 510           CONTINUE
            DO 540 K = 1,NTIME(Q)
               TIMB(K,Q) = 1000.0 * TIMB(K,Q) + TIMEC
               DTIMB(K,Q) = DTIMB(K,Q) * 2.0 * 1000.0
               IF (TIMB(K,Q).GE.GBTIME) THEN
                  NQ = NQ + 1
                  TIMB(NQ,Q) = TIMB(K,Q)
                  DTIMB(NQ,Q) = DTIMB(K,Q)
                  WTB(NQ,Q) = WTB(K,Q)
                  TIMB(K,Q) = 0.0
                  DTIMB(K,Q) = 0.0
                  WTB(K,Q) = 0.0
                  GOTANT(I1) = .TRUE.
                  GOTANT(I2) = .TRUE.
                  DO 520 IST = 1,NUMEST
                     DO 515 J = 1,MFQ
                        VVIS(1,NQ,J,Q,IST) = VVIS(1,K,J,Q,IST)
                        VVIS(2,NQ,J,Q,IST) = VVIS(2,K,J,Q,IST)
                        VVIS(1,K,J,Q,IST) = 0.0
                        VVIS(2,K,J,Q,IST) = 0.0
 515                    CONTINUE
 520                 CONTINUE
               ELSE
                  TIMB(K,Q) = 0.0
                  DTIMB(K,Q) = 0.0
                  WTB(K,Q) = 0.0
                  DO 530 IST = 1,NUMEST
                     DO 525 J = 1,MFQ
                        VVIS(1,K,J,Q,IST) = 0.0
                        VVIS(2,K,J,Q,IST) = 0.0
 525                    CONTINUE
 530                 CONTINUE
                  END IF
 540           CONTINUE
            NTIME(Q) = NQ
            NT = MAX (NT, NQ)
 550        CONTINUE
         END IF
      MSUB = MSUB - 1
C                                       At absolute end
      IF (ENDIT) THEN
         IF (MSUB.GE.SOLMIN) GO TO 300
C                                       at end of scan
      ELSE IF (NEWSCN) THEN
         IF (MSUB.GE.SOLMIN) GO TO 300
         GO TO 100
C                                       Still in scan
      ELSE
         IF (MSUB.LE.0) GO TO 210
         IF (NT.LE.0) GO TO 210
         IF (CURTIM.GT.GETIME) THEN
            IF (MSUB.GE.SOLMIN) GO TO 300
            GO TO 210
            END IF
         ABTIME = CURTIM
         AETIME = CURTIM
         BEGSOL = .FALSE.
         GO TO 210
         END IF
C                                       DONE WITH ALL SOLUTIONS
 800  CONTINUE
      IF (PRTLEV.GE.20) WRITE (*,*) 'KFIT end soln loop now'
C                                       Make sure the UV file is closed!
      CALL KGTDAT ('CLOS', SOLMOD, FOPEN, BINDI, FREQIF, IFRQ,
     *     FREQY, FREQZ, FREQS, MFQ, DF, NF, MIF, MCH, DFRAC, VSTART,
     *     VEND, IERR)
C                                       Give body count
      IF (CNTOK.LE.0) THEN
C                                       Nothing worked
         IERR = 108
         WRITE (MSGTXT, 1000) IERR, 'no valid solutions found'
         CALL MSGWRT (6)
         IERR = 8
      ELSE
         WRITE (MSGTXT,1440) CNTOK, CNTBAD
         CALL MSGWRT (5)
         END IF

 999  CONTINUE
C                                       dont forget to free memory!!
      NEEDT = 0
      CALL REMEM (SIZET, NEEDT, SIZEF, NEEDF, SIZEFT, NEEDFT,
     *   BPCORE, OBPCOR, BPSCOR, OBPSCO, WRK1, OWRK1, WRK2, OWRK2,
     *     IERR)
      RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Writing SN table ',I4)
 2001 FORMAT ('!!Insufficient memory for FFT search at Time=',
     *          I3,'/',3I2.2)
 2002 FORMAT ('  Try: using a shorter solution interval!!')
 2003 FORMAT ('       or averaging up in frequency     !!')
 1000 FORMAT ('KFIT Error ',I3,1X,A)
 2010 FORMAT ('IF/Pol = ',I2.2,'/',A2,
     *     '    ',I2.2,'/',I2.2,':',I2.2,':',I2.2,
     *     ' to ',I2.2,'/',I2.2,':',I2.2,':',I2.2,
     *     '   ','(',F6.1,' sec)')
 2020 FORMAT (' [ search +/-',I5,' x ',F6.2,' mHz and ',
     *                  '+/-',I5,' x ',F6.2,' ns ]')
 2030 FORMAT (2X,A8, A8, A8, 4X,A8, 4X,A8, 1X,A8,2X)
 2100 FORMAT (61('='))
 2110 FORMAT (61('-'))
 1440 FORMAT ('Found ', I8, ' good and ',I8,' bad solutions')
      END
      SUBROUTINE PRPWTS (MAN, MTM, MFQ, NBL, MBL, WTB, NFQ, IFP,
     *     IST, SWT, BLCODE, NTIME, VVIS)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER MAN, MTM, MFQ, NBL, MBL, NFQ, IFP
      INTEGER BLCODE(MBL), NTIME(MBL), I1, I2, IST
      REAL SWT(MAN), WTB(MBL), VVIS(2,MTM,MFQ,MBL,2)
C
      INTEGER Q, J, K
C                                     prep antenna-based weights
      CALL RFILL (MAN, 0.0, SWT)
      DO 364 Q = 1,NBL
C                                     check baseline weight
         IF (WTB(Q).LE.0.0) GO TO 364
C                                     decompose baseline code
         I1 = INT ( BLCODE(Q) / (MAN+1) )
         I2 = MOD ( BLCODE(Q) , (MAN+1) )
         DO 363 J = IFP+1,IFP+NFQ
            DO 362 K = 1,NTIME(Q)
C                                     check amplitude
               IF ((ABS(VVIS(1,K,J,Q,IST)) +
     *              ABS(VVIS(2,K,J,Q,IST))   )
     *              .GT.0.0) THEN
                  SWT(I1) = SWT(I1) + WTB(Q)
                  SWT(I2) = SWT(I2) + WTB(Q)
               END IF
 362        CONTINUE
 363     CONTINUE
 364  CONTINUE
C
      RETURN
      END
      SUBROUTINE CNVTD (SOLMOD, DIR, MAN, NOAIF, FREQ, FREQIF, SPHASE,
     *   SRATE, SDELAY, SDISPS, SSIGMA, PHASE, RATE, DELAY, MBDELY,
     *   DISP, SIGMA)
C-----------------------------------------------------------------------
C   In/out (in on DIR=1)
C      SPHASE   D(*)   Phase in turns
C      SRATE    D(*)
C      SDELAY   D(*)   MB delay (NOAIF > 1) or SB delay
C      SDISPS   D(*)   -Dispersion * lambda * lambda (at FREQ) note sign
C      SSIGMA   D(*)
C   In/out (out on DIR=1)
C      PHASE    D(*)   phase in turns
C      RATE     D(*)
C      MBDELY   D(*)   Multi-band delay
C      DELAY    D(*)   Single-band delay
C      DISP     D(*)   Dispersion in sec/m/m
C      SIGMA    D(*)
C-----------------------------------------------------------------------
      CHARACTER SOLMOD*4
      INTEGER   DIR, MAN, NOAIF
      DOUBLE PRECISION FREQ, FREQIF(NOAIF), SPHASE(MAN), SRATE(MAN),
     *   SDELAY(MAN), SDISPS(MAN), SSIGMA(MAN), PHASE(MAN,NOAIF),
     *   RATE(MAN,NOAIF), MBDELY(MAN), DISP(MAN), DELAY(MAN,NOAIF),
     *   SIGMA(MAN,NOAIF)
      INCLUDE 'KRING.CONSTS'
      INTEGER   I, J
      DOUBLE PRECISION IODLAY, IORATE, IODLBY, IOPHAS, IOSIGM
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      DO 700 I = 1,MAN
         IF (DIR.EQ.1) THEN
            IOPHAS = SPHASE(I)
            IORATE = SRATE(I)
            IODLAY = SDELAY(I)
            IODLBY = SDISPS(I)
            IOSIGM = SSIGMA(I)
            IF (SOLMOD(4:4).EQ.' ') THEN
               DO 200 J = 1, NOAIF
                  RATE(I,J)  = IORATE
                  SIGMA(I,J) = IOSIGM
                  PHASE(I,J) = IOPHAS + IODLAY * (FREQIF(J)-FREQIF(1))
     *               * NS2SEC
C     WRITE (*,*) J,IOPHAS, IODLAY, FREQIF(J), PHASE(I,J)
                  DELAY(I,J) = IODLAY
 200              CONTINUE
            ELSE IF (SOLMOD(4:4).EQ.'S') THEN
               DO 210 J = 1, NOAIF
                  RATE(I,J)  = IORATE
                  SIGMA(I,J) = IOSIGM
                  PHASE(I,J) = IOPHAS + IODLAY * (FREQIF(J)-FREQIF(1))
     *               * NS2SEC
                  DELAY(I,J) = IODLAY + IODLBY
 210              CONTINUE
            ELSE IF (SOLMOD(4:4).EQ.'I') THEN
               DO 220 J = 1, NOAIF
                  RATE(I,J)  = IORATE
                  SIGMA(I,J) = IOSIGM
                  PHASE(I,J) = IOPHAS + IODLAY * (FREQIF(J)-FREQIF(1))
     *               * NS2SEC
C    *               - IODLBY * FREQ**2 * NS2SEC / (FREQ+FREQIF(J))
                  DELAY(I,J) = IODLAY
C    *               + IODLBY * ( FREQ / ( FREQ+FREQIF(J) ) )**2
 220              CONTINUE
               MBDELY(I) = IODLAY
               DISP(I) = -IODLBY * (FREQ / VELITE)**2
            ELSE IF (SOLMOD(4:4).EQ.'T') THEN
               DO 230 J = 1, NOAIF
                  RATE(I,J)  = IORATE
                  SIGMA(I,J) = IOSIGM
                  PHASE(I,J) = IOPHAS + IODLAY * (FREQIF(J)-FREQIF(1))
     *               * NS2SEC
C    *               - IODLBY * FREQ**2 * NS2SEC / (FREQ+FREQIF(J))
                  DELAY(I,J) = IODLAY
C  *                 + IODLBY * ( FREQ / (FREQ+FREQIF(J) ) )**2
 230              CONTINUE
               MBDELY(I) = IODLAY
               DISP(I) = -IODLBY * (FREQ / VELITE)**2
               END IF
C                                     solve only for single band delays
            IF (SOLMOD(1:1).NE.' ') MBDELY(I) = 0.0D0
C                                     DIR = -1
         ELSE
            SPHASE(I) = PHASE(I,1)
            SRATE(I) = RATE(I,1)
            SDELAY(I) = DELAY(I,1)
            SDISPS(I) = -DISP(I) * (VELITE / FREQ)**2
            SSIGMA(I) = SIGMA(I,1)
            END IF
 700     CONTINUE
      RETURN
      END
      SUBROUTINE GETNXR (DISKIN, CNOIN, INCAT, ROW, MROW,
     *   SBTIME, SDTIME, SCNSOU, SCNSUB, VSTART, VEND, FREQID, IERR)
C-----------------------------------------------------------------------
      INTEGER DISKIN, CNOIN, INCAT(256), ROW, MROW,
     *   SCNSOU, SCNSUB, VSTART, VEND, FREQID, IERR, LUNNX
      REAL SBTIME, SDTIME
      DOUBLE PRECISION TIMEC
      INTEGER NXVER, NXROW
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      LUNNX = 26
      NXVER = 1
      CALL NDXINI ('READ', NXBUFF, DISKIN, CNOIN, NXVER, INCAT, LUNNX,
     *   NXROW, NXKOLS, NXNUMV, IERR)
      MSGSUP = 0
      IF (IERR.NE.0) THEN
         MSGTXT = 'GETNXR Problem reading NX table.'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       first time around,
C                                       snag the max row no. and leave
      IF (ROW.EQ.0) THEN
         MROW = NXBUFF(5)
         GO TO 990
         END IF

      NXROW = ROW
      MSGSUP = 32000
      CALL TABNDX ('READ', NXBUFF, NXROW, NXKOLS, NXNUMV, SBTIME,
     *   SDTIME, SCNSOU, SCNSUB, VSTART, VEND, FREQID, IERR)
      MSGSUP = 0
      IF (IERR.NE.0) THEN
         MSGTXT = 'TABNDX problem reading NX table.'
         CALL MSGWRT (6)
         GO TO 999
         END IF

 990     CONTINUE
      CALL TABIO ('CLOS', 0, NXROW, TIMEC, NXBUFF, IERR)
      MSGSUP = 0
 999  CONTINUE
      RETURN
      END
      SUBROUTINE KGTDAT (OP, SOLMOD, FOPEN, BINDI, FREQIF, IFRQ, FREQY,
     *   FREQZ, FREQS, MFQ, DF, NF, MIF, MCH, DFRAC, VSTART, VEND, IRET)
C-----------------------------------------------------------------------
C   KGTDAT will read a multi source data set into a temporary scratch
C   file.  Editing and calibration may be applied.
C   Inputs via common /SELCAL/  (Include DSEL.INC)
C      UNAME        C    AIPS name of input file.
C      UCLAS        C    AIPS class of input file.
C      UDISK        R    AIPS disk of input file.
C      USEQ         R    AIPS sequence of input file.
C      SOURCS(30)   C    Names (16 char) of up to 30 sources, *=>all
C                        First char of name '-' => all except those
C                        specified.
C      TIMRNG(8)    R    Start day, hour, min, sec, end day, hour,
C                        min,sec. 0's => all
C      UVRNG(2)     R    Minimum and maximum baseline lengths in
C                        1000's wavelengths. 0's => all
C      STOKES       C    Stokes types wanted.
C                        'I','Q','U','V','R','L','IQU','IQUV'
C      BCHAN        I    First chan no. selected, 1 rel. to first
C                        channel in data base. 0 => all
C      ECHAN        I    Last channel selected. 0=>all
C      BIF          I    First IF number selected, 1 rel. to first
C                        IF in data base. 0 => all
C      EIF          I    Last IF selected. 0=>all
C      DOCAL        L    If true apply calibration, else not.
C      ANTENS(50)   I    List of antennas selected, 0=>all,
C                        any negative => all except those specified
C      FGVER        I    FLAG file version number, if .le. 0 then
C                        NO flagging is applied.
C      CLUSE        I    Cal file version number to apply.
C   Output:
C      IRET         I    Error code: 0 => OK,
C                         1 => bad op
C                         2 => error on UVGET('INIT' or 'CLOS') or
C                                ZPHFIL
C                         3 => error on CALCOP, CHNDAT, or KDVMOD
C                         4 => error on UVINIT
C                         5 => end of file - UVDISK
C                         6 => no data - UVDISK
C                         7 => error on UVDISK
C                         8 => unexpected change in freq. structure
C                         9 => bad frequency indexing calculation
C-----------------------------------------------------------------------
      CHARACTER OP*4, SOLMOD*4
      LOGICAL   FOPEN
      INTEGER   MFQ, NF, MIF, MCH, BINDI, IFRQ(MFQ), VSTART, VEND, IRET
     *
      DOUBLE PRECISION FREQIF(*), FREQY(MFQ), FREQS(MFQ), FREQZ(MFQ)
      REAL      DFRAC
      INCLUDE 'KRING.CONSTS'
      INCLUDE 'KRING.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      REAL      DUM, DJUNK, REFPIX, REQ
      DOUBLE PRECISION DF1, DF2, DFMAX, DF
      INTEGER   NIF, I, J, K, L, DISK, VCNO, IIVER, OOVER, LUNUVI,
     *   LUNTB1, LUNTB2, FINDI, VO, BO, NIN, VISDSK, VISCNO, IEQ
      CHARACTER IFILE*48, BNDCOD(MAXIF)*8
      LOGICAL   T, F
      EQUIVALENCE (IEQ, REQ)
      SAVE VISDSK, VISCNO, LUNUVI, FINDI
      DATA T, F /.TRUE.,.FALSE./
      DATA VISDSK, VISCNO /0, 0/
C-----------------------------------------------------------------------
      LUNUVI = 16
      LUNTB1 = 28
      LUNTB2 = 29
      IRET = 0
      IF (OP(1:4).EQ.'INIT') GO TO 10
      IF (OP(1:4).EQ.'READ') GO TO 500
      IF (OP(1:4).EQ.'CLOS') GO TO 600
      IRET = 1
      GO TO 999
 10   CONTINUE
C                                       Setup
C                                       fill timerange selection here
      INITVS = VSTART
C                                       restore CATBLK from CATUV!
      IF (FOPEN) THEN
         CALL ZCLOSE (LUNUVI, FINDI, IRET)
         FOPEN = .FALSE.
         END IF
      CALL UVGET ('INIT', DUM, DUM, IRET)
      IF (IRET.NE.0) THEN
         CALL UVGET ('CLOS', DUM, DUM, IRET)
         MSGTXT = 'KGDAT Problem reading from the input data'
         IRET = 2
         GO TO 999
         END IF
C                                       leave NVIS alone if VEND = 0!!
      IF (VEND.GT.0) NVIS = VEND - VSTART + 1
C                                       Message
      IF (PRTLEV.GE.1) THEN
         MSGTXT = 'Get data'
         IF (DOCAL)  MSGTXT = 'Get data with calibration'
         IF (DOFLAG) MSGTXT = 'Get data with flagging'
         IF (DOCAL.AND.DOFLAG)
     *        MSGTXT = 'Get data with calibration and editing'
         IF (PRTLEV.NE.10) CALL MSGWRT (2)
         END IF
C                                       Copy
      MSGSUP = 32000
      CALL CALCOP (VISDSK, VISCNO, BUFF1, JBUFSZ, IRET)
      MSGSUP = 0
      IF (IRET.NE.0) THEN
         MSGTXT = 'Problem copying UV data/tables'
         CALL MSGWRT (6)
         IRET = 3
         GO TO 999
         END IF
      DISK = SCRVOL(VISCNO)
      VCNO = SCRCNO(VISCNO)
C                                       Copy relevant portion of IF
C                                       table. Read all IFs from old
C                                       CH/FQ table
      IIVER = 1
      CALL CHNDAT ('READ', BUFF1, DISKIN, CNOIN, IIVER, CATUV, LUNTB1,
     *   NIF, FREQIF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,2040) IRET
         CALL MSGWRT(6)
         IRET = 3
         GO TO 999
         END IF
      IF (NIF.NE.MIF) THEN
         MSGTXT =  'This is not happening, NIF <> MIF .'
         CALL MSGWRT
         IRET = 8
         GO TO 999
         END IF
C                                       Correct for ref. freq.
C                                       change in UVGET
      DO 50 I = 1, MIF
         DFINC(I) = DBLE(FINC(I))
         FREQIF(I) = FREQIF(I) - FREQ + UVFREQ
 50      CONTINUE
C                                       Write new FQ table [needed
C                                       [via KDVMOD-UVMDIV-CHNCOP]
      MSGSUP = 32000
      CALL CHNDAT ('WRIT', BUFF1, DISK, VCNO, OOVER, CATBLK, LUNTB2,
     *   NIF, FREQIF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      MSGSUP = 0
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,2050) IRET
         IRET = 3
         GO TO 990
         END IF
      IF ((NSOUWD.EQ.1).AND.
     *        ((SMODEL(1).GT.1.0E-20).OR.
     *         (NAME2.NE.'            ').OR.
     *         (CLAS2.NE.'      '))) THEN
         CALL KDVMOD (VISDSK, VISCNO, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = '  KDVMOD reports error preparing divided data'
            CALL MSGWRT (6)
            IRET = 3
            GO TO 990
            END IF
         END IF
C                                       arrange FREQIF(1) to be zero
      FREQ = FREQ + FREQIF(1)
      DO 100 I=2,MIF
         FREQIF(I) = FREQIF(I) - FREQIF(1)
 100     CONTINUE
      FREQIF(1) = 0.0D0
C                                       fill FREQG array [Hz]
      IF (CATBLK(KINAX+JLOCF).NE.MCH) THEN
         IRET = 8
         MSGTXT = 'Problem with frequency axis'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       IEQ is integer and is
C                                       equivalenced! to REQ
C                                       which is real!
      IEQ = CATBLK(KRCRP+JLOCF)
      REFPIX = REQ
C                                       assemble various freq. mapping
C                                       functions [GHz = turns/ns]
      DO 200 J = 0, MFQ-1
C                                       I is the actual IF
         I = INT (J / MCH)
C                                       L is the channel within an IF
         L = MOD (J , MCH)
C                                       K is the eff. IF
         K = INT (J / NFQ)
C                                       FREQS + FREQ = actual sky nu
         FREQS(1+J) = FREQIF(1+I) + DFINC(1+I)*(1.0+L - REFPIX)
C                                       FREQZ = nu rel. to the eff. IF
         FREQZ(1+J) = (FREQS(1+J) - FREQIF(1+K)) * NS2SEC
C                                       FREQY = special nu for SOLMOD
         FREQY(1+J) = 0.0

         IF (SOLMOD(4:4).EQ.'S')
     *        FREQY(1+J) = (FREQS(1+J) - FREQIF(1+I))         * NS2SEC

         IF (SOLMOD(4:4).EQ.'I')
     *        FREQY(1+J) = -(FREQ + 2*FREQIF(1+I) - FREQS(1+J))
     *        * ( FREQ / (FREQ + FREQIF(1+I)) )**2            * NS2SEC

         IF (SOLMOD(4:4).EQ.'T')
     *        FREQY(1+J) = -(FREQ*FREQ / (FREQ + FREQS(1+J))) * NS2SEC
C
         IF (PRTLEV.GT.15)
     *   WRITE (*,*) 'FR = ',SOLMOD,FREQS(1+J), FREQZ(1+J),FREQY(1+J)
 200     CONTINUE
C                                       Get DFMAX, max frequency span
      DF1 = MIN (FREQS(1), FREQS(MCH))
      DF2 = MAX (FREQS(1), FREQS(MCH))
      DO 150 I = 1, NOAIF-1
         DF1 = MIN ( DF1, FREQS(1+I*MCH), FREQS(MCH+I*MCH) )
         DF2 = MAX ( DF2, FREQS(1+I*MCH), FREQS(MCH+I*MCH) )
 150     CONTINUE
C                                       self-defense against DFMAX=0!!
      DFMAX = MAX ( DF2-DF1 , ABS(DFINC(1)) )
C                                       Get DF, min freq. step [Hz]
      DF = DFMAX* 2/3
      IF (MFQ.GT.1) THEN
C                                       check channel sep. values
         DO 250 I=1,MIF
            DF = MIN ( DF, DFINC(I) )
 250        CONTINUE
C                                      CHECK FOR OVERLAPS
         DO 301 I=0, MIF-1
            DO 300 J=0,MIF-1
               IF (FREQS(1+I*MCH).LT.FREQS(1+J*MCH)) THEN
C                                      non-overlapping
                  IF (FREQS(MCH+I*MCH).LT.FREQS(1+J*MCH)) THEN
                     DF1=MIN(DF,
     *                    ABS(FREQS(1+J*MCH)-FREQS(MCH+I*MCH)))
                  ELSE
C                                      overlapping
                     DF1 = MOD(
     *                    FREQS(1+J*MCH)-FREQS(1+I*MCH),
     *                                                  DFINC(1+I))
                     DF1 = MIN ( DF1, DFINC(1+I)-DF1 )
                     END IF
C     ignore minor mis-alignments
                  IF (DF1.GT.DF/10) DF=DF1
                  END IF
 300           CONTINUE
 301        CONTINUE
         END IF
C                                       Get NF, the data array span
C                                       [make sure NF is big enough
      NF = (DFMAX / DF + 1.0)
      IF (NF*DF.LT.DFMAX) NF = NF + 1
      IF (PRTLEV.GE.19) WRITE (*,*) 'in KGTDAT, NF = ',NF, DFMAX,DF

      DFRAC = DWINDO * DF * NS2SEC
      DF = DF * NS2SEC
C                                       IFRQ is an integer index array
C                                       to store slot positions of FREQs
C                                       LOOP over all averaged IFs
      DO 350 J = 0, MFQ-1
C                                       L is the effective IF number
         L = MOD (J , NFQ)
C                                       I is the effective channel number
         I = INT (J / NFQ)
C        DJUNK    = (FREQS(1+J) - FREQS(1+I*MCH)) * NS2SEC
         DJUNK    = (FREQS(1+J) - FREQS(1+I*NFQ)) * NS2SEC
         IFRQ(1+J)  = ABS(DJUNK/DF) + 1.001
         IF (IFRQ(1+J).GT.NF) THEN

            IRET = 9
            GO TO 999
            END IF
 350     CONTINUE
C                                       open vis file
      IF (VISDSK.EQ.0) THEN
         CALL ZPHFIL ('SC', DISK, VCNO, 1, IFILE, IRET)
      ELSE
         CALL ZPHFIL ('UV', DISK, VCNO, 1, IFILE, IRET)
         END IF
      IF (IRET.NE.0) THEN
         IRET = 2
         GO TO 999
         END IF
      CALL ZOPEN (LUNUVI, FINDI, DISK, IFILE, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1005) IRET
         CALL MSGWRT (6)
         IRET = 2
         GO TO 990
         END IF
      FOPEN = .TRUE.
C                                       INIT UV data access!
      VO = 0
      NIN = 1
      BO = 1
      CALL UVINIT ('READ', LUNUVI, FINDI, NVIS, VO, LREC, NIN,
     *   JBUFSZ, BUFF1, BO, BINDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         CALL MSGWRT (6)
         IRET = 4
         GO TO 999
         END IF
C                                      Normal exit for 'INIT'
      GO TO 999
 500  CONTINUE
C                                       ACCESS UV data
      NIN = 0
      CALL UVDISK ('READ', LUNUVI, FINDI, BUFF1, NIN, BINDI, IRET)
C                                       EOF => end of file
      IF (IRET.EQ.4) THEN
         CALL ZCLOSE(LUNUVI, FINDI, IRET)
         FOPEN = .FALSE.
         IRET = 5
         GO TO 999
         END IF
C                                       0 pts in => end of soln intrvl
      IF (NIN.LE.0) THEN
         CALL ZCLOSE(LUNUVI, FINDI, IRET)
         FOPEN = .FALSE.
         IRET = 6
         GO TO 999
         END IF
C                                       any other code => error!
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET, 'READ'
         CALL MSGWRT (6)
         CALL ZCLOSE(LUNUVI, FINDI, IRET)
         FOPEN = .FALSE.
         IRET = 7
         GO TO 999
         END IF
C                                       Normal exit for 'READ'
      GO TO 999
 600  CONTINUE
      IF (FOPEN) THEN
         CALL ZCLOSE(LUNUVI, FINDI, IRET)
         FOPEN = .FALSE.
         END IF
C                                       Normal exit for 'CLOS'
      GO TO 999
C                                       Error message
 990  CALL MSGWRT (6)
C
 999  RETURN
C---------------------------------------------------------------------
 1005 FORMAT ('KGTDAT: ERROR',I3,' OPENING INPUT UV FILE')
 1010 FORMAT ('KGTDAT: ERROR',I3,' INITING UV FILE')
 1100 FORMAT ('KGTDAT: ERROR',I3,1X,A4,'ING UV FILE')
 2040 FORMAT ('KGTDAT: ERROR',I3,' READING OLD FQ TABLE')
 2050 FORMAT ('KGTDAT: ERROR',I3,' WRITING NEW FQ TABLE')
      END
      SUBROUTINE KDVMOD (VISDSK, VISCNO, IRET)
C-----------------------------------------------------------------------
C   KDVMOD divides the CLEAN model visibilities into the data.
C   If no model is found or a point model is specified then the data
C   is divided by the flux density found in the Source (SU) table.
C   Inputs: from commons
C     XNCOMP    R    Number of components to be divided.
C     DISKIN    R    Input file disk number.
C     CNOIN     I    Input file catalog number.
C     DISK2     R    CLEAN file disk number.
C     XNMAP     R    Number of model files.
C     CCTVER    I    CC table version number.
C     SMODEL(7) R    If .lt. 0 use no model, if .gt. 0 use point model
C   Output:
C     CNOIN2    I    CLEAN file catalog number.
C     IRET      I    Return code, 0 => ok, otherwise not.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   MODEL, METHOD, ISTOKE, DISKO, ISCR, CHAN, IIF, NIF,
     *   NCHAN, I, IROUND, VISDSK, VISCNO
      LOGICAL   DOMSG, F, WASOME
      INCLUDE 'KRING.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INTEGER   BITER(MAXFLD)
      REAL RBUF(MAXIF)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSCD.INC'
      DATA DOMSG, F /.TRUE.,.FALSE./
C---------------------------------------------------------------------
      IRET = 0
C                                       Set model and method
C                                       (Optns limited by sort order.)
      MODEL = 0
      IF (CMOD.EQ.'COMP') MODEL = 1
      IF (CMOD.EQ.'IMAG') MODEL = 2
      IF (CMOD.EQ.'SUBI') MODEL = 3
      METHOD = 0
      IF (CMETH.EQ.'DFT') METHOD = -1
      IF (CMETH.EQ.'GRID') METHOD = 1
C                                       Point source parameters
      DOPTMD = ABS (SMODEL(1)).GT.1.0E-20
      PTFLX = SMODEL(1)
      PTRAOF = SMODEL(2)
      PTDCOF = SMODEL(3)
      PARMOD(1) = SMODEL(4)
      PARMOD(2) = SMODEL(5)
      PARMOD(3) = SMODEL(6)
      PARMOD(4) = SMODEL(7)
C                                       Get info on model file(s)
      LIMFLX = XFLUX
      MFIELD = IROUND (XNMAP)
      IF (MFIELD.LE.0) MFIELD = 1
      NONEG = F
      WASOME = F
      DO 10 I = 1,MFIELD
         BITER(I) = 1
         IF (I.LE.MAXAFL) THEN
            NCOMP(I) = ABS (XNCOMP(I)) + 0.1
            IF (XNCOMP(I).LE.-0.4) NONEG = .TRUE.
            IF (NCOMP(I).GT.0) WASOME = .TRUE.
         ELSE
            NCOMP(I) = 0
            IF (WASOME) NCOMP(I) = 1000000000
            END IF
 10      CONTINUE
      FACGRD(1) = 1.0
      FACGRD(2) = 1.0
      IF (DOPTMD) THEN
         DO3DIM = .FALSE.
      ELSE
         CALL SETGDS (DISKIN, CNOIN, NAME2, CLAS2, SEQ2, DISK2, MFIELD,
     *      CCTVER, NCOMP, BITER, MODEL, METHOD, BUFF1, BUFF2, ISTOKE,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         IF (MODEL.GT.0) THEN
            IF (MODEL.EQ.3) THEN
               MSGTXT = 'Using sub-images for the source model'
            ELSE IF (MODEL.EQ.2) THEN
               MSGTXT = 'Using images for the source model'
            ELSE
               MSGTXT = 'Using Clean Component source model'
               END IF
            CALL MSGWRT (3)
            CALL FACSET (DISKIN, CNOIN, 1, SOUWAN(1), MODEL, 1.0, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
      XVER = VER
      CNOIN2 = CCCNO(1)
C                                       Divide data by model
      DISKO = VISDSK
      ISCR = VISCNO
      NIF = 1
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
      CHAN = 1
      COMPDT = .FALSE.
      DATDIV = .TRUE.
C                                       Consider whether to process
C                                       1 IF at a time
      IF ((NIF.GT.1) .AND. (MODEL.EQ.1) .AND.
     *   (ABS(FACGRD(1)-1.0).GT.1.E-10)) THEN
C                                       number of channels
         NCHAN = CATBLK(KINAX+JLOCF)
C                                       For each IF
         DO 15 IIF = 1,NIF
C                                       Already know IF 1 scale
            IF (IIF.GT.1) THEN
C                                       Reset Components for div
               IF (MFIELD.GT.0) THEN
                  DO 12 I = 1,MFIELD
                     BITER(I) = 1
                     IF (I.LE.MAXAFL) THEN
                        NCOMP(I) = ABS (XNCOMP(I)) + 0.1
                     ELSE
                        NCOMP(I) = 0
                        IF (WASOME) NCOMP(I) = 1000000000
                        END IF
 12                  CONTINUE
                  CALL SETGDS (DISKIN, CNOIN, NAME2, CLAS2, SEQ2, DISK2,
     *               MFIELD, CCTVER, NCOMP, BITER, MODEL, METHOD, BUFF1,
     *               BUFF2, ISTOKE, IRET)
                  IF (IRET.NE.0) GO TO 999
                  XVER = VER
                  CNOIN2 = CCCNO(1)
                  END IF
C                                       Divide data by model
               DISKO = VISDSK
               ISCR = VISCNO
C                                       Set division parameters
               COMPDT = .FALSE.
               DATDIV = .TRUE.
               FACGRD(1) = 1.0
               IF (MODEL.GT.0) THEN
                  CALL FACSET (DISKIN, CNOIN, IIF, SOUWAN(1), MODEL,
     *               1.0, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
               END IF
C                                       start channel
            CHAN = 1 + (NCHAN * (IIF-1))
C                                       Divide 1 IF by model
            CALL UVMDIV (VISDSK, VISCNO, DISKO, ISCR, MODEL, METHOD,
     *         DOMSG, CHAN, NCHAN, CATBLK, JBUFSZ, FRQSEL, BUFF1,
     *         BUFF2, UBUFF, RBUF, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL UNSETG (BUFF2)
            DOMSG  = .FALSE.
 15         CONTINUE
C                                       else processing all IFs
      ELSE
         CHAN = 1
         NCHAN = CATBLK(KINAX+JLOCF) * NIF
C                                       Div all vis by model
         CALL UVMDIV (VISDSK, VISCNO, DISKO, ISCR, MODEL, METHOD, DOMSG,
     *      CHAN, NCHAN, CATBLK, JBUFSZ, FRQSEL, BUFF1, BUFF2, UBUFF,
     *      RBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (.NOT.DOPTMD) CALL UNSETG (BUFF2)
         END IF
C                                       Get true values of NCOMP
      DO 20 I = 1,MFIELD
         NCOMP(I) = NSUBG(I) - 1
 20      CONTINUE
C                                       Model divided by data now
C                                       in scratch file.
      VISDSK = 0
      VISCNO = ISCR
 999  RETURN
      END
      SUBROUTINE GTTIME ( TIME, STIME)
C-----------------------------------------------------------------------
      CHARACTER STIME*12
      REAL TIME
      INTEGER DAY, HOUR, MINUTE, SECOND
C-----------------------------------------------------------------------
      DAY    = TIME / 86400.0
      HOUR   = TIME / 3600.0
      MINUTE = TIME / 60.0
      SECOND = TIME - 60.0*MINUTE
      MINUTE = MINUTE - 60.0*HOUR
      HOUR   = HOUR - 24.0*DAY
      WRITE (STIME, 1000) DAY, HOUR, MINUTE, SECOND
      STIME(4:4) = '/'
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (4I3)
      END
      SUBROUTINE REMEM (SIZET, NEEDT, SIZEF, NEEDF, SIZEFT, NEEDFT,
     *   BPCORE, OBPCOR, BPSCOR, OBPSCO, WRK1, OWRK1, WRK2, OWRK2,
     *     IRET)
C-----------------------------------------------------------------------
      INTEGER   SIZET, NEEDT, SIZEF, NEEDF, SIZEFT, NEEDFT, IRET
      LONGINT   OBPCOR, OBPSCO, OWRK1, OWRK2
      REAL      BPCORE(2), BPSCOR(2), WRK1(2), WRK2(2)
C
      INTEGER   NWD, LSTLEN(2)
      INCLUDE 'INCS:PFFT.INC'
      COMMON /KRLAST/ LSTLEN
C-----------------------------------------------------------------------
C                                       free previous allocation
      IF (SIZET.NE.0) THEN
         CALL ZMEMRY ('FRAL', 'REMEM', NWD, BPCORE, OBPCOR, IRET)
C                                       better not happen!!!
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       this is for the end
      IF (NEEDT.EQ.0) GO TO 990
C                                       this is for BPCORE
      OBPCOR = 0
      NWD = 2 * NEEDT * NEEDF
      NWD = (NWD - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'REMEM', NWD, BPCORE, OBPCOR, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       this is for BPSCOR
      OBPSCO = 0
      NWD = 2 * NEEDFT
      NWD = (NWD - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'REMEM', NWD, BPSCOR, OBPSCO, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       this is for WRK1
      OWRK1 = 0
      NWD = FTWMUL * NEEDFT + FTWADD
      NWD = (NWD - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'REMEM', NWD, WRK1, OWRK1, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       this is for WRK2
      OWRK2 = 0
      NWD = FTWMUL * NEEDFT + FTWADD
      NWD = (NWD - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'REMEM', NWD, WRK2, OWRK2, IRET)
      IF (IRET.NE.0) GO TO 990
      LSTLEN(1) = 0
      LSTLEN(2) = 0
C                                       update size registers
      SIZET = NEEDT
      SIZEF = NEEDF
      SIZEFT = NEEDFT
      GO TO 999
 990  CONTINUE
C                                       come here for error messages
 999  CONTINUE
      RETURN
      END
      SUBROUTINE KFFT (BLINDX, VVIS, NR, ND,
     *   NTIME, NT, MT, MTM, PBL, MBL, DF, MF,
     *   MFQ, NF, IFP, IFRQ, TIMB, DTIMB, MAN, SWT, WTB,
     *   SNRFFT, TRYHRD,
     *   PRTLEV, BLDO, SRCHL, NSRCH, PRIRTY, SIZET, SIZEF, SIZEFT,
     *   BPCORE, BPSCOR, WRK1, WRK2,
     *   GOTSLN, CPHASE, CDELY, CRATE, CSGMA,
     *   BSLOP, ESLOP)
C---------------------------------------------------------------------
C   KFFT makes an initial estimate of the delay, rate and phase
C   of a visibility array on all baselines.
C    Currently assumes that all frequencies are spaced by multiples
C   of the minimum spacing.
C   Input:
C    SRCHL(NSRCH)        I    Preferred order of baselines
C    NSRCH               I    Number of baselines to search over
C    VVIS(2,MTM,MFQ,MBL)  R   Visibility array
C    MTM                 I    Maximum number of time integrations.
C    MFQ                 I    Maximum number of frequency channels.
C    MAN                 I    Number of antennas
C    NT                  I    Number of times
C    WGT(MBL)            R    Weight modification array; used if
C                             unequal integration times in the data.
C    SNRFFT              R    Minimum SNR allowed
C    PRTLEV               I    Print level
C    BLDO                I    -1 use refant set by user,
C                             -2 automatic choice
C   Output:
C    WTB(MBL)             R    Baseline weight array, returned
C                                     normalized.
C    CPHASE(MAN)       R    Phase part of solution
C    CDELY(MAN)       R    delays in seconds.
C    CRATE(MAN)       R    Rates in Hz.
C    CSGMA(MAN)       R    sigma = noise-to-signal ratio
C---------------------------------------------------------------------
      INCLUDE 'KRING.CONSTS'
      INTEGER   MFQ, MTM, MBL, MAN, NR, ND, NF, IFRQ(NF)
      INTEGER SIZET, SIZEF, SIZEFT
      REAL BPCORE(2,SIZET,SIZEF), BPSCOR(2,SIZEFT), WRK1(*), WRK2(*)
      REAL TIMB(MTM,MBL), DTIMB(MTM,MBL)
      INTEGER   PBL, BLINDX(PBL), NSRCH,
     *   SRCHL(NSRCH), NTIME(MBL), GOTSLN(MAN),
     *   NT, PRTLEV, BLDO
      REAL  VVIS(2,MTM,MFQ,MBL)
      DOUBLE PRECISION CPHASE(MAN), CDELY(MAN), CRATE(MAN), CSGMA(MAN)
      REAL  WTB(MBL), SNRFFT, TRYHRD, SWT(MAN), BSLOP, ESLOP
      INTEGER   REFA, ANT, IFP,
     *   ISI, JSI, MF, MT,
     *   REFI, REFJ, UREFI, UREFJ, IBL, NSTACK
      DOUBLE PRECISION DF
      REAL      SNRAT, RX, CPPHS, CPRAT, CPDEL, CPSGMA
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   PRIRTY(MAXANT)
      REAL      TTPHAZ, TPHAZ, TDELY, TRATE, TWT, ZVEC(8)
      REAL      TRYAGN
      CHARACTER*2 STKCOD(3)
      CHARACTER PART2*8, PART3*8, PART4*12
C                                       need these declared somewhere
C                                       here
      INCLUDE 'INCS:GAIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA STKCOD / '  ',  '+ ', '++'/
C---------------------------------------------------------------------
C                                       GOTSLN track of which antennas
C                                       are 'solved' for.
C                                       GOTSLN(I)= -1 => I is unsolved
C                                       GOTSLN(I)=  0 => I is no-solve
C                                       GOTSLN(I)>  0 => I is solved &
C                                              referenced to GOTSLN(I)
C                                       CSGMA>0  => soln is good
C                                       CSGMA<=0 => soln is bad or
C                                                    partly good
C
C                                       Loop over baselines
      DO 350 IBL = 1,NSRCH
C                                       Get ant codes and check  wgts
         ISI = INT ( SRCHL(IBL) / (MAN+1) )
         JSI = MOD ( SRCHL(IBL) , (MAN+1) )
         IF ((SWT(ISI).LE.0).OR.(SWT(JSI).LE.0.0)) GO TO 350
C
C                                       init baseline soln.
         CPPHS = 0.0
         CPRAT = 0.0
         CPDEL = 0.0
         CPSGMA = 0.0
C                                       get ultimate refant (UREF) and
C                                         chain solns
C                                       Notes:
C                                       if GOTSLN(A) = 0, A is not to be solved
C                                       if GOTSLN(A) = A, A is self-referenced
C
C                                       chain solutions until CSGMA(REFA)=<0.0

C                                       At first, assume the antenna points to
C                                       to it's own reference antenna
         REFI = ISI
         REFJ = JSI
 351     CONTINUE
         UREFI = GOTSLN(REFI)
         IF ((UREFI.NE.REFI).AND.(UREFI.GT.0)) THEN
            CPPHS = CPPHS + CPHASE(REFI)
            CPRAT = CPRAT + CRATE(REFI)
            CPDEL = CPDEL + CDELY(REFI)
            IF (CSGMA(REFI).GT.0.0) THEN
               CPSGMA = CPSGMA + CSGMA(REFI)
               REFI = UREFI
               GO TO 351
               END IF
            END IF
         UREFJ = GOTSLN(REFJ)
         IF ((UREFJ.NE.REFJ).AND. (UREFJ.GT.0)) THEN
            CPPHS = CPPHS - CPHASE(REFJ)
            CPRAT = CPRAT - CRATE(REFJ)
            CPDEL = CPDEL - CDELY(REFJ)
            IF (CSGMA(REFJ).GT.0.0) THEN
               CPSGMA = CPSGMA + CSGMA(REFJ)
               REFJ = UREFJ
               GO TO 351
               END IF
            END IF
C                                       CP* is soln for baseline
C                                           REFI-REFJ
C
         IF ((UREFI.GE.0).AND.(UREFJ.GE.0).AND.(UREFI.EQ.UREFJ).AND.
     *      ((CSGMA(REFI).GE.0.0).AND.(CSGMA(REFJ).GE.0.0))) GO TO 350
C
C                                       try a phase only search ?
C                                       [if either soln is not good]
         IF ((CSGMA(REFI).GE.0.0).AND.(CSGMA(REFJ).GE.0.0)) THEN
            TRYAGN = -1.0
         ELSE
            TRYAGN = TRYHRD
            END IF
C                                       xfer residual delay and rate
C                                         into fringe-search routine
C                                       SEARCH THIS BASELINE
         NSTACK = BLDO
         CALL KSTACK (MAN, MBL, ISI, JSI, NSTACK, WTB, PBL,
     *      BLINDX, MFQ, IFP, NF, IFRQ, MF, DF, ND, MTM, NT, MT,
     *      NTIME, NR, TIMB, DTIMB, VVIS, TRATE, TDELY, TPHAZ,
     *      SNRFFT, SIZET, SIZEF, SIZEFT, BPCORE, BPSCOR, WRK1, WRK2,
     *      BSLOP, ESLOP, TRYAGN, TWT, ZVEC)
C
C                                       convert phase to cycles, unwrap
         TPHAZ = TPHAZ / TWOPI
         TPHAZ = MOD (TPHAZ, 1.0)
         TPHAZ = MOD (TPHAZ+1.5,1.0) - 0.5
C   if TRYAGN >0 leaving KSTACK, TRATE and TDELY were used but not
C   replaced, otherwise, TRATE, TDELY were not used
C
         IF (PRTLEV.GE.2) THEN
            TTPHAZ = MOD (TTPHAZ, 1.0)
            TTPHAZ = MOD (TTPHAZ + 1.5, 1.0) - 0.5
            TTPHAZ = TTPHAZ * 360.0
            SNRAT = MIN (9999999.99, TWT)
            PART2 = ' '
            PART3 = ' '
            PART4 = ' '
            WRITE (PART2,'(F8.1)') TTPHAZ
            IF (NR.GT.1) WRITE (PART3,'(F8.1)') TRATE
            IF (ND.GT.1) WRITE (PART4,'(F12.3)') TDELY
            WRITE (MSGTXT, 2010) ISI, JSI, PART2, PART3, PART4,
     *           SNRAT, STKCOD(NSTACK)
            IF (PRTLEV.NE.10) CALL MSGWRT (4)
            IF (PRTLEV.GE.11) WRITE (*,3000) ISI, JSI, IFP, ZVEC
            END IF
C                                       is this a GOOD soln?
         IF ((TWT.LT.SNRFFT).AND.(SNRFFT.GT.0.0)) GO TO 350
C
C                                       add in baseline soln
C                                       TRYAGN<0 => we have new solns
         IF (TRYAGN.LT.0.0) THEN
            CPPHS = CPPHS + TPHAZ
            CPRAT = CPRAT + TRATE
            CPDEL = CPDEL + TDELY
            CPSGMA = CPSGMA + 1./TWT
         ELSE
C                                       otherwise, update phase & wgt
            CPPHS = TPHAZ
            CPRAT = TRATE
            CPDEL = TDELY
            CPSGMA = 1./TWT
            END IF
C                                       CP now has full soln for the
C                                       baseline REFI-REFJ
C
C                                       UREFI: ISI:
C                                       < 0 not yet ref'd
C                                       = 0 ref'd to no-solve
C                                       > 0 ref'd to UREFI
C
         IF (((UREFI*UREFJ.GT.0).AND.(PRIRTY(REFI).LT.PRIRTY(REFJ)))
     *      .OR.
     *       ((UREFI*UREFJ.LE.0).AND.((UREFJ.LT.0).OR.(UREFI.EQ.0))))
     *      THEN
            REFA = REFI
            ANT = REFJ
            RX = 1.0
         ELSE
            REFA = REFJ
            ANT = REFI
            RX = -1.0
            END IF
C                                       stuff soln into ANT position
         CPHASE(ANT) = CPPHS * RX
         CRATE(ANT)  = CPRAT * RX
         CDELY(ANT)  = CPDEL * RX
         CSGMA(ANT)  = CPSGMA
C                                       give REFA a not-bad weight
         CSGMA(REFA) = ABS(CSGMA(REFA))
C                                       ref. to no-solve antennas,
C                                       but dont let this one become
C                                         one of those!
         IF (PRTLEV.GE.19) WRITE (*,*) 'in kfft',ISI,JSI,IBL,GOTSLN
         GOTSLN(ANT) = GOTSLN(REFA)
 350     CONTINUE
C
 999  RETURN
C---------------------------------------------------------------------
 3000 FORMAT ('FFT ',3(I3,1X),10(F20.5,1X))
 2010 FORMAT ('FFT: ',I2.2,'-',I2.2, A8, A8, A12, 12X, F9.2, A2)
      END
      SUBROUTINE KRNREF (REFAN, MAN, GOTSLN, PRIRTY, GDSGMA,
     *     CSGMA, CPHASE, CRATE, CDELY, PRTLEV)
C---------------------------------------------------------------------
C   KRNREF determines the reference antenna and makes certain that
C   all solutions are referenced to the reference antenna.
C   Input:
C    SRCHL(NSRCH)        I    Preferred order of baselines
C    NSRCH               I    Number of baselines to search over
C    MTM                 I    Maximum number of time integrations.
C    MFQ                 I    Maximum number of frequency channels.
C    MAN                 I    Number of antennas
C    NBL                 I    Number of baselines
C    NT                  I    Number of times
C    WGT(MBL)            R    Weight modification array; used if
C                             unequal integration times in the data.
C    REFAN
C    SNRFFT              R    Minimum SNR allowed
C    PRTLEV               I    Print level
C    BLDO                I    -1 use refant set by user,
C                             -2 automatic choice
C   Output:
C    WTB(MBL)             R    Baseline weight array, returned
C                                     normalized.
C    CPHASE(MAN)       R    Phase part of solution
C    CDELY(MAN)       R    delays in seconds.
C    CRATE(MAN)       R    Rates in Hz.
C    CSGMA(MAN)       R    sigma = noise-to-signal ratio
C    REFAN      I    Reference antennas used
C    IERR                I    Return code, 0=>OK, 1 => all data bad,
C                             2=>insufficient memory for window.
C---------------------------------------------------------------------
      INCLUDE 'KRING.CONSTS'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER REFAN, MAN, PRTLEV
      INTEGER GOTSLN(MAN), PRIRTY(MAXANT)
      DOUBLE PRECISION CPHASE(MAN), CDELY(MAN), CRATE(MAN), CSGMA(MAN)
      REAL GDSGMA
      INTEGER I, J, K, L
      LOGICAL REREFD, TRUE, FALSE
      INCLUDE 'INCS:GAIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA TRUE, FALSE /.TRUE.,.FALSE./
C---------------------------------------------------------------------
C                                       GOTSLN track of which antennas
C                                       are 'solved' for.
C                                       GOTSLN(I)
C                                       = -1 => I unsolved
C                                       =  0 => I is no-solve
C                                       >  0 => I solved & ref'd to GOTSLN(I)
C                                       CSGMA>0  => soln is good
C                                       CSGMA<=0 => soln is bad or
C                                                    partly good
C
C                                       Now for REFANT shenanigans
C                                       REFAN is a valid antenna?
      IF (PRTLEV.GE.20) WRITE (*,*) 'entering krnref'
      IF (CSGMA(REFAN).LE.0) REFAN = 0
C
      IF (REFAN.EQ.0) THEN
C                                       find no-solve with highest priority
C                                       that has data
         J = MAN+1
         DO 100 I = 1,MAN
            IF ((GOTSLN(I).EQ.0).AND.(PRIRTY(I).LT.J)) THEN
               REFAN = I
               J = PRIRTY(REFAN)
               END IF
 100        CONTINUE
         END IF
      IF (PRTLEV.GE.20)
     *        WRITE (*,*) 'first step, refan now = ',REFAN, GOTSLN

C                                      re-referencing loop
 200  CONTINUE
      IF (PRTLEV.GE.20)
     *     WRITE (*,*) 'start re-ref loop ', REFAN, GOTSLN
C                                       1,6) chain solns as necessary
      REREFD = FALSE
      DO 300 I = 1, MAN

         IF (GOTSLN(I).GT.0) THEN
            J = GOTSLN(I)
            IF (J.NE.GOTSLN(J)) THEN
               CPHASE(I) = CPHASE(I) + CPHASE(J)
               CRATE(I)  = CRATE(I) + CRATE(J)
               CDELY(I)  = CDELY(I) + CDELY(J)
               CSGMA(I)  = CSGMA(I) + CSGMA(J)
               GOTSLN(I) = GOTSLN(J)
               REREFD = TRUE
               END IF
            END IF
 300  CONTINUE
      IF (REREFD) GO TO 200
C                                       2) find most common refant
      IF (REFAN.EQ.0) THEN
         K = 0
         DO 500 I = 1, MAN
            L = 0
            DO 400 J = 1, MAN
               IF (GOTSLN(J).EQ.I) THEN
                  L = L + 1
                  END IF
 400           CONTINUE
            IF (L.GT.K) THEN
               REFAN = I
               K = L
               END IF
 500        CONTINUE
         END IF
C                                       3,4) blank solns not chained to REFAN
C                                            does REFAN has highest priority?
C                                            [if REFAN is SOLVE- ant]
      I = REFAN
      DO 600 J = 1,MAN
         IF ((GOTSLN(J).GT.0).AND.(GOTSLN(J).NE.REFAN)) THEN
            CSGMA(J) = -1.0D0
            END IF
         IF (GOTSLN(REFAN).NE.0) THEN
            IF ((PRIRTY(J).LT.PRIRTY(I)).AND.(CSGMA(J).GT.0.0)) I = J
            END IF
 600     CONTINUE
C                                       5) reverse soln to REFAN?
      IF (I.NE.REFAN) THEN
         REFAN = I
         J = GOTSLN(REFAN)
         IF ((J.GT.0).AND.(J.NE.REFAN)) THEN
            CPHASE(J) = -CPHASE(REFAN)
            CRATE(J)  = -CRATE(REFAN)
            CDELY(J)  = -CDELY(REFAN)
            CSGMA(J)  = CSGMA(REFAN)
            CSGMA(REFAN) = GDSGMA
            GOTSLN(J) = REFAN
            GOTSLN(REFAN) = REFAN
            GO TO 200
            END IF
         END IF
C                                       give REFAN good weight,
      IF (CSGMA(REFAN).EQ.0.0) CSGMA(REFAN) = GDSGMA
C                                       force no-solves to refer to REFAN
      IF (GOTSLN(REFAN).EQ.0) THEN
         DO 700 I = 1,MAN
            IF (GOTSLN(I).EQ.0) GOTSLN(I) = REFAN
 700        CONTINUE
         END IF
 999  RETURN
      END
      SUBROUTINE KSTACK (MAN, MBL, REFSTA, SOLSTA, NSTACK, WTB,
     *   PBL, BLINDX,
     *   MFQ, IFP, NF, IFRQ, MF, DF, ND, MTM, NT, MT, NTIME,
     *   NR, TIMB, DTIMB, VVIS, TRATE, TDELY, PHAZE,
     *   SNRFFT, SIZET, SIZEF, SIZEFT, BPCORE, BPSCOR, WRK1, WRK2,
     *   BSLOP, ESLOP, TRYAGN, TWT, ZVEC)
C---------------------------------------------------------------------
C   Subroutine to solve for the dely, rate and phase of a given ant.
C   wrt a given reference antenna.
C    A coarse fringe search is done by FFTing freq-time data to the
C   delay-rate domain and searching for a maximum amplitude.
C   Interpolation of the solution is done by padding the data arrays
C   with zeroes (MT,MF) before the FFT.
C      Sensitivity is increased by (optionally) stacking data from
C   several baseline combinations before the search.
C     The data in VVIS are assumed to be evenly spaced in
C   time.  The spacing in frequency is assumed to
C   be indexed properly in IFRQ [the spacing need not be uniform].
C   Input:
C      NA       I                  Number of antennas
C      REFSTA   I                  Reference antenna to use.
C      SOLSTA   I                  Antenna for solutions.
C      NSTACK   I                  No. baseline combos to do 1,2, or 3
C      BLINDX   I(MXBASE)          baseline loc. code in data arrays
C      WTB      R(MXBASE)          Baseline weight array
C      MFQ   I                  Maximum number of frequency channels.
C      IFP      I                  First frequency number [0-offset]
C      NF       I                  Number of frequencies
C      IFRQ     I(NF)          Frequency array, indexed and folded
C      MF       I                  No. freq. in search FFT
C      ND     I                  Number of pos delays to search
C      MTM   I                  Maximum number of time integrations.
C      NT       I                  Number of times
C      WFTMOD   R(MXBASE)          multiples of base int-time [sec]
C      MT       I                  No. times in search FFT
C      NR     I                  Number of pos rates to search
C      VVIS    R(2,MTM,MFQ,MBL)    Visibility array
C   Output:
C      IR       I                  rate solution index
C      ID       I                  delay solution index
C      PHAZE    R                  phase at peak
C      TWT      R                  snr of detection
C      BPCORE   R(2,SIZET,SIZEF)         work array
C      BPSCORE  R(2,SIZEFT)           work array
C      DWORK    R(*)           work array
C---------------------------------------------------------------------
      INCLUDE 'KRING.CONSTS'
      INTEGER MAN, MBL, REFSTA, SOLSTA, NSTACK
      INTEGER PBL, BLINDX(PBL)
      REAL    WTB(MBL), TRYAGN, SLOP, BSLOP, ESLOP, ZVEC(8)
      INTEGER MFQ, IFP, NF, IFRQ(NF), MF, ND
      INTEGER MTM, NT, MT, NR
      REAL    VVIS(2,MTM,MFQ,MBL)
      REAL    DTP
      DOUBLE PRECISION DF, DFP
      REAL TIMB(MTM,MBL), DTIMB(MTM,MBL)
      REAL    TRATE, TDELY, PHAZE, TWT, SNRFFT
      INTEGER SIZET, SIZEF, SIZEFT
      INTEGER NTIME(MBL)
      REAL BPCORE(2,SIZET,SIZEF), BPSCOR(2,SIZEFT), WRK1(*), WRK2(*)
      INTEGER I, J
      INTEGER S1, S2, S3
      INTEGER Q0, Q1, Q2, Q3
      INTEGER I0, I1, I2, I3
      INTEGER     J1, J2, J3, JZ
      INTEGER               J1BEG, J1END, J2BEG, J2END, J3BEG, J3END
      REAL    T0BEG, T0END, T1BEG, T1END, T2BEG, T2END, T3BEG, T3END
      REAL    TBEG, TEND
      INTEGER IBL0, IBL1, IBL2, IBL3, NTUSE
      REAL DTINT, TRATEP, TDELYP
C      INTEGER INSTA1, INSTA2, K1, K2, K3,
      REAL    VR1, VI1, VR2, VI2, VR3, VI3, VR, VI, VW
      DOUBLE PRECISION SUMW, SUMWW, XCOUNT, AMPL, Z
C      REAL TBEG, TEND, DTINT
      DOUBLE PRECISION RWORK, IWORK
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:GAIN.INC'
C---------------------------------------------------------------------
 1    CONTINUE

      XCOUNT = 0.0D0
      SUMW = 0.0D0
      SUMWW = 0.0D0
C                                             zero bpcore array
      DO 20 I = 1,NF
         DO 10 J = 1,NT
            BPCORE(1,J,I) = 0.0
            BPCORE(2,J,I) = 0.0
 10         CONTINUE
 20      CONTINUE
C                                             direct baseline is special
C
C                                             PROCESS DIRECT BASELINE
C                                             [need Q0]
      I0 = REFSTA
      I1 = SOLSTA
C                                             [throw out self-baseline]
      IF (I0.EQ.I1) GO TO 999
C                                             get baseline orientation
C                                             get baseline index
      IF (I1.GT.I0) THEN
         IBL0 = I0*(MAN+1) + I1
         S1 = 1.0
      ELSE
         IBL0 = I1*(MAN+1) + I0
         S1 = -1.0
         END IF
C                                             check for data
      IF (BLINDX(IBL0).EQ.0) GO TO 999
C                                             get baseline code
      Q0 = BLINDX(IBL0)
C                                             check weight
      IF (WTB(Q0).LE.0.0) GO TO 999
C                                             USE:
C                                             beginning of earliest data
C                                             max number of times found
C                                             average time spacing
      SLOP = BSLOP
      NTUSE = NT
      DTINT = ( ESLOP - SLOP ) / NTUSE
C
C                                             OR:
C                                             beginning of earliest data
C                                               on this baseline
C                                             max number of times found
C                                               on this baseline
C                                             average time spacing
C                                               on this baseline
      SLOP = TIMB(1,Q0) - DTIMB(1,Q0)
      NTUSE = NTIME(Q0)
      DTINT = (TIMB(NTUSE,Q0)+DTIMB(NTUSE,Q0) - SLOP) / NTUSE
C
C                                             OR:
C                                             BETTER TIME ALGORITHM HERE
C                                             [use all timestamps on
C                                              this baseline to determine the
C                                              best spacing value to use
C                                              as well as the slop factor]
C
C                                             TO DETERMINE CELL SPACINGS
C                                             cell spacing in mHz
      DTP = DTINT
C                                             cell spacing in ns
      DFP = DF
C                                             baseline selected, now process
C                                             dynamic spectra
      DO 100 J=1,NTUSE
         T0BEG = SLOP + (J-1)*DTINT
         T0END = SLOP + J*DTINT
C                                             find timecodes for I1-I0
C                                             baseline
         CALL FNDK(T0BEG, T0END,
     *        TIMB(1,Q0), NTIME(Q0), J1BEG, J1END)
C                                             loop over I1-I0 timecodes
         DO 90 J1 = J1BEG, J1END
C                                             get actual duration
            T1BEG = TIMB(J1,Q0) - DTIMB(J1,Q0)
            T1END = TIMB(J1,Q0) + DTIMB(J1,Q0)
C                                             restrict to target duration
            TBEG = MAX (T0BEG, T1BEG)
            TEND = MIN (T0END, T1END)
            IF (TBEG.GE.TEND) GO TO 90
C                                             adjust weight
            VW = WTB(Q0) * (TEND-TBEG) / (2.*DTIMB(J1,Q0))
C                                             get I1-I0 vis
C                                             get target vis
            DO 801 I = 1,NF
               VR = VVIS(1,J1,IFP+I,Q0)
               VI = VVIS(2,J1,IFP+I,Q0) * S1
C                                             trap zero amplitudes
               IF ((ABS(VR)+ABS(VI)).GT.1.0E-20) THEN
C                                             sum wgtd vis
                  BPCORE(1,J,I) = BPCORE(1,J,I) + VW*VR
                  BPCORE(2,J,I) = BPCORE(2,J,I) + VW*VI
C                                             sum wgt
                  SUMW = SUMW + VW
C                                             sum wgt**2
                  SUMWW = SUMWW + VW*VW
C                                             count vis
                  XCOUNT = XCOUNT + 1
               END IF
 801        CONTINUE
 90      CONTINUE
 100  CONTINUE
      IF (NSTACK.EQ.1) GO TO 800
C                                             process singly indirect
C                                             baselines
C                                             REFSTA=I0-[I1]-I1=SOLSTA
      I0 = REFSTA
      I2 = SOLSTA
      DO 200 I1 = 1,MAN
         IF (I1.EQ.REFSTA) GO TO 200
         IF (I1.EQ.SOLSTA) GO TO 200
C                                             get baseline codes
         IF (I1.GT.I0) THEN
            IBL1 = I0*(MAN+1) + I1
            S1 = 1.0
         ELSE
            IBL1 = I1*(MAN+1) + I0
            S1 = -1.0
         END IF
         IF (I2.GT.I1) THEN
            IBL2 = I1*(MAN+1) + I2
            S2 = 1.0
         ELSE
            IBL2 = I2*(MAN+1) + I1
            S2 = -1.0
         END IF
C                                             check if baselines have data
         IF (BLINDX(IBL1).EQ.0) GO TO 200
         IF (BLINDX(IBL2).EQ.0) GO TO 200
C                                             get baseline codes
         Q1 = BLINDX(IBL1)
         Q2 = BLINDX(IBL2)
C                                             check baseline weights
         IF (WTB(Q1).LE.0.0) GO TO 200
         IF (WTB(Q2).LE.0.0) GO TO 200
C                                             baselines have been selected,
C                                             now process dynamic spectra
         DO 190 J=1,NTUSE
            T0BEG = SLOP + (J-1)*DTINT
            T0END = SLOP +     J*DTINT
C                                             find timecodes
            CALL FNDK(T0BEG, T0END,
     *           TIMB(1,Q1), NTIME(Q1), J1BEG, J1END)
            CALL FNDK(T0BEG, T0END,
     *           TIMB(1,Q2), NTIME(Q2), J2BEG, J2END)
C                                             loop over timecodes
            DO 180 J1 = J1BEG, J1END
               DO 170 J2 = J2BEG, J2END
C                                             find actual durations
               T1BEG = TIMB(J1,Q1) - DTIMB(J1,Q1)
               T1END = TIMB(J1,Q1) + DTIMB(J1,Q1)
               T2BEG = TIMB(J2,Q2) - DTIMB(J2,Q2)
               T2END = TIMB(J2,Q2) + DTIMB(J2,Q2)
C                                             restrict to target duration
               TBEG = MAX (T0BEG, T1BEG, T2BEG)
               TEND = MIN (T0END, T1END, T2END)
               IF (TBEG.GE.TEND) GO TO 170
C                                             adjust weights
                  VW =  DTIMB(J1,Q1)/WTB(Q1)
     *                + DTIMB(J2,Q2)/WTB(Q2)
                  VW = (TEND-TBEG) / (2.0 * VW)
C                                             get I2-I1-I0 vis
                  DO 802 I = 1, NF
                     VR1 = VVIS(1,J1,IFP+I,Q1)
                     VI1 = VVIS(2,J1,IFP+I,Q1) * S1
                     VR2 = VVIS(1,J2,IFP+I,Q2)
                     VI2 = VVIS(2,J2,IFP+I,Q2) * S2
                     VR = VR1*VR2 - VI1*VI2
                     VI = VR1*VI2 + VI1*VR2
C                                             trap zero amplitudes
                     IF ((ABS(VR)+ABS(VI)).GT.0.0) THEN
C                                             sum wgtd vis
                        BPCORE(1,J,I) = BPCORE(1,J,I) + VW*VR
                        BPCORE(2,J,I) = BPCORE(2,J,I) + VW*VI
C                                             sum wgt
                        SUMW = SUMW + VW
C                                             sum wgt**2
                        SUMWW = SUMWW + VW*VW
C                                             count vis
                        XCOUNT = XCOUNT + 1
                     END IF
 802              CONTINUE
 170           CONTINUE
 180        CONTINUE
 190     CONTINUE
 200  CONTINUE
      IF (NSTACK.EQ.2) GO TO 800
C                                             process doubly indirect
C                                             baselines
C
C                                             process I0-[I1]-[I2]-I3
      I0 = REFSTA
      I3 = SOLSTA
C                                             [throw out self-baselines]
C                                             [throw out direct baselines]
C                                             [throw out singly indirect
C                                                    baselines]
      DO 300 I1 = 1,MAN
         IF (I1.EQ.REFSTA) GO TO 300
         IF (I1.EQ.SOLSTA) GO TO 300
         DO 290 I2 = 1,MAN
            IF (I2.EQ.REFSTA) GO TO 290
            IF (I2.EQ.SOLSTA) GO TO 290
            IF (I2.EQ.I1) GO TO 290
C                                             get baseline codes
            IF (I1.GT.I0) THEN
               IBL1 = I0*(MAN+1) + I1
               S1 = 1.0
            ELSE
               IBL1 = I1*(MAN+1) + I0
               S1 = -1.0
            END IF
            IF (I2.GT.I1) THEN
               IBL2 = I1*(MAN+1) + I2
               S2 = 1.0
            ELSE
               IBL2 = I2*(MAN+1) + I1
               S2 = -1.0
            END IF
            IF (I3.GT.I2) THEN
               IBL3 = I2*(MAN+1) + I3
               S3 = 1.0
            ELSE
               IBL3 = I3*(MAN+1) + I2
               S3 = -1.0
            END IF
C                                             check if baselines have data
            IF (BLINDX(IBL1).EQ.0) GO TO 290
            IF (BLINDX(IBL2).EQ.0) GO TO 290
            IF (BLINDX(IBL3).EQ.0) GO TO 290
C                                             get baseline codes
            Q1 = BLINDX(IBL1)
            Q2 = BLINDX(IBL2)
            Q3 = BLINDX(IBL3)
C                                             check weights
            IF (WTB(Q1).LE.0.0) GO TO 290
            IF (WTB(Q2).LE.0.0) GO TO 290
            IF (WTB(Q3).LE.0.0) GO TO 290
C                                     baselines have been
C                                     selected, now process
C                                     dynamic spectra
            DO 280 J = 1,NTUSE
               T0BEG = SLOP + (J-1)*DTINT
               T0END = SLOP +     J*DTINT
C                                     find timecodes
               CALL FNDK(T0BEG, T0END, TIMB(1,Q1),
     *              NTIME(Q1), J1BEG, J1END)
               CALL FNDK(T0BEG, T0END, TIMB(1,Q2),
     *              NTIME(Q2), J2BEG, J2END)
               CALL FNDK(T0BEG, T0END, TIMB(1,Q3),
     *              NTIME(Q3), J3BEG, J3END)
C                                     loop over timecodes
               DO 270 J1 = J1BEG, J1END
                  DO 260 J2 = J2BEG, J2END
                     DO 250 J3 = J3BEG, J3END
C                                     find actual durations
                        T1BEG = TIMB(J1,Q1) - DTIMB(J1,Q1)
                        T1END = TIMB(J1,Q1) + DTIMB(J1,Q1)
                        T2BEG = TIMB(J2,Q2) - DTIMB(J2,Q2)
                        T2END = TIMB(J2,Q2) + DTIMB(J2,Q2)
                        T3BEG = TIMB(J3,Q3) - DTIMB(J3,Q3)
                        T3END = TIMB(J3,Q3) + DTIMB(J3,Q3)
C                                     restrict to target durations
                        TBEG = MAX (T0BEG, T1BEG, T2BEG, T3BEG)
                        TEND = MIN (T0END, T1END, T2END, T3END)
                        IF (TBEG.GE.TEND) GO TO 250
C                                     adjust weight
                        VW =   DTIMB(J1,Q1)/WTB(Q1)
     *                       + DTIMB(J2,Q2)/WTB(Q2)
     *                       + DTIMB(J3,Q3)/WTB(Q3)
                        VW = (TEND-TBEG) / (2.0 * VW)
C                                     get I3-I2-I1-I0 vis
                        DO 803 I = 1, NF
                           VR1 = VVIS(1,J1,IFP+I,Q1)
                           VI1 = VVIS(2,J1,IFP+I,Q1) * S1
                           VR2 = VVIS(1,J2,IFP+I,Q2)
                           VI2 = VVIS(2,J2,IFP+I,Q2) * S2
                           VR3 = VVIS(1,J3,IFP+I,Q3)
                           VI3 = VVIS(2,J3,IFP+I,Q3) * S3
                           VR =   VR1*VR2*VR3 - VR1*VI2*VI3
     *                          - VI1*VR2*VI3 - VI1*VI2*VR3
                           VI =   VR1*VR2*VI3 + VR1*VI2*VR3
     *                          + VI1*VR2*VR3 - VI1*VI2*VI3
C                                     trap zero amplitudes
                           IF ((ABS(VR)+ABS(VI)).GT.0.0) THEN
C                                     sum wgtd vis
                              BPCORE(1,J,I) = BPCORE(1,J,I) + VW*VR
                              BPCORE(2,J,I) = BPCORE(2,J,I) + VW*VI
C                                     sum wgt
                              SUMW = SUMW + VW
C                                     sum wgt**2
                              SUMWW = SUMWW + VW * VW
C                                     count vis
                              XCOUNT = XCOUNT + 1
                           END IF
 803                    CONTINUE
 250                 CONTINUE
 260              CONTINUE
 270           CONTINUE
 280        CONTINUE
 290     CONTINUE
 300  CONTINUE
C
 800  CONTINUE
      IF (XCOUNT.LE.0.0) GO TO 999
C                                             restart search from
C                                             below here
 450  CONTINUE
C                                             ready, go
C                                             get peak location and
C                                             peak value
      TRATEP = TRATE * DTP * MT
      TDELYP = TDELY * DFP * MF
      JZ = NT
      CALL KGTPEK (NF, JZ, MF, MT, ND, NR, IFRQ, SIZET, SIZEF, SIZEFT,
     *   BPCORE, BPSCOR, WRK1, WRK2, TRATEP, TDELYP, RWORK, IWORK,
     *   TRYAGN)
      TRATE = TRATEP / (DTP * MT)
      TDELY = TDELYP / (DFP * MF)
C
C                                             do polar conversion
C
C                                             ampl=0 ?
      IF ((IWORK.EQ.0.0).AND.(RWORK.EQ.0.0)) RWORK = 1.0
      PHAZE = -ATAN2 (IWORK,RWORK)
C                                             get peak amplitude
      AMPL = SQRT (RWORK*RWORK + IWORK*IWORK)
C
C                                             SNR computation section
C
      ZVEC(1) = AMPL
      ZVEC(2) = SUMW
      ZVEC(3) = SUMWW
      ZVEC(4) = XCOUNT
C
C                                             Version 1: from FRING
C
      Z = 0.0
      IF (SUMW.GT.1.0E-20) Z = AMPL/SUMW
      Z = MIN (Z, 0.99999D0) * PI/2.0
      Z = MAX(Z,1.0D-20)
      Z = (TAN(Z) ** 1.163) * SQRT (SUMW / SQRT (SUMWW / XCOUNT))
      ZVEC(5) = Z
C
C                                             Version 2: from AIPSMEMO 101
C
      CALL GETSNR (AMPL, SUMW, SUMWW, XCOUNT, Z)
      Z = Z * SQRT(XCOUNT)
      ZVEC(6) = Z
C                                             HIGH SNR FROM MEMO 101
      Z = AMPL*AMPL
      Z = Z - (SUMWW/XCOUNT) - (SUMW/XCOUNT)**2*(XCOUNT-1)
      Z = Z * XCOUNT / SUMWW
      Z = MAX(Z,1.0D-20)
      Z = 1.0 / SQRT(Z)
      ZVEC(7) = Z

C                                             LOW SNR FROM MEMO 101
      Z = AMPL*AMPL - (SUMWW/XCOUNT)
      Z = (8/PI) * Z / ((SUMW/XCOUNT)**2)
      Z = Z / (XCOUNT-1)
      Z = MAX(0.0D0,Z)
      ZVEC(8) = SQRT(Z)
C
C                                             END SNR computation section
C
      TWT = ZVEC(6)
C                                             TWT is now the SNR
C                                             of the visibility that
C                                             would be obtained if all
C                                             the data were added together
C                                             with this solution.
C
      IF ((TRYAGN.GT.0.0).AND.(TWT.LT.TRYAGN)) THEN
         TRYAGN = -1.0
         GO TO 450
         END IF
C                                             if SNR too low,
C                                             try unstacked
      IF ((TWT.LT.SNRFFT).AND.(NSTACK.GT.1).AND.(SNRFFT.GT.0.0)) THEN
         NSTACK = NSTACK-1
         GO TO 1
         END IF
 999  RETURN
      END
C
      SUBROUTINE KGTPEK (NF, NT, MF, MT, ND, NR, IFRQ, SIZET, SIZEF,
     *   SIZEFT, BPCORE, BPSCOR, WRK1, WRK2, RPEAK, DPEAK, RWORK, IWORK,
     *    TRYAGN)
C-----------------------------------------------------------------------
C   KGTPEK is a routine to do a gridded KRINGe search by means of
C   padded FFTs.
C   Inputs:
C      NF     I    No. frequency channels.
C      NT     I    No. time points.
C      MF     I    No. points for freq. FFT.
C      MT     I    No. points for time FFT.
C      ND   I    No. pos delays to search.
C      NR   I    No. pos rates to search.
C      SIZEFT    I    max of MF and MT
C      BPCORE R(2,SIZET,SIZEF)  array containing data in time and frequency
C      DWORK  R(*) work array
C      BPSCOR R(2,SIZEFT)  FFT temp array
C    Output: (in AP memory)
C      DPEAK  I    delay at peak
C      RPEAK  I    rate at peak
C      RWORK  D    complex visibility real part
C      IWORK  D    complex visibility imag part
C-----------------------------------------------------------------------
C                                       inputs
      INTEGER   NF, NT, MF, MT, ND, NR, IFRQ(NF)
      INTEGER   SIZET, SIZEF, SIZEFT
      REAL      BPCORE(2,SIZET,SIZEF), BPSCOR(2,SIZEFT), WRK1(*),
     *   WRK2(*), RPEAK, DPEAK, TRYAGN, RPPEAK, DPPEAK
      INCLUDE 'INCS:PSTD.INC'
C                                       outputs
      DOUBLE PRECISION RWORK, IWORK
C                                       internals
      INTEGER I, J, K, NRM, NDM, P, JF, IR, ID
      REAL AMPL, VMAX
C-----------------------------------------------------------------------
C                                       confirm that ND and NR are non-negative
      IF (ND.LT.0) GO TO 999
      IF (NR.LT.0) GO TO 999
C                                       try expedited search first
      IF (TRYAGN.GT.0.0) THEN
C                                       no search in delay or rate
         RWORK = 0.0D0
         IWORK = 0.0D0
         RPPEAK = TWOPI*RPEAK/MT
         DPPEAK = TWOPI*DPEAK/MF
         DO 110 I = 1,NF
            DO 100 J = 1,NT
               RWORK = RWORK + BPCORE(1,J,I) * COS(RPPEAK*J+DPPEAK*I)
               IWORK = IWORK + BPCORE(2,J,I) * COS(RPPEAK*J+DPPEAK*I)
               IWORK = IWORK - BPCORE(1,J,I) * SIN(RPPEAK*J+DPPEAK*I)
               RWORK = RWORK + BPCORE(2,J,I) * SIN(RPPEAK*J+DPPEAK*I)
 100        CONTINUE
 110     CONTINUE
         RPEAK = -RPEAK
         DPEAK = -DPEAK
C                                       finished
         GO TO 999
         END IF
C                                       do trivial case quickly
      IF ((ND.EQ.1).AND.(NR.EQ.1)) THEN
C                                       no search in delay or rate
         RWORK = 0.0D0
         IWORK = 0.0D0
         DO 112 I = 1,NT
            DO 111 J = 1,NF
               RWORK = RWORK + BPCORE(1,J,I)
               IWORK = IWORK + BPCORE(2,J,I)
 111        CONTINUE
 112     CONTINUE
C                                       finished
         GO TO 999
         END IF
C                                       set useful pointers
      NRM = NR/2
      NDM = ND/2

C                                       do rates first if
C                                       the NR search can be done in place OR
C                                       the NR search requires less space
C                                           than the ND search
      IF ((NR.LE.NT).OR.
     *   ((ND.GT.NF).AND.(NR.LE.ND))) THEN
C                                       Xform to rate space
C                                       Loop over all frequencies
         DO 230 J = 0, NF-1
C                                       zero scratch array
            DO 200 I=NT, MT-1
               BPSCOR(1,1+I) = 0.0
               BPSCOR(2,1+I) = 0.0
 200           CONTINUE
C                                       copy the Jth frequency to scratch space
            DO 210 I = 0, NT-1
               BPSCOR(1,1+I) = BPCORE(1,1+I,1+J)
               BPSCOR(2,1+I) = BPCORE(2,1+I,1+J)
 210           CONTINUE
C                                       Do time FFT.
            P = MT
            CALL DOFFT (BPSCOR, P, WRK1, 1)

C                                       copy back Xform from -NRM to NRP
            DO 220 I = 0,NR-1
               K = MOD(MT + I - NRM, MT)
               BPCORE(1,1+I,1+J) = BPSCOR(1,1+K)
               BPCORE(2,1+I,1+J) = BPSCOR(2,1+K)
 220           CONTINUE
 230        CONTINUE
C                                       Xform to delay space
C                                       search the results while in scratch
C                                       initialize to delay/rate plane center
         VMAX = -1.0
         ID = -20000
         IR = -20000
         DO 280 I = 0, NR-1
C                                       copy first rate to scratch space
            DO 250 J = 0, MF-1
               BPSCOR(1,1+J) = 0.0
               BPSCOR(2,1+J) = 0.0
 250           CONTINUE
            DO 260 J = 0,NF-1
               JF = IFRQ(1+J)
               BPSCOR(1,JF) = BPCORE(1,1+I,1+J)
               BPSCOR(2,JF) = BPCORE(2,1+I,1+J)
 260           CONTINUE

C                                       Do delay FFT.
            P = MF
            CALL DOFFT (BPSCOR, P, WRK2, 2)
C                                       search the result from -NDM to NDP
            DO 270 J = 0, ND-1
               K = MOD(MF + J - NDM, MF)
               AMPL = BPSCOR(1,1+K)**2 + BPSCOR(2,1+K)**2
               IF (AMPL.GT.VMAX) THEN
                  VMAX = AMPL
                  RWORK = BPSCOR(1,1+K)
                  IWORK = BPSCOR(2,1+K)
                  RPEAK = I
                  DPEAK = J
                  END IF
 270           CONTINUE
 280        CONTINUE
C                                       finished!
      ELSE
C                                       Xform to delay space
C                                       Loop over all times
         DO 330 I = 0, NT-1
C                                       zero scratch array
            DO 300 JF = 0, MF-1
               BPSCOR(1,1+JF) = 0.0
               BPSCOR(2,1+JF) = 0.0
 300           CONTINUE
C                                       copy Ith time to scratch space
            DO 310 J = 0, NF-1
               JF = IFRQ(1+J)
               BPSCOR(1,JF) = BPCORE(1,1+I,1+J)
               BPSCOR(2,JF) = BPCORE(2,1+I,1+J)
 310           CONTINUE
C                                       Do freq FFT.
            P = MF
            CALL DOFFT (BPSCOR, P, WRK2, 2)
C                                       copy only from -NDM to NDP
            DO 320 J = 0,ND-1
               K = MOD(MF-NDM +J, MF)
               BPCORE(1,1+I,1+J) = BPSCOR(1,1+K)
               BPCORE(2,1+I,1+J) = BPSCOR(2,1+K)
 320           CONTINUE
 330        CONTINUE
C                                       Xform to rate space
C                                       search the results without
C                                       removing from the scratch
C                                       space
C
C                                       initialize to delay/rate
C                                       plane center

         VMAX = -1.0
         ID = -20000
         IR = -20000
         DO 380 J = 0, ND-1
C                                       copy each delay to scratch
C                                       zero scratch array
            DO 340 I = NT, MT-1
               BPSCOR(1,1+I ) = 0.0
               BPSCOR(2,1+I ) = 0.0
 340           CONTINUE
            DO 350 I = 0,NT-1
               BPSCOR(1,1+I) = BPCORE(1,1+I,1+J)
               BPSCOR(2,1+I) = BPCORE(2,1+I,1+J)
 350           CONTINUE

C                                       Do rate FFT.
            P = MT
            CALL DOFFT (BPSCOR, P, WRK1, 1)
C                                       search from -NRM to NRP
            DO 370 I = 0, NR - 1
               K = MOD(MT + I - NRM, MT)
               AMPL = BPSCOR(1,1+K)**2 + BPSCOR(2,1+K)**2
               IF (AMPL.GT.VMAX) THEN
                  VMAX = AMPL
                  RWORK = BPSCOR(1,1+K)
                  IWORK = BPSCOR(2,1+K)
                  RPEAK = I
                  DPEAK = J
                  END IF
 370           CONTINUE
 380        CONTINUE
C                                       finished
         END IF
      RPEAK = RPEAK - NRM
      DPEAK = DPEAK - NDM
C
 999  RETURN
      END
      SUBROUTINE DOFFT (CARRAY, LEN, WORK, WHICH)
C-----------------------------------------------------------------------
      INTEGER   LEN, LSTLEN(2), WHICH
      REAL      CARRAY(2,LEN)
      INCLUDE 'INCS:PFFT.INC'
      REAL      WORK(*)
      COMMON /KRLAST/ LSTLEN
C-----------------------------------------------------------------------
      IF (LEN.NE.LSTLEN(WHICH)) THEN
         CALL CFFTI(LEN, WORK)
         LSTLEN(WHICH) = LEN
         END IF
      CALL CFFTB (LEN, CARRAY, WORK)
C
 999  RETURN
      END
C
      SUBROUTINE KLSF (VVIS, SOLVER, SOLVED, SOLVDD, BLCODE,
     *     MBL, MAN, NBL, NTIME, MTM,
     *     MFQ, NFQ, IFP, SWT, WTB,
     *     NSRLS, GDSGMA,
     *     CPHAZ, CRATE, TIMB,
     *     CDELAY, DFREQ, CDELBY, DDFREQ,
     *     CSGMA, REFAN, NT, DF, DT, PRTLEV, TIMEC, FREQ, IERR)
C-----------------------------------------------------------------------
C   KLSF does least squares solutions for delay and rate
C   The input values of CPHAZ,CDELAY, CRATE, and CDELBY are the initial
C   guesses.
C   Input:
C    MTM              I    Maximum number of time integrations.
C    MFQ              I    Maximum number of frequency channels.
C    MBL              I    Maximum number of baselines
C    MAN              I    Number of antennas
C    NBL              I    Number of baselines
C    NFQ              I    Number of frequencies
C    NT               I    Number of times
C    IFP              I    Frequency offset index
C    NSRLS            R    Minimum SNR allowed
C    DT               R    Time spacing
C    DF               D    Frequency spacing
C    BLCODE(MBL)      I    Location of baseline in array
C    NTIME(MBL)       I    Number of times on each baseline
C    VVIS(2,MTM,MFQ,MBL) R   Visibility array
C    TIMB(MTM,MBL)    R    Time wrt center
C    WTB(MBL)         R    Weight for each baseline
C    SWT(MAN)         R    Weight for each antenna
C    DFREQ(MFQ)       D    Frequency array
C    DDFREQ(MFQ)      D    incremental Frequency array
C    REFAN            I    Reference antenna
C    PRTLEV           I    Print level, prints results at .ge. 2
C   Input/Output:
C    CPHAZ(MAN)       R    Phase part of solution (degrees)
C    CDELAY(MAN)      R    delays in nano-seconds
C    CDELBY(MAN)      R    second delay term
C                             [ns of -dispersion*(VELITE/FREQ)^2]
C    CRATE(MAN)       R    Rates in mHz.
C    CSGMA(MAN)       R    Weights = SNR
C    REFAN            I    Reference antennas used
C   Output:
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'KRING.CONSTS'
      INTEGER MGP, IERR
      PARAMETER (MGP = 4*MAXANT)

      INTEGER MTM, MFQ, MBL, MAN, NBL, NFQ, IFP, REFAN,
     *     NT, PRTLEV, NGP
      REAL VVIS(2,MTM,MFQ,MBL), TIMB(MTM,MBL), SWT(MAN), WTB(MBL)
      REAL NSRLS, GDSGMA, DT
      INTEGER BLCODE(MBL), NTIME(MBL)
      DOUBLE PRECISION DFREQ(MFQ), DDFREQ(MFQ),
     *     CPHAZ(MAN), CDELAY(MAN), CDELBY(MAN),
     *     CRATE(MAN), CSGMA(MAN), FREQ
      DOUBLE PRECISION DF, REFT, TIMEC, REFDF, REFDDF
C
      INTEGER MXITER, I, QI, NPAR, ITER, NPOI, NFREE, J, NSING, NSOLV,
     *     INFO, QJ, L
      DOUBLE PRECISION CHITOL, STPTOL, GAMMA, SUMWGT, SUMSQW, OLDCHI,
     *     MAXD, DGOBA, NEWCHI, SNR, RMSRES, GFAC, BESTCH, BESTG,
     *     PHASE, DELAY, RATE, DELBY,
     *     EPHASE, ERATE, EDELAY, EDELBY,
     *     RPHASE, RRATE, RDELAY, RDELBY
      LOGICAL SOLVER, SOLVED, SOLVDD, GTDERV, GREAT, SOSO, TOOSML
      LOGICAL GOODST(MAXANT), SOLVEP(MAXANT), LOCCON(MGP)
      DOUBLE PRECISION NEWPAR(MGP), OLDPAR(MGP),
     *     GRAD(MGP), HESS(MGP,MGP), RIGHTT(MGP,MGP), LEFT(MGP,MGP),
     *     EPAR(MGP), WORK(10*MGP)
      INTEGER ITO(MGP), IFROM(MGP)
      CHARACTER LABSTR*5, REFSTR*1, PART1*10, PART2*8,
     *     PART3*8, PART4*12, PART5*12, PART6*9
      REAL ZVEC(8)
      DOUBLE PRECISION SNRPV, Z
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C---------------------------------------------------------------------
C      WRITE (*,*) 'in KLSF ! '
      NGP = 4*MAN
      IERR = 0
      REFT = TIMEC/1000.0
      J      = NFQ/2 + 1
C     REFDF  = DFREQ(IFP+J)/2.0 + ABS(DFREQ(IFP+1)+DFREQ(IFP+NFQ))/4.0
      REFDF  = DFREQ(J)/2.0 + ABS(DFREQ(1)+DFREQ(NFQ))/4.0
      J      = NFQ/2 + 1
      REFDDF= DDFREQ(IFP+J)/2.0+ABS(DDFREQ(IFP+1)+DDFREQ(IFP+NFQ))/4.0

C                                       Iteration info:
      MXITER = 40
      MXITER = 50
      STPTOL = 1.0D-2
      CHITOL = STPTOL
      GAMMA = 80.0D0
      GAMMA = 1.0D0
      IF (PRTLEV.GT.19) WRITE (*,*) 'CUTOF = ',CUTOF,REFT,REFDF,REFDDF
C
      DO 10 I = 1, MAN
         QI = 4*(I-1)
C                                       Identify antennas with good
C                                       data and with good initial
C                                       solutions
         GOODST(I) = (SWT(I).GT.VMIN) .AND. (CSGMA(I).GE.0.0D0)
C                                       Identify antennas to be
C                                       solved for
         SOLVEP(I) = .TRUE.
CKTEST         SOLVEP(I) = (GSOLVE(I).GT.0).AND.(I.NE.REFAN)
C                                       Load parameter solve vector
         LOCCON(QI+1) = GOODST(I) .AND. SOLVEP(I)
         LOCCON(QI+2) = GOODST(I) .AND. SOLVEP(I) .AND. SOLVER
         LOCCON(QI+3) = GOODST(I) .AND. SOLVEP(I) .AND. SOLVED
         LOCCON(QI+4) = GOODST(I) .AND. SOLVEP(I) .AND. SOLVDD
C                                       Load (good) incoming solutions
         IF (GOODST(I).AND.((CSGMA(I).LE.NSRLS).OR.(NSRLS.LT.0.)))
     *        THEN
            PHASE        = CPHAZ(I)
            RATE         = CRATE(I)
            DELAY        = CDELAY(I)
            DELBY        = CDELBY(I)
            PHASE = PHASE * TWOPI
            RATE  = RATE  * TWOPI
            DELAY = DELAY * TWOPI
            DELBY = DELBY * TWOPI
            PHASE = PHASE + RATE*REFT + DELAY*(REFDF-DFREQ(1)) +
     *         DELBY*REFDDF
            IF (PRTLEV.GE.20) WRITE (*,*) 'l  ',I,SWT(I),CSGMA(I)
         ELSE
            PHASE        = 0.0D0
            RATE         = 0.0D0
            DELAY        = 0.0D0
            DELBY        = 0.0D0
            IF (PRTLEV.GE.20) WRITE (*,*) 'nl ',I,SWT(I),CSGMA(I)
            END IF
         OLDPAR(QI+1) = PHASE
         OLDPAR(QI+2) = RATE
         OLDPAR(QI+3) = DELAY
         OLDPAR(QI+4) = DELBY
 10      CONTINUE
C                                       # parameters to be solved for
      NPAR = 0
      DO 30 I = 1, NGP
         IF (LOCCON(I)) NPAR = NPAR + 1
 30      CONTINUE
C                                       # parameters [to REFANT]
C                                       needing to be blanked
      QJ = 4*(REFAN-1)
      NSOLV = 0
      IF (LOCCON(QJ+1)) NSOLV = NSOLV + 1
      IF (LOCCON(QJ+2)) NSOLV = NSOLV + 1
      IF (LOCCON(QJ+3)) NSOLV = NSOLV + 1
      IF (LOCCON(QJ+4)) NSOLV = NSOLV + 1
C
      DO 590 ITER = 1, MXITER
         GAMMA = MAX ( GAMMA, 1.0D-12)
         GAMMA = MIN ( GAMMA, 1.0D12)
C                                       Init grad, hessian, chisq
         CALL DFILL (NGP, 0.0D0, GRAD)
         I = NGP*NGP
         CALL DFILL (I, 0.0D0, HESS)
         NPOI   = 0
         SUMWGT = 0.0D0
         SUMSQW = 0.0D0
         OLDCHI = 0.0D0
         GTDERV = .TRUE.
         CALL KGTMOD (MTM, MFQ, MBL, VVIS, WTB, NTIME, TIMB, DFREQ,
     *        DDFREQ, IFP, NFQ, NGP, OLDPAR, GOODST, NBL, MAN,
     *        BLCODE, OLDCHI, REFT, REFDF, REFDDF, LOCCON,
     *        GTDERV, GRAD, HESS, NPOI, SUMWGT, SUMSQW)
         IF ((NPOI.EQ.0) .OR. (2*NPOI.LT.NPAR)) THEN
            MSGTXT = 'No LS refinement because: insufficient data'
            CALL MSGWRT (6)
            IERR = 1
            GO TO 999
            END IF
C                                       Invert matrix
C                                       [ send in the # expected
C                                       singular parameters]
         NSING = NSOLV
         CALL MATINV (NGP, HESS, GRAD, LOCCON, NGP, RIGHTT, EPAR,
     *        LEFT, WORK, ITO, IFROM, GAMMA, CUTOF, NSING, INFO,
     *        PRTLEV)
         IF (INFO.GT.0) THEN
            MSGTXT = 'No LS refinement because: SVD did not converge'
            CALL MSGWRT (6)
            IERR = 2
            GO TO 999
            END IF
         NFREE = 2*NPOI + NSING - NPAR

         IF (NSING.GT.NSOLV) THEN
            MSGTXT = 'No LS refinement because: solutions singular'
            CALL MSGWRT (6)
            IERR = 3
         IF (PRTLEV.GT.19) WRITE (*,*) 'count=',NPOI,NPAR,NSING,NSOLV
            GO TO 999
            END IF
C                                       Construct/apply corrections
C                                       [get error & max fractional
C                                       deviation]

C                                     Find optimal step size
C                                     starting at LMM fiducial value
         GFAC = 1.0D0/(1.0D0+GAMMA)
         GFAC = 4.0
         BESTG = 10.0
         BESTCH = OLDCHI
         DO 330 J = 1,12
            GFAC = GFAC / 4.0
C                                     re-reference corrections
            DO 320 I = 1, MAN
               QI = 4*(I-1)
               DO 310 L = 1,4
                  NEWPAR(QI+L) = OLDPAR(QI+L)
     *                         - GFAC*(GRAD(QI+L)-GRAD(QJ+L))
 310              CONTINUE
 320           CONTINUE
            NEWCHI = 0.0D0
            GTDERV = .FALSE.
            CALL KGTMOD (MTM, MFQ, MBL, VVIS, WTB, NTIME, TIMB, DFREQ,
     *           DDFREQ, IFP, NFQ, NGP, NEWPAR, GOODST, NBL, MAN,
     *           BLCODE, NEWCHI, REFT, REFDF, REFDDF, LOCCON,
     *           GTDERV, GRAD, HESS, NPOI, SUMWGT, SUMSQW)
C                                     was it a great step?
            GREAT  =  BESTCH .GT. (1.2 * NEWCHI)
C                                     was it a so-so step?
            SOSO   =  BESTCH .GT. NEWCHI
C                                     was it too small a step?
            TOOSML = (BESTCH.LT.NEWCHI).AND.(NEWCHI.LT.OLDCHI)
            IF (TOOSML) GO TO 331
            IF (SOSO) THEN
               BESTCH = NEWCHI
               BESTG = GFAC
               END IF
            IF (GREAT) GO TO 332
 330        CONTINUE
 331     CONTINUE
         IF (BESTG.GT.5.0) GO TO 580
C                                     Use optimal step found:
C
C                                     re-reference corrections
         DO 350 I = 1, MAN
            QI = 4*(I-1)
            DO 340 L = 1,4
               NEWPAR(QI+L) = OLDPAR(QI+L)
     *                      - BESTG*(GRAD(QI+L)-GRAD(QJ+L))
 340           CONTINUE
 350        CONTINUE
C                                        Recompute Chi-sq
         NEWCHI = 0.0D0
         GTDERV = .FALSE.
         CALL KGTMOD (MTM, MFQ, MBL, VVIS, WTB, NTIME, TIMB, DFREQ,
     *        DDFREQ, IFP, NFQ, NGP, NEWPAR, GOODST, NBL, MAN,
     *        BLCODE, NEWCHI, REFT, REFDF, REFDDF, LOCCON,
     *        GTDERV, GRAD, HESS, NPOI, SUMWGT, SUMSQW)

 332     CONTINUE
         MAXD = 0.0D0
         DO 360 I = 1, NGP
C                                       Get error estimates and
C                                       estimate max par change
            EPAR(I)   = EPAR(I) * SQRT(BESTG * OLDCHI / NFREE)
            DGOBA = ABS(GRAD(I))/(EPAR(I)+1.0D-8)
            MAXD = MAX (MAXD, DGOBA)
            IF ((PRTLEV.GT.20).AND.(LOCCON(I)))
     *     WRITE (*,'(I3,5(1X,F30.15))') I,OLDPAR(I),GRAD(I),EPAR(I)
 360     CONTINUE
         IF (PRTLEV.GT.15)
     *   WRITE (*,*) 'o=',J,ITER,GAMMA,OLDCHI,NEWCHI,CHITOL,MAXD,BESTG
C
 580     CONTINUE
         IF (NEWCHI.GE.OLDCHI) THEN
C                                        Shrink bad steps and go again
            NEWCHI = OLDCHI
            CALL DCOPY (NGP, OLDPAR, 1, NEWPAR, 1)
            GAMMA = GAMMA * 4.0
         ELSE
C                                     Shrink good but small steps
C                                     expand good large steps
            IF (BESTG.LT.1.0E-5) THEN
               GAMMA = GAMMA * 16.0
            ELSE
               GAMMA = GAMMA / 4.0
               END IF
            CALL DCOPY (NGP, NEWPAR, 1, OLDPAR, 1)
C                                     if chi-squared drops by < 1% and parameters
C                                     didnt change, stop now
            IF ((MAXD.LT.STPTOL).AND.
     *          ((OLDCHI-NEWCHI).LE.(OLDCHI*CHITOL))) THEN
               OLDCHI = NEWCHI
               GO TO 600
               END IF
C                                     If chi-squared drops by <0.01%, stop now
            IF ((OLDCHI-NEWCHI).LE.(OLDCHI*CHITOL*CHITOL)) THEN
               OLDCHI = NEWCHI
               GO TO 600
               END IF
            OLDCHI = NEWCHI
            END IF
C
 590     CONTINUE
C                                       Finished Ls solution
 600  CONTINUE
C                                       Form rms residual
      RMSRES = SQRT(OLDCHI*NPOI/(NFREE*SUMWGT))
C                                       Extract REFANT solution
      QI = 4*(REFAN-1)
      RPHASE = OLDPAR(QI+1)
      RRATE  = OLDPAR(QI+2)
      RDELAY = OLDPAR(QI+3)
      RDELBY = OLDPAR(QI+4)
      DO 710 I = 1, MAN
         QI = 4*(I-1)
         IF (GOODST(I)) THEN
C                                       copy internal to external
            PHASE  = (OLDPAR(QI+1) - RPHASE)
            RATE   = (OLDPAR(QI+2) - RRATE)
            DELAY  = (OLDPAR(QI+3) - RDELAY)
            DELBY  = (OLDPAR(QI+4) - RDELBY)
C                                       Prep for SNR calculations
            ZVEC(1) = OLDCHI
            ZVEC(2) = SWT(I)
            ZVEC(3) = SUMWGT
            ZVEC(4) = NPOI
C                                       FRING calc
            Z = MAX(1.0D-20, EPAR(QI+1))
            ZVEC(5) = 1.0 / Z
C                                       Memo 101 [X_r not X_a] calc
            Z = (1.0 - OLDCHI / (2.0*SWT(I)))
            Z = (1.0 - OLDCHI / (2.0*SUMWGT))
            ZVEC(6) = SNRPV(Z) * SQRT(NPOI * SWT(I)/SUMWGT)
C                                       Hi SNR
            Z = SQRT(2.0*SUMWGT) / SQRT(OLDCHI)
            Z = SQRT(    SUMWGT) / SQRT(OLDCHI)
            ZVEC(7) = Z * SQRT(NPOI * SWT(I)/SUMWGT)
C                                       Low SNR
            Z = 1.0 - OLDCHI / (2.0*SUMWGT)
            Z = 1.0 - OLDCHI / (    SUMWGT)
            Z = SQRT(8.0/PI) * Z
            ZVEC(8) = Z * SQRT(NPOI * SWT(I)/SUMWGT)
C
            IF (PRTLEV.GE.11) WRITE (*,3000) I,NSING, IFP, ZVEC
C
            SNR = ZVEC(6)
            IF (.NOT.SOLVEP(I)) SNR = GDSGMA
            PHASE = PHASE - RATE*REFT - DELAY*REFDF - DELBY*REFDDF
            RATE  = RATE  / TWOPI
            DELAY = DELAY / TWOPI
            DELBY = DELBY / TWOPI
            PHASE = PHASE / TWOPI
C                                       ! PHASE = 0.0 if SOLVEP = F
            CPHAZ(I)  = PHASE
C                                       ! RATE = 0.0 if SOLVER = F
            CRATE(I)  = RATE
C                                       ! DELAY = 0.0 if SOLVED = F
            CDELAY(I) = DELAY
C                                       ! DELBY = 0.0 if SOLVDD = F
            CDELBY(I) = DELBY
            CSGMA(I)  = 1.0/SNR
            IF (PRTLEV.GE.1) THEN
C                                       Send results to screen
               PHASE = MOD (PHASE, 1.0D0)
               PHASE = 360.0 * MOD (PHASE+1.5D0,1.0D0) - 180.0D0
               IF (I.LT.REFAN) THEN
                  PHASE  = -PHASE
                  DELAY  = -DELAY
                  DELBY  = -DELBY
                  END IF
               LABSTR = 'LSq: '
               REFSTR = ' '
               IF (I.EQ.REFAN) REFSTR = '*'
               PART2 = ' '
               PART3 = ' '
               PART4 = ' '
               PART5 = ' '
               PART6 = 'Infinite!'
               EDELBY = -DELBY * (FREQ / VELITE)**2
               WRITE (PART1, 2010) LABSTR, I, REFSTR
               IF (SOLVEP(I)) WRITE (PART2, '(F8.1)')  PHASE
               IF (SOLVER)    WRITE (PART3, '(F8.1)')  RATE
               IF (SOLVED)    WRITE (PART4, '(F12.3)') DELAY
               IF (SOLVDD)    WRITE (PART5, '(F12.3)') EDELBY
               IF (SNR.LE.999999.99) WRITE (PART6,'(F9.2)') SNR
               WRITE (MSGTXT, 2020) PART1, PART2,
     *              PART3, PART4, PART5, PART6
               IF (PRTLEV.NE.10) CALL MSGWRT (5)
               END IF
            END IF
 710     CONTINUE
C                                       Show error bars
      IF (PRTLEV.GE.2) THEN
         DO 730 I = 1, MAN
            QI = 4*(I-1)
            IF (GOODST(I)) THEN
               EPHASE  = EPAR(QI+1) * 360.0
               ERATE   = EPAR(QI+2) * DT * SQRT(NT/3.0) * TWOPI
               EDELAY  = EPAR(QI+3) * DF * SQRT(NFQ/3.0) * TWOPI
               EDELBY  = EPAR(QI+4) * DF * SQRT(NFQ/3.0) * TWOPI
               EDELBY = EDELBY * (FREQ / VELITE)**2
               LABSTR = 'err: '
               REFSTR = ' '
               IF (I.EQ.REFAN) REFSTR = '*'
               PART2 = ' '
               PART3 = ' '
               PART4 = ' '
               PART5 = ' '
               WRITE (PART1, 2010) LABSTR, I, REFSTR
               IF (SOLVEP(I)) WRITE (PART2, '(F8.2)')  EPHASE
               IF (SOLVER)    WRITE (PART3, '(F8.2)')  ERATE
               IF (SOLVED)    WRITE (PART4, '(F12.4)') EDELAY
               IF (SOLVDD)    WRITE (PART5, '(F12.4)') EDELBY
               WRITE (MSGTXT, 2020)  PART1, PART2,
     *              PART3, PART4, PART5
               IF (PRTLEV.NE.10) CALL MSGWRT (3)
               END IF
 730        CONTINUE
         WRITE (MSGTXT, 2550) RMSRES
         IF (PRTLEV.NE.10) CALL MSGWRT (2)
         END IF
 999  RETURN
C-----------------------------------------------------------------------
 2010 FORMAT (A5,'[', I2.2,A1,']')
 2020 FORMAT (A10, A8, A8, A12, A12, A9,2X)
 2550 FORMAT ('                 RMS residual =', F10.5)
 3000 FORMAT ('LS  ',3(I3,1X),10(F20.5,1X))
      END
      SUBROUTINE MATINV (M, ARR, VEC, KCON, L, RIGHTT, SIGMA, LEFT,
     *   WORK, ITO, IFROM, GAMMA, CUTOF, NSING, INFO, PRTLEV)
C-----------------------------------------------------------------------
C   MATINV inverts the matrix given in ARR.
C   MATINV also repacks ARR and VEC to zero those rows/columns not being
C     solved for
C   Input:
C      M       I      Dimension of ARR, VEC, KCON
C      ARR     R(M,M) Matrix to be packed, inverted, and unpacked
C      VEC     R(M)   Solution vector to be packed, and unpacked
C      KCON    L(M)   Vary flag controlling packing, unpacking
C      L       I      Dimension of ZDEX, RIGHTT, LEFT
C   Output:
C      SIGMA    R(L)   contains error bars
C      RIGHTT   R(L,L) work matrix
C      LEFT   R(L,L) secondary work matrix
C      CUTOF  R      SVD condition number cutoff
C      NSING   I      number of singular values being thrown away
C-----------------------------------------------------------------------
C                                       i/o variables
      INTEGER M, L
      DOUBLE PRECISION VEC(M)
      DOUBLE PRECISION ARR(M,M), RIGHTT(L,L), SIGMA(L), LEFT(L,L),
     *   WORK(10*L)
      LOGICAL KCON(M)
      INTEGER ITO(L), IFROM(L)
      INTEGER NSING, LWORK, INFO, NEXP, PRTLEV
      DOUBLE PRECISION CUTOF, SMIN, GAMMA
C                                       no includes
C                                       internal variables
      INTEGER N, I, J, K, Q, R
C-----------------------------------------------------------------------
C                                       Save #expected singular values
      NEXP = NSING
C                                       prep array and vector
      DO 130 I = 1, M
         IF (KCON(I)) THEN
            DO 110 J = I+1,M
               ARR(I,J) = ARR(I,J) / ( 1.0D0 + GAMMA)
               ARR(J,I) = ARR(I,J)
 110           CONTINUE
         ELSE
            VEC(I) = 0.0D0
            DO 120 J = 1,M
               ARR(I,J) = 0.0D0
               ARR(J,I) = 0.0D0
 120           CONTINUE
            END IF
 130     CONTINUE
C                                       save some time, find largest
      K = 0
      CALL FILL (M, 0, ITO)
      CALL FILL (M, 0, IFROM)
      DO 210 I = 1,M
         IF (KCON(I)) THEN
            K = K + 1
            ITO(I) = K
            IFROM(K) = I
            END IF
 210      CONTINUE
C                                       Compress matrix and vector
C                                       Rescale off-diagonal elements
      DO 230 J = 1,K
         DO 220 I = 1,K
            ARR(I,J) = ARR(IFROM(I),IFROM(J))
 220         CONTINUE
         ARR(J,J) = ARR(J,J)
         VEC(J) = VEC(IFROM(J))
 230      CONTINUE
      N = K
C                                       Invert matrix
      IF (N.EQ.1) THEN
         RIGHTT(1,1) = 1.0
         SIGMA(1)    = ARR(1,1)
         LEFT(1,1)   = 1.0
      ELSE IF (N.GE.2) THEN
         LWORK = 10*L
         INFO = 0
         CALL DGESVD ('A', 'A', N, N, ARR, M,
     *     SIGMA, LEFT, L, RIGHTT, L, WORK, LWORK, INFO)
         END IF
C                                       Zero expected singular parameters
      DO 330 J = 0,NEXP-1
         IF (PRTLEV.GE.19) WRITE (*,*) 'editing values ',
     *                                N-J,SIGMA(N-J),N,J
         SIGMA(N-J) = 0.0D0
 330     CONTINUE
C                                       Invert singular values, removing
C                                       some
      SMIN = SIGMA(1) * CUTOF * (1.0D0 + GAMMA)
      NSING = 0
      DO 340 J = 1,N
         IF (PRTLEV.GE.19) WRITE (*,*) 'all values',J,SIGMA(J),GAMMA
         SIGMA(J) = SIGMA(J) * (1.0D0 + GAMMA)
         IF (SIGMA(J).GT.SMIN) THEN
            SIGMA(J) = 1.0D0/SIGMA(J)
         ELSE
            SIGMA(J) = 0.0D0
            NSING = NSING + 1
            END IF
 340      CONTINUE
C                                       assemble inverted matrix
      R = N - NSING
      DO 370 K = 1,N
         DO 360 J = 1,N
            ARR(J,K) = 0.0
            DO 350 Q = 1,R
               ARR(J,K) = ARR(J,K) + RIGHTT(Q,J)*SIGMA(Q)*LEFT(K,Q)
 350            CONTINUE
 360         CONTINUE
         ARR(K,K) = MAX(ARR(K,K),0.0D0)
 370     CONTINUE

      DO 410 K = 1,N
         SIGMA(K) = 0.0D0
         DO 400 J = 1,N
            SIGMA(K) = SIGMA(K) + ARR(K,J) * VEC(J)
 400        CONTINUE
 410     CONTINUE
C                                       send out solution and its error bars
      DO 590 I = M,1,-1
         IF (ITO(I).NE.0) THEN
            VEC(I)   = SIGMA(ITO(I))
            SIGMA(I) = ARR(ITO(I),ITO(I))
            SIGMA(I) = SQRT(ABS(SIGMA(I)))
         ELSE
            VEC(I)   = 0.0D0
            SIGMA(I) = 0.0D0
            END IF
 590     CONTINUE
      RETURN
C---------------------------------------------------------------------
      END
      SUBROUTINE KGTMOD (MTM, MFQ, MBL, VVIS, WTB, NTIME, TIMB,
     *     DFREQ, DDFREQ, IFP, NFQ, MGP, PAR, GOODST, NBL, MAN,
     *     BLCODE, SUMSQR, REFT, REFDF, REFDDF, LOCCON,
     *     GTDERV, GRAD, HESS, SUMPOI, SUMWTS, SUMSQW)
C---------------------------------------------------------------------
C---------------------------------------------------------------------
      INCLUDE 'KRING.CONSTS'
      INTEGER MTM, MFQ, MBL, IFP, NFQ, MGP, NBL, MAN, SUMPOI
      REAL VVIS(2,MTM,MFQ,MBL), WTB(MBL), TIMB(MTM,MBL)
      DOUBLE PRECISION DFREQ(MFQ), DDFREQ(MFQ)
      DOUBLE PRECISION PAR(MGP), GRAD(MGP), HESS(MGP,MGP)
      DOUBLE PRECISION SUMSQR, SUMWTS, SUMSQW, REFT, REFDF, REFDDF
      INTEGER NTIME(MBL), BLCODE(MBL)
      LOGICAL GOODST(MAN), GTDERV, LOCCON(MGP)
C
      INTEGER Q, QI, QJ, QQI, QQJ, J, K
      DOUBLE PRECISION PHASE, RATE, DELAY, DELBY, XT, XTRESR, XTRESI
      DOUBLE PRECISION G(4), GI(4), GJ(4), XTDER1, XTDER2
      INTEGER  L, N
C---------------------------------------------------------------------
C                                       Assemble Chisq, gradient and hessian
      DO 199 Q = 1, NBL
C                                       Get good baseline and deconstruct it
         IF (WTB(Q).LE.VMIN) GO TO 199
         QI = INT ( BLCODE(Q) / (MAN+1) )
         QJ = MOD ( BLCODE(Q) , (MAN+1) )
         QQI = 4*(QI-1)
         QQJ = 4*(QJ-1)
C                                       Both ends must be good.
         IF ((.NOT.GOODST(QI)).OR.(.NOT.(GOODST(QJ)))) GO TO 199
C                                       Assemble model and its pieces
         PHASE = PAR(QQI+1) - PAR(QQJ+1)
         RATE  = PAR(QQI+2) - PAR(QQJ+2)
         DELAY = PAR(QQI+3) - PAR(QQJ+3)
         DELBY = PAR(QQI+4) - PAR(QQJ+4)
C                                       Loop over freqs and times
         DO 190 J = IFP+1, IFP+NFQ
            DO 180 K = 1, NTIME(Q)
C                                       Check amplitudes
               IF ((ABS(VVIS(1,K,J,Q))+ABS(VVIS(2,K,J,Q))).LT.VMIN)
     *             GO TO 180
               G(1) = 1.0D0
               G(2) = TIMB(K,Q) - REFT
               G(3) = DFREQ(J)  - REFDF
               G(4) = DDFREQ(J) - REFDDF
               XT = PHASE * G(1) + RATE  * G(2)
     *            + DELAY * G(3) + DELBY * G(4)
C                                       Assemble residual
               XTRESR = COS(XT) - VVIS(1,K,J,Q)
               XTRESI = SIN(XT) - VVIS(2,K,J,Q)
               SUMSQR = SUMSQR+ (XTRESR*XTRESR + XTRESI*XTRESI)*WTB(Q)
C                                       Get derivatives?
               IF (GTDERV) THEN
C                                       Get some sums
                  SUMPOI = SUMPOI+ 1
                  SUMWTS = SUMWTS+ WTB(Q)
                  SUMSQW = SUMSQW+ WTB(Q)**2
C                                       Assemble nubs of complex derivatives
                  XTDER1 = VVIS(1,K,J,Q)*SIN(XT)-VVIS(2,K,J,Q)*COS(XT)
                  XTDER2 = VVIS(1,K,J,Q)*COS(XT)+VVIS(2,K,J,Q)*SIN(XT)
                  XTDER1 = XTDER1 * WTB(Q)
                  XTDER2 = XTDER2 * WTB(Q)
C                                       compute Chi-sq, gradient and hessian
                  DO 130 L = 1,4
                     GI(L) = 0.0
                     GJ(L) = 0.0
                     IF (LOCCON(QQI+L)) GI(L) = G(L)
                     IF (LOCCON(QQJ+L)) GJ(L) = -G(L)
 130                 CONTINUE
                  DO 150 L = 1,4
                     GRAD(QQI+L) = GRAD(QQI+L) + GI(L)*XTDER1
                     GRAD(QQJ+L) = GRAD(QQJ+L) + GJ(L)*XTDER1
                     DO 140 N = 1,4
                        HESS(QQI+L,QQI+N) = HESS(QQI+L,QQI+N)
     *                                    + GI(L)*GI(N)*XTDER2
                        HESS(QQI+L,QQJ+N) = HESS(QQI+L,QQJ+N)
     *                                    + GI(L)*GJ(N)*XTDER2
                        HESS(QQJ+L,QQI+N) = HESS(QQJ+L,QQI+N)
     *                                    + GJ(L)*GI(N)*XTDER2
                        HESS(QQJ+L,QQJ+N) = HESS(QQJ+L,QQJ+N)
     *                                    + GJ(L)*GJ(N)*XTDER2
 140                    CONTINUE
 150                 CONTINUE
                  END IF
 180           CONTINUE
 190        CONTINUE
 199     CONTINUE
      RETURN
      END
C
      LOGICAL FUNCTION NOSRCH (SRC, DOCWNT, NCALWD, CALWAN)
C-----------------------------------------------------------------------
C   NOSRCH looks through the list of calibrator sources to determine if
C   the source is wanted.
C  Input:
C    SRC     I       source number
C    DOCWNT  L       wanted flag from selection system [SOUFIL]
C    NCALWD  I       # cal sources in list
C    CALWAN  I(*)    List of source numbers to accept
C-----------------------------------------------------------------------
      INTEGER SRC, NCALWD, CALWAN(*), I
      LOGICAL DOCWNT
C-----------------------------------------------------------------------
      NOSRCH = .NOT.DOCWNT
      DO 100 I = 1, NCALWD
         IF (CALWAN(I).EQ.SRC) NOSRCH = DOCWNT
  100    CONTINUE
      IF (NCALWD.EQ.0) NOSRCH = DOCWNT
      NOSRCH = .NOT.NOSRCH
C
      RETURN
      END
C
      SUBROUTINE CALZER (KOLS, BUFFER, ZPHS, ZDEL, ZRAT, IRET)
C-----------------------------------------------------------------------
C   CALZER zeros the RATE, DELAY and/or PHASE solutions upon request
C   CALZER will loop over all records in the SN table and zero the
C   rates, delays, and/or phases upon request.
C        The table should already be open and BUFFER should be the
C   buffer used by TABINI (or other table opening routines).
C   [This code was taken from CALREF]
C   Input:
C    KOLS(9)         I    Array of TABIO column pointers in order:
C                         antenna, ref. antenna, subarray, weight, time,
C                         real, imag, delay, rate.
C    BUFFER(*)       I    Buffer for TABIO use; table must already be
C                         open
C    ZRAT            L    Zero RATEs ?
C    ZDEL            L    Zero DELAYs ?
C    ZPHS            L    Zero PHASEs ?
C   Output:
C    IRET            I    Return code 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER KOLS(9), BUFFER(*), IRET
      LOGICAL ZPHS, ZDEL, ZRAT
C
      INTEGER   IRCODE, RECORD(1024), LKOLS(9),
     *   ANTKOL, REFKOL, SUBKOL, WTKOL, TIMKOL, REKOL, IMKOL, DELKOL,
     *   RATKOL
      INTEGER   NUMREC, LOOPR
      REAL      RECR(1024), TRE, TIM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECORD, RECR)
      EQUIVALENCE (LKOLS(1), ANTKOL), (LKOLS(2), REFKOL),
     *   (LKOLS(3), SUBKOL), (LKOLS(4), WTKOL), (LKOLS(5), TIMKOL),
     *   (LKOLS(6), REKOL), (LKOLS(7), IMKOL),
     *   (LKOLS(8), DELKOL), (LKOLS(9), RATKOL)
      DATA IRCODE /0/
C-----------------------------------------------------------------------
      IRET = 0
C                                       is there anything to do?
      IF (.NOT.(ZPHS.OR.ZDEL.OR.ZRAT)) GO TO 999
C                                       get column pointers
      CALL COPY (9, KOLS, LKOLS)
C                                       Get number of records in table
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
C                                       Loop thru table changing any
C                                       solns to zero if so requested
      DO 200 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.GT.0) GO TO 900
C                                       See if wanted.
         IF ((RECR(WTKOL).GT.0.0).AND.(RECR(REKOL).NE.FBLANK).AND.
     *       (IRET.EQ.0))  THEN
C                                       Phase
            IF (ZPHS) THEN
               TRE = RECR(REKOL)
               TIM = RECR(IMKOL)
               TRE = TRE*TRE + TIM*TIM
               RECR(REKOL) = SQRT(TRE)
               RECR(IMKOL) = 0.0
               END IF
C                                       Delay
            IF (ZDEL) RECR(DELKOL) = 0.0
C                                       Rate
            IF (ZRAT) RECR(RATKOL) = 0.0
C                                       MBdelay
C                                       Rewrite record
            CALL TABIO ('WRIT', IRCODE, LOOPR, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
            END IF
 200     CONTINUE
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET, LOOPR
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('CALZER: TABIO ERROR',I3,' on record ',I5)
      END
C
      SUBROUTINE SNGET (TIMEC, MAN, MIF, FREQIF,
     *   SNKOLS, SNNUMV, IIF, IST, GOTSLN,
     *   CMBDEL, CDISP, CPHAZ, CDELY, CRATE, CSGMA,
     *   FRREAL, FRIMAG, FRDELY, FRRATE, FRWGHT, IERR)
C-----------------------------------------------------------------------
C   SNGET reads a set of SN table entries and initializes the solution
C   arrays.
C   Input:
C      MIF      I                 Maximum number of IFs
C      FREQIF   D(*)              IF frequency offsets (Hz)
C      SNKOLS   I(*)              SN table column pointers
C      SNNUMV   I(*)              SN table element counts.
C      CMBDEL   R(NUMANT,2)       Multiband delays in nano-seconds.
C      CDISP    R(NUMANT,2)       Dispersion in nano-s/m/m
C      CPHAZ    R(2,MIF,NUMANT)   Phase part of solution
C      CDELY    R(2,MIF,NUMANT)   delays in seconds.
C      CRATE    R(2,MIF,NUMANT)   Rates in Hz.
C      CSGMA    R(2,MIF,NUMANT)   Weights = SNR
C   Input on common:
C      NUMANT   I                 The number of antennas.
C      MIF      I                 The number of IFs.
C      NUMPOL   I                 The number of polarizations.
C   Output:
C      IERR     I                 Return code, 0=>OK, else TABSN error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION TIMEC, TIMED, TIME2
      INTEGER   RECSOU, NODENO, RECSUB, SNKOLS(*), SNNUMV(*), IERR, IIF,
     *   IST, MAN, GOTSLN(MAN)
C
      INTEGER MIF, REF(2,MAXIF), I, J, SNEREC, SNREC
      REAL DELTA, IFRM
      DOUBLE PRECISION CPHAZ(MAN), CDELY(MAN), CRATE(MAN), CSGMA(MAN),
     *   CMBDEL(MAN), CDISP(MAN)
      REAL FRREAL(2,MIF), FRIMAG(2,MIF), FRDELY(2,MIF), FRRATE(2,MIF),
     *   FRWGHT(2,MIF), FRMBDL(2), FRDISP(2), FRDDSP(2)
      DOUBLE PRECISION FREQIF(MIF), SNWT
C
      INTEGER   IANT, LUNSN
      LOGICAL   ISAPPL
      INCLUDE 'KRING.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      LUNSN = 27
      TIMED = TIMEC/86400.0
      NODENO = 0
C                                       Open table for write,
C                                       determine number of rows
      SNREC = 0
      ISAPPL = .FALSE.
      CALL SNINI ('WRIT', CLBUFF, DISKIN, CNOIN, SNVER, INCAT, LUNSN,
     *   SNREC, SNKOLS, SNNUMV, NUMANT, NUMPOL, MIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) GO TO 999
      SNEREC = SNREC - 1
      CALL TABIO ('CLOS', 0, SNREC, TIME2, CLBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1000) IERR, 'finding max row no of SN table'
         CALL MSGWRT (6)
         GO TO 999
         END IF

C                                        Now read through the records
      ISAPPL = .FALSE.
      CALL SNINI ('READ', CLBUFF, DISKIN, CNOIN, SNVER, INCAT, LUNSN,
     *   SNREC, SNKOLS, SNNUMV, NUMANT, NUMPOL, MIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1000) IERR, 'initializing SN table for read'
         CALL MSGWRT (6)
         GO TO 999
         END IF

      DO 200 J = 1, SNEREC
C                                       read from SN table
         SNREC = J
         CALL TABSN ('READ', CLBUFF, SNREC, SNKOLS, SNNUMV, NUMPOL,
     *      TIME2, DELTA, RECSOU, IANT, RECSUB, FRQSEL, IFRM, NODENO,
     *      FRMBDL, FRDISP, FRDDSP, FRREAL, FRIMAG, FRDELY, FRRATE,
     *      FRWGHT, REF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT, 1000) IERR, 'reading SN table'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       Require REFANT to match
         IF (REF(IST,IIF).NE.REFANT) GO TO 200
C                                       Require good weight
         IF (FRWGHT(IST,IIF).LE.0.0D0) GO TO 200

C                                       Found a good solution, keep it.
C                                       [do we want to weight by
C                                        (TIMED-TIME2) ?]
         SNWT = FRWGHT(IST,IIF)
         CSGMA(IANT) = CSGMA(IANT) + SNWT*SNWT
         CMBDEL(IANT) = CMBDEL(IANT) + FRMBDL(IST)*SNWT
         CDISP(IANT) = CDISP(IANT) + FRDISP(IST)*SNWT
         CDELY(IANT) = CDELY(IANT) + FRDELY(IST,IIF)*SNWT
         CRATE(IANT) = CRATE(IANT) + FRRATE(IST,IIF)*SNWT
         CPHAZ(IANT) = CPHAZ(IANT) + SNWT
 200  CONTINUE

      DO 300 I = 1, MAN
         IF (CSGMA(I).GT.0.0) THEN
            SNWT = CPHAZ(I)
C                                       [set the weight <0 to find new phase]
C                                        in KFFT]
            CSGMA(I)  = -SNWT / CSGMA(I)
            CMBDEL(I) = CMBDEL(I) * 1.0D9 / SNWT
            CDISP(I)  = CDISP(I) * 1.0D9 / SNWT
            CDELY(I)  = CDELY(I) * 1.0D9 / SNWT
            CRATE(I)  = CRATE(I) * 1000.0 * (FREQ+FREQIF(IIF))
     *                                   / SNWT
            CPHAZ(I)  = 0.0D0
            GOTSLN(I) = REFANT
            END IF
 300     CONTINUE

      CALL TABIO ('CLOS', 0, SNREC, TIME2, CLBUFF, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'SNGET Error closing table'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SNGET Error ',I3,1X,A50)
      END
      SUBROUTINE SNPUT (TIMEC, DELT, SCNSOU, NODENO, SCNSUB, MAN,
     *   MIF, FREQIF, GOTANT, SNROW, SNKOLS, SNNUMV, CNTOK, CNTBAD,
     *   CMBDEL, CDISP, CPHAZ, CDELY, CRATE, CSGMA, REFAN, IERR,
     *   FRREAL, FRIMAG, FRDELY, FRRATE, FRWGHT)
C-----------------------------------------------------------------------
C   SNPUT prepares a set of SN table entries and writes them to an SN
C   table.
C   Input:
C      TIMEC    D     Time in days
C      DELT     R     Solution interval in days
C      SCNSOU   I     Source number
C      NODENO   I     Node numbe.
C      SCNSUB   I     Subarray number
C      MIF      I     Maximum number of IFs
C      FREQIF   D(*)  IF frequency offsets (Hz)
C      GOTANT   L(*)  Flags indiINCATg if there was data for each ant.
C      ISNRNO   I     TABSN counter.
C      SNKOLS   I(*)  SN table column pointers
C      SNNUMV   I(*)  SN table element counts.
C      CMBDEL   R(NUMANT,2)  Multiband delays in seconds.
C      CPHAZ    R(2,MIF,NUMANT) Phase part of solution
C      CDELY    R(2,MIF,NUMANT) delays in seconds.
C      CRATE    R(2,MIF,NUMANT) Rates in Hz.
C      CSGMA    R(2,MIF,NUMANT) Weights = SNR
C      REFAN    I(2,MIF) Reference antennas used
C   Input on common:
C      NUMANT   I     The number of antennas.
C      MIF      I     The number of IFs.
C      NUMPOL   I     The number of polarizations.
C      NSRLS    R     Minimum allowed SNR for a solution.
C   Input/output in common:
C      REFUSE   I(*)  The number of useages of each antenna as reference
C                     antenna.
C   Input/Output:
C      CNTOK    I     Count of good solutions
C      CNTBAD   I     Count of failed solutions.
C   Output:
C      IERR     I     Return code, 0=>OK, else TABSN error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION TIMEC, TIMED
      INTEGER   SCNSOU, NODENO, SCNSUB, ISNRNO, SNKOLS(*),
     *   SNNUMV(*), CNTOK, CNTBAD, ST, IERR, SNROW
C
      INTEGER MAN, MIF, REFAN(2,MIF)
      REAL DELT, IFRM
      DOUBLE PRECISION CPHAZ(MAN,MIF,2), CDELY(MAN,MIF,2),
     *   CRATE(MAN,MIF,2), CSGMA(MAN,MIF,2), CMBDEL(MAN,2), CDISP(MAN,2)
      LOGICAL   GOTANT(MAN)
      REAL FRREAL(2,MIF), FRIMAG(2,MIF), FRDELY(2,MIF), FRRATE(2,MIF),
     *   FRWGHT(2,MIF), FRMBDL(2), FRDISP(2), FRDDSP(2)
      DOUBLE PRECISION FREQIF(MIF)
C
      INTEGER   IANT, IIF, LUNSN
      LOGICAL   ISAPPL
      INCLUDE 'KRING.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      LUNSN = 27
C                                       Open table
      ISAPPL = .FALSE.
      CALL SNINI ('WRIT', CLBUFF, DISKIN, CNOIN, SNVER, INCAT, LUNSN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, MIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       start at the first requested row
C                                       [ if SNROW not zero ]
      IF (SNROW.EQ.0) THEN
         SNROW = ISNRNO
         GO TO 990
         END IF
      IF (SNROW.NE.0) ISNRNO = SNROW
C                                       Loop over antennae
      DO 420 IANT = 1,NUMANT
         DO 410 ST = 1,2
            IF (CMBDEL(IANT,ST).NE.FBLANK) THEN
               FRMBDL(ST) = CMBDEL(IANT,ST) / 1.0D9
            ELSE
               FRMBDL(ST) = 0.0
               END IF
            IF (CDISP(IANT,ST).NE.FBLANK) THEN
               FRDISP(ST) = CDISP(IANT,ST) / 1.0D9
               FRDDSP(ST) = 0.0
            ELSE
               FRDISP(ST) = 0.0
               FRDDSP(ST) = 0.0
               END IF
            DO 400 IIF = 1,MIF
               FRREAL(ST,IIF) = COS(TWOPI*CPHAZ(IANT,IIF,ST))
               FRIMAG(ST,IIF) = SIN(TWOPI*CPHAZ(IANT,IIF,ST))
               IF (CRATE(IANT,IIF,ST).NE.FBLANK) THEN
                  FRRATE(ST,IIF) = (CRATE(IANT,IIF,ST)/1000.0D0 ) /
     *                 (FREQ + FREQIF(IIF))
               ELSE
                  FRRATE(ST,IIF) = FBLANK
                  END IF
               IF (CDELY(IANT,IIF,ST).NE.FBLANK) THEN
                  FRDELY(ST,IIF) = CDELY(IANT,IIF,ST) / 1.0D9
               ELSE
                  FRDELY(ST,IIF) = FBLANK
                  END IF
               IF ((REFAN(ST,IIF).GT.0).AND.
     *             (REFAN(ST,IIF).LE.NUMANT)) THEN
                  REFUSE(REFAN(ST,IIF)) = REFUSE(REFAN(ST,IIF)) + 1
                  REFUSS(REFAN(ST,IIF),SCNSUB) =
     *               REFUSS(REFAN(ST,IIF),SCNSUB) + 1
                  END IF
               IF ((CSGMA(IANT,IIF,ST).GT.0.0).AND.
     *            ((CSGMA(IANT,IIF,ST).LE.NSRLS).OR.(NSRLS.LT.0.)) )
     *               THEN
                  FRWGHT(ST,IIF) = 1./CSGMA(IANT,IIF,ST)
                  IF (GOTANT(IANT).AND.
     *               ((ST.EQ.1).OR.(NUMPOL.GT.1))) CNTOK = CNTOK + 1
               ELSE
                  FRWGHT(ST,IIF) = 0.0
                  FRREAL(ST,IIF) = FBLANK
                  FRIMAG(ST,IIF) = FBLANK
                  FRDELY(ST,IIF) = FBLANK
                  IF (GOTANT(IANT).AND.
     *               ((ST.EQ.1).OR.(NUMPOL.GT.1))) CNTBAD = CNTBAD + 1
                  END IF
 400           CONTINUE
 410        CONTINUE
         IF (GOTANT(IANT)) THEN
            IFRM = 0.0
            TIMED = TIMEC/86400.0
            CALL TABSN ('WRIT', CLBUFF, ISNRNO, SNKOLS, SNNUMV,
     *         NUMPOL, TIMED, DELT, SCNSOU, IANT, SCNSUB, FRQSEL,IFRM,
     *         NODENO, FRMBDL, FRDISP, FRDDSP, FRREAL, FRIMAG, FRDELY,
     *         FRRATE, FRWGHT, REFAN, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 420     CONTINUE
C                                       save next solution number
      SNROW = ISNRNO
 990  CONTINUE
      CALL TABIO ('CLOS', 0, ISNRNO, TIMED, CLBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE KRNADJ (SUBBEG, SUBEND, MIF, IRET)
C-----------------------------------------------------------------------
C   KRNADJ massages the solutions so that interpolation between points
C   is reasonable.
C   Output:
C   IRET   I     Return error code. 0 => OK, otherwise error.
C   NOTE: this routine uses several of the arrays in the DSEL.INC
C   commons as work space.
C-----------------------------------------------------------------------
      INTEGER   IRET, SUBBEG, SUBEND, MIF
C
      CHARACTER KEYS(22)*24
      HOLLERITH CATUVH(256)
      INTEGER   ANT, REF, KOLS(22), LOOP, NKEY, LKEY, IFLOOP, KEY(2,2),
     *   IPNT, IIVER, NIF, IERR, MXINDX, REFTMP, LOOPSA, MBKOLS(14),
     *   KEYSUB(2,2), LUNCH
      LOGICAL   T, ISAPPL, DOIT
      INTEGER   ISNRNO, MXCNT
      INTEGER   NUMROW, NWD
      LONGINT   OFFSET
      REAL      WORK(1), SMOTIM(3), FKEY(2,2)
      LOGICAL   NOTREF
      INCLUDE 'KRING.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DOUBLE PRECISION FOFF(MAXIF), FREQIF
      CHARACTER BNDCOD(MAXIF)*8
      EQUIVALENCE (CATUVH, CATUV)
      DATA NKEY, LKEY /11,24/
      DATA T /.TRUE./
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA KEYS /'ANTENNA NO.             ',
     *   'REFANT 1                ', 'SUBARRAY                ',
     *   'WEIGHT 1                ', 'TIME                    ',
     *   'REAL1                   ', 'IMAG1                   ',
     *   'DELAY 1                 ', 'RATE 1                  ',
     *   'MBDELAY1                ', 'DISP 1',
     *   'ANTENNA NO.             ',
     *   'REFANT 2                ', 'SUBARRAY                ',
     *   'WEIGHT 2                ', 'TIME                    ',
     *   'REAL2                   ', 'IMAG2                   ',
     *   'DELAY 2                 ', 'RATE 2                  ',
     *   'MBDELAY2                ', 'DISP 2' /
C-----------------------------------------------------------------------
      LUNCH = 29
      LUNCH = 27
C                                       Initialize OFFSET; if OFFSET = 0
C                                       then workspace has not been
C                                       allocated
      OFFSET = 0
C                                       NOTREF indicates that some
C                                       antenna solutions were not
C                                       adjusted.
      NOTREF = .FALSE.
C                                       See if any work to be done.
      DOIT = ZPHS.OR.ZRAT.OR.ZDEL
      MXCNT = 0
      DO 5 LOOP = 1,NUMANT
         DOIT = DOIT .OR. ((REFUSE(LOOP).GT.0) .AND. (LOOP.NE.REFANT))
         IF (REFUSE(LOOP).GT.MXCNT) MXINDX = LOOP
         IF (REFUSE(LOOP).GT.MXCNT) MXCNT = REFUSE(LOOP)
 5       CONTINUE
      IF (.NOT.DOIT) GO TO 999
C                                       Message
      MSGTXT = 'Adjusting solutions to a common reference antenna'
      CALL MSGWRT (2)
C
C
C
C                                       If user did not specify a REFANT, use
C                                       the one with the most solutions.
      REFTMP = REFANT
      IF (REFTMP.LE.0) REFTMP = MXINDX
C                                       Get IF frequencies
      FOFF(1) = 0.0
      IIVER = 1
      IF (EIF.GT.1)
     *   CALL CHNDAT ('READ', CLBUFF, DISKIN, CNOIN, IIVER, INCAT,
     *     LUNCH, NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
C
C                                       Open solution table
      CALL SNINI ('READ', CLBUFF, DISKIN, CNOIN, SNVER, INCAT, LUNCH,
     *   ISNRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, MIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.GT.0) GO TO 990
C
      NUMROW = CLBUFF(5)
C                                       Set column pointers for sort
      CALL FNDCOL (NKEY, KEYS(1), LKEY, T, CLBUFF, KOLS(1), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 980
         END IF
C                                       Close
      CALL TABIO ('CLOS', 0, ISNRNO, WORK, CLBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 980
         END IF
C                                       Allocate workspace for CALREF.
C                                       If each of the 5 scratch arrays
C                                       has NUMROW then CALREF is
C                                       guaranteed to have enough space
C                                       to re-refence the table. In most
C                                       cases this will be less than a
C                                       megabyte of data so the
C                                       allocation should rarely fail.
C                                       A single array is allocated and
C                                       divided up for the CALREF calls.
      NWD = (5 * NUMROW- 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'KRNADJ', NWD, WORK, OFFSET, IRET)
      IF (IRET .NE. 0) THEN
         MSGTXT = 'Could not allocate enough workspace to reference'
         CALL MSGWRT (6)
         MSGTXT = 'solutions to a common antenna. Solutions will not'
         CALL MSGWRT (6)
         MSGTXT = 'be adjusted. If this is not acceptable then either'
         CALL MSGWRT (6)
         MSGTXT = 'increase SOLINT or shutdown some applications and'
         CALL MSGWRT (6)
         MSGTXT = 'run KRING again.'
         IRET = 0
         GO TO 999
         END IF
C                                       Sort to time-ant order.
      KEY(1,1) = KOLS(5)
      KEY(2,1) = KOLS(5)
      KEY(1,2) = KOLS(1)
      KEY(2,2) = KOLS(1)
      CALL TABSRT (DISKIN, CNOIN, 'SN', SNVER, SNVER, KEY, KEYSUB, FKEY,
     *   CLBUFF, INCAT, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 980
         END IF
C                                       Open for write
      CALL SNINI ('WRIT', CLBUFF, DISKIN, CNOIN, SNVER, INCAT, LUNCH,
     *   ISNRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, MIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Set column pointers
      DO 40 LOOP = 1,11
         IPNT = KOLS(LOOP)
         KOLS(LOOP) = CLKOLS(IPNT)
 40      CONTINUE
C                                       Set MBD kol pointers
      CALL COPY (5, KOLS(1), MBKOLS(1))
      MBKOLS(6) = KOLS(10)
      MBKOLS(7) = KOLS(11)
C
      IF (NUMPOL.GT.1) THEN
C                                       Second Stokes'
         CALL FNDCOL (NKEY, KEYS(11), LKEY, T, CLBUFF, KOLS(11), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET
            GO TO 980
            END IF
         DO 60 LOOP = 12,22
            IPNT = KOLS(LOOP)
            KOLS(LOOP) = CLKOLS(IPNT)
 60         CONTINUE
C                                       Set MBD kol pointers
         CALL COPY (5, KOLS(12), MBKOLS(8))
         MBKOLS(13) = KOLS(21)
         MBKOLS(14) = KOLS(22)
         END IF
C                                       Smoothing times
      SMOTIM(1) = 1.0E-6
      SMOTIM(2) = 1.0E-6
      SMOTIM(3) = 1.0E-6
C                                       Re-reference multi-band delays
C                                       first. Routine MBDREF does NOT
C                                       change the reference antenna,
C                                       leaves that to CALREF.
C                                       Loop over subarrays
      DO 110 LOOPSA = SUBBEG, SUBEND
C                                       Loop over reference antennas
C                                       used.
         REF = REFTMP
         DO 100 LOOP = 1,NUMANT
            IF ((REFUSS(LOOP,LOOPSA).GT.0) .AND. (LOOP.NE.REF)) THEN
               ANT = LOOP
C                                       First Stokes'
               CALL MBDREF (ANT, REF, LOOPSA, MBKOLS(1), CLBUFF,
     *            SMOTIM, NUMROW, WORK(OFFSET+1), WORK(OFFSET+NUMROW+1),
     *            WORK(OFFSET+2*NUMROW+1), WORK(OFFSET+3*NUMROW+1),
     *            IRET)
               IF (IRET.GT.0) GO TO 990
C                                       Second Stokes'
               IF (NUMPOL.GT.1) THEN
                  CALL MBDREF (ANT, REF, LOOPSA, MBKOLS(8), CLBUFF,
     *               SMOTIM, NUMROW, WORK(OFFSET+1),
     *               WORK(OFFSET+NUMROW+1), WORK(OFFSET+2*NUMROW+1),
     *               WORK(OFFSET+3*NUMROW+1), IRET)
                  IF (IRET.GT.0) GO TO 990
                  END IF
               END IF
  100       CONTINUE
  110    CONTINUE
C                                       Now re-reference IF dependent
C                                       values
C                                       Loop over IFs
      DO 200 IFLOOP = 1,MIF
         FREQIF = FREQ + FOFF(BIF + IFLOOP - 1)
C                                       Loop over subarrays
         DO 150 LOOPSA = SUBBEG,SUBEND
C                                       Loop over reference antennas
C                                       used.
            REF = REFTMP
            DO 150 LOOP = 1,NUMANT
               IF ((REFUSS(LOOP,LOOPSA).GT.0) .AND. (LOOP.NE.REF)) THEN
                  ANT = LOOP
C                                       First Stokes'
                  CALL CALREF (ANT, REF, LOOPSA, KOLS(1), FREQIF,
     *               SMOTIM, NUMROW, CLBUFF, WORK(OFFSET+1),
     *               WORK(OFFSET+NUMROW+1),
     *               WORK(OFFSET+2*NUMROW+1),
     *               WORK(OFFSET+3*NUMROW+1),
     *               WORK(OFFSET+4*NUMROW+1), IRET)
C                                       Inability to connect ANT to
C                                       REF is not necessarily an
C                                       error
                  IF (IRET.EQ.1) THEN
                     IRET = 0
                     NOTREF = .TRUE.
                     END IF
                  IF (IRET.GT.0) GO TO 990
                  IF (NUMPOL.LE.1) GO TO 150
C                                       Second Stokes'
                  CALL CALREF (ANT, REF, LOOPSA, KOLS(12), FREQIF,
     *               SMOTIM, NUMROW, CLBUFF, WORK(OFFSET+1),
     *               WORK(OFFSET+NUMROW+1),
     *               WORK(OFFSET+2*NUMROW+1),
     *               WORK(OFFSET+3*NUMROW+1),
     *               WORK(OFFSET+4*NUMROW+1), IRET)
C                                       Inability to connect ANT to
C                                       REF is not necessarily an
C                                       error
                  IF (IRET.EQ.1) THEN
                     IRET = 0
                     NOTREF = .TRUE.
                     END IF
                  IF (IRET.GT.0) GO TO 990
                  END IF
 150           CONTINUE
C                                       zero solutions if so requested
         IF (ZPHS.OR.ZDEL.OR.ZRAT) THEN
            CALL CALZER (KOLS(1), CLBUFF, ZPHS, ZDEL, ZRAT, IRET)
            IF (IRET.GT.0) GO TO 990
            IF (NUMPOL.GT.1) THEN
               CALL CALZER (KOLS(12), CLBUFF, ZPHS, ZDEL, ZRAT, IRET)
               IF (IRET.GT.0) GO TO 990
               END IF
            END IF
C                                       Update column pointers for IF
         KOLS(2) = KOLS(2) + 1
         KOLS(4) = KOLS(4) + 1
         KOLS(6) = KOLS(6) + 1
         KOLS(7) = KOLS(7) + 1
         KOLS(8) = KOLS(8) + 1
         KOLS(9) = KOLS(9) + 1
         KOLS(13) = KOLS(13) + 1
         KOLS(15) = KOLS(15) + 1
         KOLS(17) = KOLS(17) + 1
         KOLS(18) = KOLS(18) + 1
         KOLS(19) = KOLS(19) + 1
         KOLS(20) = KOLS(20) + 1
 200     CONTINUE
C                                       Warn user if re-referencing was
C                                       partially successful
      IF (NOTREF) THEN
         MSGTXT = 'Some solutions were not adjusted to the common'
         CALL MSGWRT (6)
         MSGTXT = 'reference antenna. This may be because one or more'
         CALL MSGWRT (6)
         MSGTXT = 'antennas were not used. Check your data before'
         CALL MSGWRT (6)
         MSGTXT = 'proceeding.'
         CALL MSGWRT (6)
         END IF
C                                       Close table
      CALL TABIO ('CLOS', 0, ISNRNO, WORK, CLBUFF, IRET)
      GO TO 999
C                                       Error
 980  CALL MSGWRT (6)
 990  WRITE (MSGTXT,1990)
      CALL MSGWRT (6)
C
 999  CONTINUE
C                                       De-allocate any dynamic
C                                       workspace. Ignore errors.
      IF (OFFSET .NE. 0) THEN
         CALL ZMEMRY ('FREE', 'KRNADJ', 5 * NUMROW, WORK, OFFSET, IERR)
         END IF
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I5,' FINDING SN TABLE COLUMNS')
 1010 FORMAT ('TABIO ERROR ',I5,' CLOSING SN TABLE')
 1020 FORMAT ('TABSRT ERROR ',I5,' SORTING SN TABLE')
 1990 FORMAT ('ERROR OCCURED IN KRNADJ')
      END
      SUBROUTINE KRNHIS (SOLMOD)
C-----------------------------------------------------------------------
C   KRNHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12, HILINE*72, SOLMOD*(*)
      INTEGER   LUNTB1, LUNTB2, LIMIT, IERR, ITEMP, I, TIME(3),
     *   DATE(3), SOLMIN, SOLSUB
      LOGICAL   T
      INCLUDE 'KRING.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      LUNTB1 = 27
      LUNTB2 = 28
C                                       sub interval parameters
      SOLMIN = XSOLM + 0.1
      SOLSUB = XSOLS + 0.1
      IF (SOLSUB.LE.0) SOLSUB = 1
      IF (SOLSUB.GT.10) SOLSUB = 10
      IF ((SOLMIN.LE.0) .OR. (SOLMIN.GT.SOLSUB)) SOLMIN = SOLSUB
C                                       Write History.
      CALL HIINIT (3)
C                                       Multisource - open old history
      CALL HIOPEN (LUNTB2, DISKIN, CNOIN, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       calibration history
      CALL CALHIS (LUNTB2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Write control info.
C                                       CC tables
      IF (SMODEL(1).LE.0.0) THEN
C                                       CC File Name etc.
         CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUNTB2,
     *      BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                        CCfile version no.
         WRITE (HILINE,2001) TSKNAM, CCTVER
         CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                        Number of images
         WRITE (HILINE,2002) TSKNAM, MFIELD
         CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Number of CLEAN components.
         DO 140 I = 1,MFIELD
            WRITE (HILINE,2003) TSKNAM, I, NCOMP(I)
            CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
 140        CONTINUE
         END IF
C                                       General information
C                                       Soln. interval.
      XSOLIN = SOLINT  / 60.0
      IF (XSOLIN.GT.9999.99) XSOLIN = 9999.99
      WRITE (HILINE,2010) TSKNAM, XSOLIN
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       subintervals
      IF (SOLSUB.GT.1) THEN
         WRITE (HILINE,2005) TSKNAM, SOLSUB
         CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,2006) TSKNAM, SOLMIN
         CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                        Reference ant
      WRITE (HILINE,2012) TSKNAM, REFANT
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Average RR,LL
      ITEMP = -1
      IF (AVGPOL) ITEMP = 1
      WRITE (HILINE,2013) TSKNAM, ITEMP
      IF (NCOR.GT.1) CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Soln in each IF?
      ITEMP = 1
      IF (DOIF) ITEMP = -1
      IF (DODLBY) ITEMP = 2
      WRITE (HILINE,2015) TSKNAM, ITEMP
      IF (EIF.GT.BIF) CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Multi- and single-band fit
      IF (DODLBY) THEN
         WRITE (HILINE,3015) TSKNAM
         CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                        SNR cutoff.
      WRITE (HILINE,2016) TSKNAM, SNRFFT
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Point source model
      IF (ABS (SMODEL(1)).GT.0.0) THEN
         WRITE (HILINE,2020) TSKNAM, SMODEL(1), SMODEL(2), SMODEL(3)
         CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Other parameters
         WRITE (HILINE,2021) TSKNAM, SMODEL(4), SMODEL(5),
     *      SMODEL(6), SMODEL(7)
         IF (SMODEL(4).GT.0.01) CALL HIADD (LUNTB2, HILINE, BUFF2,
     *        IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Output SN table
      WRITE (HILINE,2062) TSKNAM, SNVER
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Full weight annulus
      WRITE (HILINE,2035) TSKNAM, MNPABL
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
      WRITE (HILINE,2036) TSKNAM, MXPABL
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
      WRITE (HILINE,2037) TSKNAM, WTPABL
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       No. baselines to search
      WRITE (HILINE,2040) TSKNAM, BLDO
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Delay window
      IF (.NOT.DODLAY) THEN
         WRITE (HILINE,3023) TSKNAM
      ELSE
         WRITE (HILINE,2041) TSKNAM, DWINDO
         END IF
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Rate window.
      IF (.NOT.DORATE) THEN
         WRITE (HILINE, 3024) TSKNAM
      ELSE
         WRITE (HILINE,2042) TSKNAM, RWINDO
         END IF
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Integration time.
      WRITE (HILINE,2043) TSKNAM, CPARM(1)
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        LS solution?
      ITEMP = 1
      IF (DODRLS) ITEMP = -1
      WRITE (HILINE,2044) TSKNAM, ITEMP
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Rereferenced?
      IF (CPARM(8).GT.1.0E-10) THEN
         ITEMP = 1
         WRITE (HILINE,2045) TSKNAM, ITEMP
         CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       BIF/EIF
      IF (SOLMOD(1:1).NE.'N') THEN
         WRITE (HILINE,2046) TSKNAM, 'B', LBIF
         CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,2046) TSKNAM, 'E', LBIF
         CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Antenna weights.
      ITEMP = 1
      LIMIT = MIN (ITEMP+8,NANT)
      WRITE (HILINE,2050) TSKNAM, (ANTWT(I), I = ITEMP,LIMIT)
      CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
      ITEMP = ITEMP + 9
 160  IF (ITEMP.LE.NANT) THEN
         LIMIT = MIN (ITEMP+8,NANT)
         WRITE (HILINE,2051) TSKNAM, (ANTWT(I), I = ITEMP,LIMIT)
         CALL HIADD (LUNTB2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         ITEMP = ITEMP + 9
         GO TO 160
         END IF
C                                       Close HI file
 190  CALL HICLOS (LUNTB2, T, BUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2001 FORMAT (A6,'INVER = ',I5,' /CC file version no.')
 2002 FORMAT (A6,'NMAPS =',I4,' /Number of clean images used')
 2003 FORMAT (A6,'NCOMP(',I3,') = ',I8,' /Number of clean comps.')
 2005 FORMAT (A6,'SOLSUB = ',I3,' /Number of sub-intervals')
 2006 FORMAT (A6,'SOLMIN = ',I3,' /Min number of sub-intervals')
 2010 FORMAT (A6,'SOLINT = ',F7.2,' /Soln. inter. (min)')
 2012 FORMAT (A6,'REFANT = ',I4,' /Reference antenna')
 2013 FORMAT (A6,'CPARM(2) = ',I4,' />0 => avg. RR,LL')
 2015 FORMAT (A6,'CPARM(5) = ',I4,' />0 => soln. for each IF')
 3015 FORMAT (A6,'/ Did multi- and single band delay fits')
 2016 FORMAT (A6,'CPARM(4)=',F5.1,' / SNR cutoff')
 2020 FORMAT (A6,'SMODEL = ',2(F10.5,','),F10.5,
     *   ' /Pt. model parameters')
 2021 FORMAT (A6,'        ',4F10.5,' / Other parms.')
 2062 FORMAT (A6,'SNVER =',I4,' / Output SN table version')
 2035 FORMAT (A6,'UVRANGE(1)=',1PE12.5,' /Min. bl. full weight')
 2036 FORMAT (A6,'UVRANGE(2)=',1PE12.5,' /Max. bl. full weight')
 2037 FORMAT (A6,'WTUV =',1PE12.5,' /Weight outside annulus')
 2040 FORMAT (A6,'CPARM(3)=',I2,' /No. baseline combinations')
 3023 FORMAT (A6,'/ No delay search performed')
 2041 FORMAT (A6,'CPARM(2)=',F10.0,' /Delay win. (nsec)')
 3024 FORMAT (A6,'/ No rate search performed')
 2042 FORMAT (A6,'CPARM(3)=',F8.2,' /Rate win. (MHz)')
 2043 FORMAT (A6,'CPARM(1)=',F5.2,' /Input integ. time (sec)')
 2044 FORMAT (A6,'CPARM(10)=',I2,' /.gt.0 => no ls. soln.')
 2045 FORMAT (A6,'CPARM(8)=',I2,' /.gt.0 => do not rereference')
 2046 FORMAT (A6,A1,'IF =',I3,5X,'/ IF range used in fit')
 2050 FORMAT (A6,'ANTWT=',9F5.1,' /Ant. wt')
 2051 FORMAT (A6,'     ',9F5.1)
      END
      SUBROUTINE GETSNR (AMPL, SUMW, SUMWW, XCOUNT, SNRONE)
C-----------------------------------------------------------------------
C  This subroutine uses the formulas in AIPS memo 101 to convert
C  the coherent peak amplitude and moments of the weights into
C  an SNR
C-----------------------------------------------------------------------
      DOUBLE PRECISION AMPL, XCOUNT, SUMW, SUMWW, SNRONE
      DOUBLE PRECISION SNRPV, Z, Z2
C-----------------------------------------------------------------------
      Z = AMPL - SQRT(SUMWW)
      Z = (Z / SUMW) * ( AMPL + SQRT(SUMWW) ) / SUMW
      Z = Z * XCOUNT / (XCOUNT - 1.0D0)
      Z = MAX ( Z, 0.0D0)
      Z = SQRT(Z)
      Z = MIN(Z, 1.0D0)
      Z2 = SNRPV(Z)
      SNRONE = Z2
      RETURN
      END
      DOUBLE PRECISION FUNCTION SNRPV (GTAR)
C-----------------------------------------------------------------------
C  This function figures out SNR Per Vis when given GTAR
C-----------------------------------------------------------------------
      DOUBLE PRECISION GOGAMM, GTAR, GAMLOW, GAMUPP, GAMNEW, GNEW
C-----------------------------------------------------------------------
      GAMLOW = 3.3D-12
      GAMUPP = 2.5D10
 30   CONTINUE
      GAMNEW = SQRT(GAMLOW*GAMUPP)
C     NETGET computes the same function but using routines I snagged
C     off of Brian.
      GNEW = GOGAMM(GAMNEW)
      IF (GNEW.GT.GTAR) GAMUPP = GAMNEW
      IF (GNEW.LT.GTAR) GAMLOW = GAMNEW
      IF (ABS(GAMUPP-GAMLOW).GT.(0.001*GAMUPP)) GO TO 30
      SNRPV = SQRT(4.0D0*GAMNEW)
      RETURN
      END
      DOUBLE PRECISION FUNCTION GOGAMM(X)
C-----------------------------------------------------------------------
C  This function returns G(gamma=x) as defined in AIPS MEMO 101
C  This uses the tabulated approximations provided in Abramowitz and
C  Stegun.  We need a better algorithm here!
C-----------------------------------------------------------------------
      DOUBLE PRECISION X, T
      DOUBLE PRECISION I981, I983P, I982A4, GOGAMF
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      T = X / 3.75D0
      IF (ABS(T).LE.1.0D0) THEN
         GOGAMF = SQRT(X) * EXP(-X) * (I981(T)+ I983P(T))/2.0D0
      ELSE
         GOGAMF = I982A4(T)
         END IF
      GOGAMM = GOGAMF * SQRT(TWOPI)
      RETURN
      END
      DOUBLE PRECISION FUNCTION I981 (T)
C-----------------------------------------------------------------------
C  This function returns the rhs of A&S 9.8.1
C  T2 must be within the closed interval +/- 1.0
C-----------------------------------------------------------------------
      DOUBLE PRECISION T, T2
C-----------------------------------------------------------------------
      T2 = T*T
      I981 = 1.0D0
     *     + 3.5156229D0 * T2
     *     + 3.0899424D0 * T2**2
     *     + 1.2067492D0 * T2**3
     *     + 0.2659732D0 * T2**4
     *     + 0.0360768D0 * T2**5
     *     + 0.0045813D0 * T2**6
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION I983P (T)
C  This function returns the rhs of A&S 9.8.3
C  multiplied by 3.75T (= X)
      DOUBLE PRECISION T, T2, I983
      T2 = T*T
      I983 = 0.5D0
     *      + 0.87890594D0 * T2
     *      + 0.51498869D0 * T2**2
     *      + 0.15084934D0 * T2**3
     *      + 0.02658733D0 * T2**4
     *      + 0.00301532D0 * T2**5
     *      + 0.00032411D0 * T2**6
      I983P = I983 * T * 3.75D0
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION I982A4 (T)
C  This function returns the avg of the rhs's of A&S 9.8.2 and 9.8.4
      DOUBLE PRECISION T, ONEOT, I982P4
      ONEOT = 1.0D0 / T
      I982P4 =  0.39894228D0 + 0.39894228D0
     *       + (0.01328592D0 - 0.03988024D0) * ONEOT
     *       + (0.00225319D0 - 0.00362018D0) * ONEOT**2
     *       - (0.00157565D0 - 0.00163801D0) * ONEOT**3
     *       + (0.00916281D0 - 0.01031555D0) * ONEOT**4
     *       - (0.02057706D0 - 0.02282967D0) * ONEOT**5
     *       + (0.02635537D0 - 0.02895312D0) * ONEOT**6
     *       - (0.01647633D0 - 0.01787654D0) * ONEOT**7
     *       + (0.00392377D0 - 0.00420059D0) * ONEOT**8
      I982A4 = I982P4/2.0D0
      RETURN
      END
C
      SUBROUTINE FNDK(T0, T1, TARR, NTARR, IT0, IT1)
C                                             this subroutine
C                                             finds those indices
C                                             IT0 and IT1 for which
C                                             TARR(IT0) and
C                                             TARR(IT1) lie within
C                                             T0 and T1
      REAL T0, T1, TARR(*)
      INTEGER NTARR, IT0, IT1
C                                             Do bounds checking
      IF (T0.GT.T1) THEN
         IT0 = 1
         IT1 = 0
         GO TO 990
         END IF
C                                             Do lower bound
      DO 100 IT0 = NTARR+1,2,-1
         IF (T0.GT.TARR(IT0-1)) GO TO 110
 100     CONTINUE
C                                             If T0=<TARR(1)
      IT0 = 1
 110  CONTINUE
C                                             Do upper bound
      DO 200 IT1 = 0,NTARR-1
         IF (T1.LT.TARR(1+IT1)) GO TO 210
 200     CONTINUE
C                                             If T1>=TARR(NTARR)
      IT1 = NTARR
 210  CONTINUE
 990  CONTINUE
      RETURN
C
      END

* ======================================================================
* NIST Guide to Available Math Software.
* NeededBLAS for module DGESVD from package LAPACK.
* Retrieved from CAMSUN on Wed Sep 17 19:19:33 1997.
* ======================================================================

* BLAS REQUIRED BY LAPACK ROUTINE: dgesvd
* -----------------------------------------------------------
* Note: Link to BLAS optimized for your system, if available.
* -----------------------------------------------------------

      subroutine  dcopy(n,dx,incx,dy,incy)
c
c     copies a vector, x, to a vector, y.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*)
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,7)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dx(i)
   30 continue
      if( n .lt. 7 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,7
        dy(i) = dx(i)
        dy(i + 1) = dx(i + 1)
        dy(i + 2) = dx(i + 2)
        dy(i + 3) = dx(i + 3)
        dy(i + 4) = dx(i + 4)
        dy(i + 5) = dx(i + 5)
        dy(i + 6) = dx(i + 6)
   50 continue
      return
      end
      DOUBLE PRECISION FUNCTION DNRM2 ( N, X, INCX )
*     .. Scalar Arguments ..
      INTEGER                           INCX, N
*     .. Array Arguments ..
      DOUBLE PRECISION                  X( * )
*     ..
*
*  DNRM2 returns the euclidean norm of a vector via the function
*  name, so that
*
*     DNRM2 := sqrt( x'*x )
*
*
*
*  -- This version written on 25-October-1982.
*     Modified on 14-October-1993 to inline the call to DLASSQ.
*     Sven Hammarling, Nag Ltd.
*
*
*     .. Parameters ..
      DOUBLE PRECISION      ONE         , ZERO
      PARAMETER           ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      INTEGER               IX
      DOUBLE PRECISION      ABSXI, NORM, SCALE, SSQ
*     .. Intrinsic Functions ..
      INTRINSIC             ABS, SQRT
*     ..
*     .. Executable Statements ..
      IF( N.LT.1 .OR. INCX.LT.1 )THEN
         NORM  = ZERO
      ELSE IF( N.EQ.1 )THEN
         NORM  = ABS( X( 1 ) )
      ELSE
         SCALE = ZERO
         SSQ   = ONE
*        The following loop is equivalent to this call to the LAPACK
*        auxiliary routine:
*        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
*
         DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
            IF( X( IX ).NE.ZERO )THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI )THEN
                  SSQ   = ONE   + SSQ*( SCALE/ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SSQ   = SSQ   +     ( ABSXI/SCALE )**2
               END IF
            END IF
   10    CONTINUE
         NORM  = SCALE * SQRT( SSQ )
      END IF
*
      DNRM2 = NORM
      RETURN
*
*     End of DNRM2.
*
      END
      subroutine  drot (n,dx,incx,dy,incy,c,s)
c
c     applies a plane rotation.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp,c,s
      integer i,incx,incy,ix,iy,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = c*dx(ix) + s*dy(iy)
        dy(iy) = c*dy(iy) - s*dx(ix)
        dx(ix) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
   20 do 30 i = 1,n
        dtemp = c*dx(i) + s*dy(i)
        dy(i) = c*dy(i) - s*dx(i)
        dx(i) = dtemp
   30 continue
      return
      end
      subroutine  dscal(n,da,dx,incx)
c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision da,dx(*)
      integer i,incx,m,mp1,n,nincx
c
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end
      subroutine  dswap (n,dx,incx,dy,incy)
c
c     interchanges two vectors.
c     uses unrolled loops for increments equal one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
c
c       clean-up loop
c
   20 m = mod(n,3)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
        dtemp = dx(i + 1)
        dx(i + 1) = dy(i + 1)
        dy(i + 1) = dtemp
        dtemp = dx(i + 2)
        dx(i + 2) = dy(i + 2)
        dy(i + 2) = dtemp
   50 continue
      return
      end
      SUBROUTINE DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
*     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEMV  performs one of the matrix-vector operations
*
*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
*
*  where alpha and beta are scalars, x and y are vectors and A is an
*  m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
*
*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
*
*              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
*           Before entry, the incremented array X must contain the
*           vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
*           Before entry with BETA non-zero, the incremented array Y
*           must contain the vector y. On exit, Y is overwritten by the
*           updated vector y.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEMV ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
*     up the start points in  X  and  Y.
*
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
*     First form  y := beta*y.
*
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     $   RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  y := alpha*A*x + y.
*
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
*
*        Form  y := alpha*A'*x + y.
*
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               DO 90, I = 1, M
                  TEMP = TEMP + A( I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               DO 110, I = 1, M
                  TEMP = TEMP + A( I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DGEMV .
*
      END
      SUBROUTINE DTRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * )
*     ..
*
*  Purpose
*  =======
*
*  DTRMV  performs one of the matrix-vector operations
*
*     x := A*x,   or   x := A'*x,
*
*  where x is an n element vector and  A is an n by n unit, or non-unit,
*  upper or lower triangular matrix.
*
*  Parameters
*  ==========
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   x := A*x.
*
*              TRANS = 'T' or 't'   x := A'*x.
*
*              TRANS = 'C' or 'c'   x := A'*x.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit
*           triangular as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the order of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry with  UPLO = 'U' or 'u', the leading n by n
*           upper triangular part of the array A must contain the upper
*           triangular matrix and the strictly lower triangular part of
*           A is not referenced.
*           Before entry with UPLO = 'L' or 'l', the leading n by n
*           lower triangular part of the array A must contain the lower
*           triangular matrix and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u', the diagonal elements of
*           A are not referenced either, but are assumed to be unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, n ).
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the n
*           element vector x. On exit, X is overwritten with the
*           tranformed vector x.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JX, KX
      LOGICAL            NOUNIT
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.
     $         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.
     $         .NOT.LSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRMV ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
      NOUNIT = LSAME( DIAG, 'N' )
*
*     Set up the start point in X if the increment is not unity. This
*     will be  ( N - 1 )*INCX  too small for descending loops.
*
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  x := A*x.
*
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 20, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     DO 10, I = 1, J - 1
                        X( I ) = X( I ) + TEMP*A( I, J )
   10                CONTINUE
                     IF( NOUNIT )
     $                  X( J ) = X( J )*A( J, J )
                  END IF
   20          CONTINUE
            ELSE
               JX = KX
               DO 40, J = 1, N
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 30, I = 1, J - 1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      + INCX
   30                CONTINUE
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )*A( J, J )
                  END IF
                  JX = JX + INCX
   40          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 60, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     DO 50, I = N, J + 1, -1
                        X( I ) = X( I ) + TEMP*A( I, J )
   50                CONTINUE
                     IF( NOUNIT )
     $                  X( J ) = X( J )*A( J, J )
                  END IF
   60          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 80, J = N, 1, -1
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 70, I = N, J + 1, -1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      - INCX
   70                CONTINUE
                     IF( NOUNIT )
     $                  X( JX ) = X( JX )*A( J, J )
                  END IF
                  JX = JX - INCX
   80          CONTINUE
            END IF
         END IF
      ELSE
*
*        Form  x := A'*x.
*
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 100, J = N, 1, -1
                  TEMP = X( J )
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 90, I = J - 1, 1, -1
                     TEMP = TEMP + A( I, J )*X( I )
   90             CONTINUE
                  X( J ) = TEMP
  100          CONTINUE
            ELSE
               JX = KX + ( N - 1 )*INCX
               DO 120, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 110, I = J - 1, 1, -1
                     IX   = IX   - INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  110             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   - INCX
  120          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 140, J = 1, N
                  TEMP = X( J )
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 130, I = J + 1, N
                     TEMP = TEMP + A( I, J )*X( I )
  130             CONTINUE
                  X( J ) = TEMP
  140          CONTINUE
            ELSE
               JX = KX
               DO 160, J = 1, N
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 150, I = J + 1, N
                     IX   = IX   + INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  150             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   + INCX
  160          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of DTRMV .
*
      END
      SUBROUTINE DGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
*     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, INCY, LDA, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DGER   performs the rank 1 operation
*
*     A := alpha*x*y' + A,
*
*  where alpha is a scalar, x is an m element vector, y is an n element
*  vector and A is an m by n matrix.
*
*  Parameters
*  ==========
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( m - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the m
*           element vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  Y      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCY ) ).
*           Before entry, the incremented array Y must contain the n
*           element vector y.
*           Unchanged on exit.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients. On exit, A is
*           overwritten by the updated matrix.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JY, KX
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGER  ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      IF( INCX.EQ.1 )THEN
         DO 20, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          CONTINUE
            END IF
            JY = JY + INCY
   20    CONTINUE
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         DO 40, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          CONTINUE
            END IF
            JY = JY + INCY
   40    CONTINUE
      END IF
*
      RETURN
*
*     End of DGER  .
*
      END
      SUBROUTINE DGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  DGEMM  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*op( B ) + beta*C,
*
*  where  op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X',
*
*  alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n',  op( A ) = A.
*
*              TRANSA = 'T' or 't',  op( A ) = A'.
*
*              TRANSA = 'C' or 'c',  op( A ) = A'.
*
*           Unchanged on exit.
*
*  TRANSB - CHARACTER*1.
*           On entry, TRANSB specifies the form of op( B ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSB = 'N' or 'n',  op( B ) = B.
*
*              TRANSB = 'T' or 't',  op( B ) = B'.
*
*              TRANSB = 'C' or 'c',  op( B ) = B'.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry,  M  specifies  the number  of rows  of the  matrix
*           op( A )  and of the  matrix  C.  M  must  be at least  zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N  specifies the number  of columns of the matrix
*           op( B ) and the number of columns of the matrix C. N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry,  K  specifies  the number of columns of the matrix
*           op( A ) and the number of rows of the matrix op( B ). K must
*           be at least  zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by m  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  n by k  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
*           LDB must be at least  max( 1, k ), otherwise  LDB must be at
*           least  max( 1, n ).
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n  matrix
*           ( alpha*op( A )*op( B ) + beta*C ).
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
*     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
*     and  columns of  A  and the  number of  rows  of  B  respectively.
*
      NOTA  = LSAME( TRANSA, 'N' )
      NOTB  = LSAME( TRANSB, 'N' )
      IF( NOTA )THEN
         NROWA = M
         NCOLA = K
      ELSE
         NROWA = K
         NCOLA = M
      END IF
      IF( NOTB )THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
*
*     Test the input parameters.
*
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.
     $         ( .NOT.LSAME( TRANSB, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( K  .LT.0               )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEMM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And if  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         RETURN
      END IF
*
*     Start the operations.
*
      IF( NOTB )THEN
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B + beta*C.
*
            DO 90, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 50, I = 1, M
                     C( I, J ) = ZERO
   50             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             CONTINUE
               END IF
               DO 80, L = 1, K
                  IF( B( L, J ).NE.ZERO )THEN
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B + beta*C
*
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = ZERO
                  DO 100, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  100             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  110          CONTINUE
  120       CONTINUE
         END IF
      ELSE
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B' + beta*C
*
            DO 170, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 130, I = 1, M
                     C( I, J ) = ZERO
  130             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 140, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  140             CONTINUE
               END IF
               DO 160, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*B( J, L )
                     DO 150, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  150                CONTINUE
                  END IF
  160          CONTINUE
  170       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B' + beta*C
*
            DO 200, J = 1, N
               DO 190, I = 1, M
                  TEMP = ZERO
                  DO 180, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  180             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  190          CONTINUE
  200       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DGEMM .
*
      END
      SUBROUTINE DTRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
*     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      DOUBLE PRECISION   ALPHA
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DTRMM  performs one of the matrix-matrix operations
*
*     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
*
*  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
*
*     op( A ) = A   or   op( A ) = A'.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*           On entry,  SIDE specifies whether  op( A ) multiplies B from
*           the left or right as follows:
*
*              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
*
*              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
*
*           Unchanged on exit.
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix A is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n'   op( A ) = A.
*
*              TRANSA = 'T' or 't'   op( A ) = A'.
*
*              TRANSA = 'C' or 'c'   op( A ) = A'.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit triangular
*           as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of B. M must be at
*           least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of B.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
*           zero then  A is not referenced and  B need not be set before
*           entry.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
*           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
*           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
*           upper triangular part of the array  A must contain the upper
*           triangular matrix  and the strictly lower triangular part of
*           A is not referenced.
*           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
*           lower triangular part of the array  A must contain the lower
*           triangular matrix  and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
*           A  are not referenced either,  but are assumed to be  unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
*           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
*           then LDA must be at least max( 1, n ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
*           Before entry,  the leading  m by n part of the array  B must
*           contain the matrix  B,  and  on exit  is overwritten  by the
*           transformed matrix.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in  the  calling  (sub)  program.   LDB  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
*
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRMM ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
*     And when  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
*
*     Start the operations.
*
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*A*B.
*
            IF( UPPER )THEN
               DO 50, J = 1, N
                  DO 40, K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*B( K, J )
                        DO 30, I = 1, K - 1
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   30                   CONTINUE
                        IF( NOUNIT )
     $                     TEMP = TEMP*A( K, K )
                        B( K, J ) = TEMP
                     END IF
   40             CONTINUE
   50          CONTINUE
            ELSE
               DO 80, J = 1, N
                  DO 70 K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP      = ALPHA*B( K, J )
                        B( K, J ) = TEMP
                        IF( NOUNIT )
     $                     B( K, J ) = B( K, J )*A( K, K )
                        DO 60, I = K + 1, M
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   60                   CONTINUE
                     END IF
   70             CONTINUE
   80          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*B*A'.
*
            IF( UPPER )THEN
               DO 110, J = 1, N
                  DO 100, I = M, 1, -1
                     TEMP = B( I, J )
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( I, I )
                     DO 90, K = 1, I - 1
                        TEMP = TEMP + A( K, I )*B( K, J )
   90                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  100             CONTINUE
  110          CONTINUE
            ELSE
               DO 140, J = 1, N
                  DO 130, I = 1, M
                     TEMP = B( I, J )
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( I, I )
                     DO 120, K = I + 1, M
                        TEMP = TEMP + A( K, I )*B( K, J )
  120                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  130             CONTINUE
  140          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*B*A.
*
            IF( UPPER )THEN
               DO 180, J = N, 1, -1
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 150, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  150             CONTINUE
                  DO 170, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 160, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  160                   CONTINUE
                     END IF
  170             CONTINUE
  180          CONTINUE
            ELSE
               DO 220, J = 1, N
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
                  DO 190, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  190             CONTINUE
                  DO 210, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 200, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  200                   CONTINUE
                     END IF
  210             CONTINUE
  220          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*B*A'.
*
            IF( UPPER )THEN
               DO 260, K = 1, N
                  DO 240, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 250, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  250                CONTINUE
                  END IF
  260          CONTINUE
            ELSE
               DO 300, K = N, 1, -1
                  DO 280, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 270, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  270                   CONTINUE
                     END IF
  280             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 290, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  290                CONTINUE
                  END IF
  300          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of DTRMM .
*
      END

* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module DGESVD from package LAPACK.
* Retrieved from NETLIB on Wed Sep 17 19:10:51 1997.
* ======================================================================
      SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK driver routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          JOBU, JOBVT
      INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
     $                   VT( LDVT, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGESVD computes the singular value decomposition (SVD) of a real
*  M-by-N matrix A, optionally computing the left and/or right singular
*  vectors. The SVD is written
*
*       A = U * SIGMA * transpose(V)
*
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
*  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*
*  Note that the routine returns V**T, not V.
*
*  Arguments
*  =========
*
*  JOBU    (input) CHARACTER*1
*          Specifies options for computing all or part of the matrix U:
*          = 'A':  all M columns of U are returned in array U:
*          = 'S':  the first min(m,n) columns of U (the left singular
*                  vectors) are returned in the array U;
*          = 'O':  the first min(m,n) columns of U (the left singular
*                  vectors) are overwritten on the array A;
*          = 'N':  no columns of U (no left singular vectors) are
*                  computed.
*
*  JOBVT   (input) CHARACTER*1
*          Specifies options for computing all or part of the matrix
*          V**T:
*          = 'A':  all N rows of V**T are returned in the array VT;
*          = 'S':  the first min(m,n) rows of V**T (the right singular
*                  vectors) are returned in the array VT;
*          = 'O':  the first min(m,n) rows of V**T (the right singular
*                  vectors) are overwritten on the array A;
*          = 'N':  no rows of V**T (no right singular vectors) are
*                  computed.
*
*          JOBVT and JOBU cannot both be 'O'.
*
*  M       (input) INTEGER
*          The number of rows of the input matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the input matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit,
*          if JOBU = 'O',  A is overwritten with the first min(m,n)
*                          columns of U (the left singular vectors,
*                          stored columnwise);
*          if JOBVT = 'O', A is overwritten with the first min(m,n)
*                          rows of V**T (the right singular vectors,
*                          stored rowwise);
*          if JOBU .ne. 'O' and JOBVT .ne. 'O', the contents of A
*                          are destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The singular values of A, sorted so that S(i) >= S(i+1).
*
*  U       (output) DOUBLE PRECISION array, dimension (LDU,UCOL)
*          (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
*          If JOBU = 'A', U contains the M-by-M orthogonal matrix U;
*          if JOBU = 'S', U contains the first min(m,n) columns of U
*          (the left singular vectors, stored columnwise);
*          if JOBU = 'N' or 'O', U is not referenced.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= 1; if
*          JOBU = 'S' or 'A', LDU >= M.
*
*  VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
*          If JOBVT = 'A', VT contains the N-by-N orthogonal matrix
*          V**T;
*          if JOBVT = 'S', VT contains the first min(m,n) rows of
*          V**T (the right singular vectors, stored rowwise);
*          if JOBVT = 'N' or 'O', VT is not referenced.
*
*  LDVT    (input) INTEGER
*          The leading dimension of the array VT.  LDVT >= 1; if
*          JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
*          if INFO > 0, WORK(2:MIN(M,N)) contains the unconverged
*          superdiagonal elements of an upper bidiagonal matrix B
*          whose diagonal is in S (not necessarily sorted). B
*          satisfies A = U * B * VT, so it has the same singular values
*          as A, and singular vectors related by U and VT.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= 1.
*          LWORK >= MAX(3*MIN(M,N)+MAX(M,N),5*MIN(M,N)-4).
*          For good performance, LWORK should generally be larger.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if DBDSQR did not converge, INFO specifies how many
*                superdiagonals of an intermediate bidiagonal form B
*                did not converge to zero. See the description of WORK
*                above for details.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            WNTUA, WNTUAS, WNTUN, WNTUO, WNTUS, WNTVA,
     $                   WNTVAS, WNTVN, WNTVO, WNTVS
      INTEGER            BDSPAC, BLK, CHUNK, I, IE, IERR, IR, ISCL,
     $                   ITAU, ITAUP, ITAUQ, IU, IWORK, LDWRKR, LDWRKU,
     $                   MAXWRK, MINMN, MINWRK, MNTHR, NCU, NCVT, NRU,
     $                   NRVT, WRKBL
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, SMLNUM
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   DUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DBDSQR, DGEBRD, DGELQF, DGEMM, DGEQRF, DLACPY,
     $                   DLASCL, DLASET, DORGBR, DORGLQ, DORGQR, DORMBR,
     $                   XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      MINMN = MIN( M, N )
      MNTHR = ILAENV (6, 'DGESVD', JOBU // JOBVT, M, N, 0 )
      WNTUA = LSAME( JOBU, 'A' )
      WNTUS = LSAME( JOBU, 'S' )
      WNTUAS = WNTUA .OR. WNTUS
      WNTUO = LSAME( JOBU, 'O' )
      WNTUN = LSAME( JOBU, 'N' )
      WNTVA = LSAME( JOBVT, 'A' )
      WNTVS = LSAME( JOBVT, 'S' )
      WNTVAS = WNTVA .OR. WNTVS
      WNTVO = LSAME( JOBVT, 'O' )
      WNTVN = LSAME( JOBVT, 'N' )
      MINWRK = 1
*
      IF( .NOT.( WNTUA .OR. WNTUS .OR. WNTUO .OR. WNTUN ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( WNTVA .OR. WNTVS .OR. WNTVO .OR. WNTVN ) .OR.
     $         ( WNTVO .AND. WNTUO ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -6
      ELSE IF( LDU.LT.1 .OR. ( WNTUAS .AND. LDU.LT.M ) ) THEN
         INFO = -9
      ELSE IF( LDVT.LT.1 .OR. ( WNTVA .AND. LDVT.LT.N ) .OR.
     $         ( WNTVS .AND. LDVT.LT.MINMN ) ) THEN
         INFO = -11
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV.)
*
      IF( INFO.EQ.0 .AND. LWORK.GE.1 .AND. M.GT.0 .AND. N.GT.0 ) THEN
         IF( M.GE.N ) THEN
*
*           Compute space needed for DBDSQR
*
            BDSPAC = MAX( 3*N, 5*N-4 )
            IF( M.GE.MNTHR ) THEN
               IF( WNTUN ) THEN
*
*                 Path 1 (M much larger than N, JOBU='N')
*
                  MAXWRK = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1)
                  MAXWRK = MAX( MAXWRK, 3*N+2*N*
     $                     ILAENV( 1, 'DGEBRD', ' ', N, N, -1 ) )
                  IF( WNTVO .OR. WNTVAS )
     $               MAXWRK = MAX( MAXWRK, 3*N+( N-1 )*
     $                        ILAENV( 1, 'DORGBR', 'P', N, N, -1 ) )
                  MAXWRK = MAX( MAXWRK, BDSPAC )
                  MINWRK = MAX( 4*N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUO .AND. WNTVN ) THEN
*
*                 Path 2 (M much larger than N, JOBU='O', JOBVT='N')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( N*N+WRKBL, N*N+M*N+N )
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUO .AND. WNTVAS ) THEN
*
*                 Path 3 (M much larger than N, JOBU='O', JOBVT='S' or
*                 'A')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( N*N+WRKBL, N*N+M*N+N )
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUS .AND. WNTVN ) THEN
*
*                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUS .AND. WNTVO ) THEN
*
*                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUS .AND. WNTVAS ) THEN
*
*                 Path 6 (M much larger than N, JOBU='S', JOBVT='S' or
*                 'A')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUA .AND. WNTVN ) THEN
*
*                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M,
     $                    M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUA .AND. WNTVO ) THEN
*
*                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M,
     $                    M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUA .AND. WNTVAS ) THEN
*
*                 Path 9 (M much larger than N, JOBU='A', JOBVT='S' or
*                 'A')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M,
     $                    M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               END IF
            ELSE
*
*              Path 10 (M at least N, but not much larger)
*
               MAXWRK = 3*N + ( M+N )*ILAENV( 1, 'DGEBRD', ' ', M, N,
     $                  -1 )
               IF( WNTUS .OR. WNTUO )
     $            MAXWRK = MAX( MAXWRK, 3*N+N*
     $                     ILAENV( 1, 'DORGBR', 'Q', M, N, -1 ) )
               IF( WNTUA )
     $            MAXWRK = MAX( MAXWRK, 3*N+M*
     $                     ILAENV( 1, 'DORGBR', 'Q', M, M, -1 ) )
               IF( .NOT.WNTVN )
     $            MAXWRK = MAX( MAXWRK, 3*N+( N-1 )*
     $                     ILAENV( 1, 'DORGBR', 'P', N, N, -1 ) )
               MAXWRK = MAX( MAXWRK, BDSPAC )
               MINWRK = MAX( 3*N+M, BDSPAC )
               MAXWRK = MAX( MAXWRK, MINWRK )
            END IF
         ELSE
*
*           Compute space needed for DBDSQR
*
            BDSPAC = MAX( 3*M, 5*M-4 )
            IF( N.GE.MNTHR ) THEN
               IF( WNTVN ) THEN
*
*                 Path 1t(N much larger than M, JOBVT='N')
*
                  MAXWRK = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1)
                  MAXWRK = MAX( MAXWRK, 3*M+2*M*
     $                     ILAENV( 1, 'DGEBRD', ' ', M, M, -1 ) )
                  IF( WNTUO .OR. WNTUAS )
     $               MAXWRK = MAX( MAXWRK, 3*M+M*
     $                        ILAENV( 1, 'DORGBR', 'Q', M, M, -1 ) )
                  MAXWRK = MAX( MAXWRK, BDSPAC )
                  MINWRK = MAX( 4*M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVO .AND. WNTUN ) THEN
*
*                 Path 2t(N much larger than M, JOBU='N', JOBVT='O')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( M*M+WRKBL, M*M+M*N+M )
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVO .AND. WNTUAS ) THEN
*
*                 Path 3t(N much larger than M, JOBU='S' or 'A',
*                 JOBVT='O')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORGBR', 'Q', M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( M*M+WRKBL, M*M+M*N+M )
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVS .AND. WNTUN ) THEN
*
*                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVS .AND. WNTUO ) THEN
*
*                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORGBR', 'Q', M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVS .AND. WNTUAS ) THEN
*
*                 Path 6t(N much larger than M, JOBU='S' or 'A',
*                 JOBVT='S')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV (1, 'DORGLQ', ' ', M,
     $                    N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORGBR', 'Q', M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVA .AND. WNTUN ) THEN
*
*                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
*
                  WRKBL = M + M*ILAENV (1, 'DGELQF', ' ', M, N, -1)
                  WRKBL = MAX( WRKBL, M+N*ILAENV (1, 'DORGLQ', ' ', N,
     $                    N, -1))
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV (1, 'DGEBRD', ' ', M, M, -1))
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV (1, 'DORGBR', 'P', M, M, -1))
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVA .AND. WNTUO ) THEN
*
*                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
*
                  WRKBL = M + M*ILAENV (1, 'DGELQF', ' ', M, N, -1)
                  WRKBL = MAX( WRKBL, M+N*ILAENV (1, 'DORGLQ', ' ', N,
     $                    N, -1))
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV (1, 'DGEBRD', ' ', M, M, -1))
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV (1, 'DORGBR', 'P', M, M, -1))
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV (1, 'DORGBR', 'Q', M, M, -1))
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVA .AND. WNTUAS ) THEN
*
*                 Path 9t(N much larger than M, JOBU='S' or 'A',
*                 JOBVT='A')
*
                  WRKBL = M + M*ILAENV (1, 'DGELQF', ' ', M, N, -1)
                  WRKBL = MAX( WRKBL, M+N*ILAENV (1, 'DORGLQ', ' ', N,
     $                    N, -1))
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV (1, 'DGEBRD', ' ', M, M, -1))
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV (1, 'DORGBR', 'P', M, M, -1))
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV (1, 'DORGBR', 'Q', M, M, -1))
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               END IF
            ELSE
*
*              Path 10t(N greater than M, but not much larger)
*
               MAXWRK = 3*M + ( M+N )*ILAENV( 1, 'DGEBRD', ' ', M, N,
     $            -1)
               IF( WNTVS .OR. WNTVO )
     $            MAXWRK = MAX( MAXWRK, 3*M+M*
     $                     ILAENV (1, 'DORGBR', 'P', M, N, -1))
               IF( WNTVA )
     $            MAXWRK = MAX( MAXWRK, 3*M+N*
     $                     ILAENV (1, 'DORGBR', 'P', N, N, -1))
               IF( .NOT.WNTUN )
     $            MAXWRK = MAX( MAXWRK, 3*M+( M-1 )*
     $                     ILAENV (1, 'DORGBR', 'Q', M, M, -1))
               MAXWRK = MAX( MAXWRK, BDSPAC )
               MINWRK = MAX( 3*M+N, BDSPAC )
               MAXWRK = MAX( MAXWRK, MINWRK )
            END IF
         END IF
         WORK( 1 ) = MAXWRK
      END IF
*
      IF( LWORK.LT.MINWRK ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGESVD', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         IF( LWORK.GE.1 )
     $      WORK( 1 ) = ONE
         RETURN
      END IF
*
*     Get machine constants
*
      EPS = DLAMCH( 'P' )
      SMLNUM = SQRT( DLAMCH( 'S' ) ) / EPS
      BIGNUM = ONE / SMLNUM
*
*     Scale A if max element outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', M, N, A, LDA, DUM )
      ISCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ISCL = 1
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, IERR )
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ISCL = 1
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, IERR )
      END IF
*
      IF( M.GE.N ) THEN
*
*        A has at least as many rows as columns. If A has sufficiently
*        more rows than columns, first reduce using the QR
*        decomposition (if sufficient workspace available)
*
         IF( M.GE.MNTHR ) THEN
*
            IF( WNTUN ) THEN
*
*              Path 1 (M much larger than N, JOBU='N')
*              No left singular vectors to be computed
*
               ITAU = 1
               IWORK = ITAU + N
*
*              Compute A=Q*R
*              (Workspace: need 2*N, prefer N+N*NB)
*
               CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     $                      LWORK-IWORK+1, IERR )
*
*              Zero out below R
*
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA )
               IE = 1
               ITAUQ = IE + N
               ITAUP = ITAUQ + N
               IWORK = ITAUP + N
*
*              Bidiagonalize R in A
*              (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
               CALL DGEBRD( N, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                      IERR )
               NCVT = 0
               IF( WNTVO .OR. WNTVAS ) THEN
*
*                 If right singular vectors desired, generate P'.
*                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                  CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  NCVT = N
               END IF
               IWORK = IE + N
*
*              Perform bidiagonal QR iteration, computing right
*              singular vectors of A in A if desired
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', N, NCVT, 0, 0, S, WORK( IE ), A, LDA,
     $                      DUM, 1, DUM, 1, WORK( IWORK ), INFO )
*
*              If right singular vectors desired in VT, copy them there
*
               IF( WNTVAS )
     $            CALL DLACPY( 'F', N, N, A, LDA, VT, LDVT )
*
            ELSE IF( WNTUO .AND. WNTVN ) THEN
*
*              Path 2 (M much larger than N, JOBU='O', JOBVT='N')
*              N left singular vectors to be overwritten on A and
*              no right singular vectors to be computed
*
               IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+LDA*N ) THEN
*
*                    WORK(IU) is LDA by N, WORK(IR) is LDA by N
*
                     LDWRKU = LDA
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+N*N ) THEN
*
*                    WORK(IU) is LDA by N, WORK(IR) is N by N
*
                     LDWRKU = LDA
                     LDWRKR = N
                  ELSE
*
*                    WORK(IU) is LDWRKU by N, WORK(IR) is N by N
*
                     LDWRKU = ( LWORK-N*N-N ) / N
                     LDWRKR = N
                  END IF
                  ITAU = IR + LDWRKR*N
                  IWORK = ITAU + N
*
*                 Compute A=Q*R
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy R to WORK(IR) and zero out below it
*
                  CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ), LDWRKR )
                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, WORK( IR+1 ),
     $                         LDWRKR )
*
*                 Generate Q in A
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize R in WORK(IR)
*                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                  CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing R
*                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                  CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                         WORK( ITAUQ ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
                  IWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of R in WORK(IR)
*                 (Workspace: need N*N+BDSPAC)
*
                  CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM, 1,
     $                         WORK( IR ), LDWRKR, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + N
*
*                 Multiply Q in A by left singular vectors of R in
*                 WORK(IR), storing result in WORK(IU) and copying to A
*                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
*
                  DO 10 I = 1, M, LDWRKU
                     CHUNK = MIN( M-I+1, LDWRKU )
                     CALL DGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ),
     $                           LDA, WORK( IR ), LDWRKR, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', CHUNK, N, WORK( IU ), LDWRKU,
     $                            A( I, 1 ), LDA )
   10             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  IE = 1
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize A
*                 (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
*
                  CALL DGEBRD( M, N, A, LDA, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing A
*                 (Workspace: need 4*N, prefer 3*N+N*NB)
*
                  CALL DORGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of A in A
*                 (Workspace: need BDSPAC)
*
                  CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM, 1,
     $                         A, LDA, DUM, 1, WORK( IWORK ), INFO )
*
               END IF
*
            ELSE IF( WNTUO .AND. WNTVAS ) THEN
*
*              Path 3 (M much larger than N, JOBU='O', JOBVT='S' or 'A')
*              N left singular vectors to be overwritten on A and
*              N right singular vectors to be computed in VT
*
               IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+LDA*N ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is LDA by N
*
                     LDWRKU = LDA
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+N*N ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is N by N
*
                     LDWRKU = LDA
                     LDWRKR = N
                  ELSE
*
*                    WORK(IU) is LDWRKU by N and WORK(IR) is N by N
*
                     LDWRKU = ( LWORK-N*N-N ) / N
                     LDWRKR = N
                  END IF
                  ITAU = IR + LDWRKR*N
                  IWORK = ITAU + N
*
*                 Compute A=Q*R
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy R to VT, zeroing out below it
*
                  CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ),
     $                         LDVT )
*
*                 Generate Q in A
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize R in VT, copying result to WORK(IR)
*                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                  CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  CALL DLACPY( 'L', N, N, VT, LDVT, WORK( IR ), LDWRKR )
*
*                 Generate left vectors bidiagonalizing R in WORK(IR)
*                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                  CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                         WORK( ITAUQ ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing R in VT
*                 (Workspace: need N*N+4*N-1, prefer N*N+3*N+(N-1)*NB)
*
                  CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of R in WORK(IR) and computing right
*                 singular vectors of R in VT
*                 (Workspace: need N*N+BDSPAC)
*
                  CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT, LDVT,
     $                         WORK( IR ), LDWRKR, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + N
*
*                 Multiply Q in A by left singular vectors of R in
*                 WORK(IR), storing result in WORK(IU) and copying to A
*                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
*
                  DO 20 I = 1, M, LDWRKU
                     CHUNK = MIN( M-I+1, LDWRKU )
                     CALL DGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ),
     $                           LDA, WORK( IR ), LDWRKR, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', CHUNK, N, WORK( IU ), LDWRKU,
     $                            A( I, 1 ), LDA )
   20             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  ITAU = 1
                  IWORK = ITAU + N
*
*                 Compute A=Q*R
*                 (Workspace: need 2*N, prefer N+N*NB)
*
                  CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy R to VT, zeroing out below it
*
                  CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ),
     $                         LDVT )
*
*                 Generate Q in A
*                 (Workspace: need 2*N, prefer N+N*NB)
*
                  CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize R in VT
*                 (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                  CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Multiply Q in A by left vectors bidiagonalizing R
*                 (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                  CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                         WORK( ITAUQ ), A, LDA, WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing R in VT
*                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                  CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of A in A and computing right
*                 singular vectors of A in VT
*                 (Workspace: need BDSPAC)
*
                  CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT, LDVT,
     $                         A, LDA, DUM, 1, WORK( IWORK ), INFO )
*
               END IF
*
            ELSE IF( WNTUS ) THEN
*
               IF( WNTVN ) THEN
*
*                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
*                 N left singular vectors to be computed in U and
*                 no right singular vectors to be computed
*
                  IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
*
*                       WORK(IR) is LDA by N
*
                        LDWRKR = LDA
                     ELSE
*
*                       WORK(IR) is N by N
*
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IR), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IR+1 ), LDWRKR )
*
*                    Generate Q in A
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IR)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left vectors bidiagonalizing R in WORK(IR)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IR)
*                    (Workspace: need N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM,
     $                            1, WORK( IR ), LDWRKR, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply Q in A by left singular vectors of R in
*                    WORK(IR), storing result in U
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
     $                           WORK( IR ), LDWRKR, ZERO, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
*
*                    Bidiagonalize R in A
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left vectors bidiagonalizing R
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM,
     $                            1, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTVO ) THEN
*
*                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
*                 N left singular vectors to be computed in U and
*                 N right singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*N*N+MAX( 4*N, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*N ) THEN
*
*                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+N )*N ) THEN
*
*                       WORK(IU) is LDA by N and WORK(IR) is N by N
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     ELSE
*
*                       WORK(IU) is N by N and WORK(IR) is N by N
*
                        LDWRKU = N
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R
*                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
*
*                    Generate Q in A
*                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
*
                     CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to
*                    WORK(IR)
*                    (Workspace: need 2*N*N+4*N,
*                                prefer 2*N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need 2*N*N+4*N-1,
*                                prefer 2*N*N+3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in WORK(IR)
*                    (Workspace: need 2*N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, WORK( IU ),
     $                            LDWRKU, DUM, 1, WORK( IWORK ), INFO )
*
*                    Multiply Q in A by left singular vectors of R in
*                    WORK(IU), storing result in U
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
     $                           WORK( IU ), LDWRKU, ZERO, U, LDU )
*
*                    Copy right singular vectors of R to A
*                    (Workspace: need N*N)
*
                     CALL DLACPY( 'F', N, N, WORK( IR ), LDWRKR, A,
     $                            LDA )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
*
*                    Bidiagonalize R in A
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left vectors bidiagonalizing R
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right vectors bidiagonalizing R in A
*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in A
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), A,
     $                            LDA, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTVAS ) THEN
*
*                 Path 6 (M much larger than N, JOBU='S', JOBVT='S'
*                         or 'A')
*                 N left singular vectors to be computed in U and
*                 N right singular vectors to be computed in VT
*
                  IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
*
*                       WORK(IU) is LDA by N
*
                        LDWRKU = LDA
                     ELSE
*
*                       WORK(IU) is N by N
*
                        LDWRKU = N
                     END IF
                     ITAU = IU + LDWRKU*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
*
*                    Generate Q in A
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to VT
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT,
     $                            LDVT )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (Workspace: need N*N+4*N-1,
*                                prefer N*N+3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in VT
*                    (Workspace: need N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT,
     $                            LDVT, WORK( IU ), LDWRKU, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply Q in A by left singular vectors of R in
*                    WORK(IU), storing result in U
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
     $                           WORK( IU ), LDWRKU, ZERO, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to VT, zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ),
     $                            LDVT )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in VT
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in VT
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               END IF
*
            ELSE IF( WNTUA ) THEN
*
               IF( WNTVN ) THEN
*
*                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
*                 M left singular vectors to be computed in U and
*                 no right singular vectors to be computed
*
                  IF( LWORK.GE.N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
*
*                       WORK(IR) is LDA by N
*
                        LDWRKR = LDA
                     ELSE
*
*                       WORK(IR) is N by N
*
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Copy R to WORK(IR), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IR+1 ), LDWRKR )
*
*                    Generate Q in U
*                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IR)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IR)
*                    (Workspace: need N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM,
     $                            1, WORK( IR ), LDWRKR, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply Q in U by left singular vectors of R in
*                    WORK(IR), storing result in A
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
     $                           WORK( IR ), LDWRKR, ZERO, A, LDA )
*
*                    Copy left singular vectors of A from A to U
*
                     CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need N+M, prefer N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
*
*                    Bidiagonalize R in A
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in A
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM,
     $                            1, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTVO ) THEN
*
*                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
*                 M left singular vectors to be computed in U and
*                 N right singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*N ) THEN
*
*                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+N )*N ) THEN
*
*                       WORK(IU) is LDA by N and WORK(IR) is N by N
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     ELSE
*
*                       WORK(IU) is N by N and WORK(IR) is N by N
*
                        LDWRKU = N
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need 2*N*N+N+M, prefer 2*N*N+N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to
*                    WORK(IR)
*                    (Workspace: need 2*N*N+4*N,
*                                prefer 2*N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need 2*N*N+4*N-1,
*                                prefer 2*N*N+3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in WORK(IR)
*                    (Workspace: need 2*N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, WORK( IU ),
     $                            LDWRKU, DUM, 1, WORK( IWORK ), INFO )
*
*                    Multiply Q in U by left singular vectors of R in
*                    WORK(IU), storing result in A
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
     $                           WORK( IU ), LDWRKU, ZERO, A, LDA )
*
*                    Copy left singular vectors of A from A to U
*
                     CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
*
*                    Copy right singular vectors of R from WORK(IR) to A
*
                     CALL DLACPY( 'F', N, N, WORK( IR ), LDWRKR, A,
     $                            LDA )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need N+M, prefer N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
*
*                    Bidiagonalize R in A
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in A
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in A
*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in A
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), A,
     $                            LDA, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTVAS ) THEN
*
*                 Path 9 (M much larger than N, JOBU='A', JOBVT='S'
*                         or 'A')
*                 M left singular vectors to be computed in U and
*                 N right singular vectors to be computed in VT
*
                  IF( LWORK.GE.N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
*
*                       WORK(IU) is LDA by N
*
                        LDWRKU = LDA
                     ELSE
*
*                       WORK(IU) is N by N
*
                        LDWRKU = N
                     END IF
                     ITAU = IU + LDWRKU*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to VT
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT,
     $                            LDVT )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (Workspace: need N*N+4*N-1,
*                                prefer N*N+3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in VT
*                    (Workspace: need N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT,
     $                            LDVT, WORK( IU ), LDWRKU, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply Q in U by left singular vectors of R in
*                    WORK(IU), storing result in A
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
     $                           WORK( IU ), LDWRKU, ZERO, A, LDA )
*
*                    Copy left singular vectors of A from A to U
*
                     CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need N+M, prefer N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R from A to VT, zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ),
     $                            LDVT )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in VT
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in VT
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               END IF
*
            END IF
*
         ELSE
*
*           M .LT. MNTHR
*
*           Path 10 (M at least N, but not much larger)
*           Reduce to bidiagonal form without QR decomposition
*
            IE = 1
            ITAUQ = IE + N
            ITAUP = ITAUQ + N
            IWORK = ITAUP + N
*
*           Bidiagonalize A
*           (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
*
            CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                   IERR )
            IF( WNTUAS ) THEN
*
*              If left singular vectors desired in U, copy result to U
*              and generate left bidiagonalizing vectors in U
*              (Workspace: need 3*N+NCU, prefer 3*N+NCU*NB)
*
               CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
               IF( WNTUS )
     $            NCU = N
               IF( WNTUA )
     $            NCU = M
               CALL DORGBR( 'Q', M, NCU, N, U, LDU, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVAS ) THEN
*
*              If right singular vectors desired in VT, copy result to
*              VT and generate right bidiagonalizing vectors in VT
*              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
               CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
               CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTUO ) THEN
*
*              If left singular vectors desired in A, generate left
*              bidiagonalizing vectors in A
*              (Workspace: need 4*N, prefer 3*N+N*NB)
*
               CALL DORGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVO ) THEN
*
*              If right singular vectors desired in A, generate right
*              bidiagonalizing vectors in A
*              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
               CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IWORK = IE + N
            IF( WNTUAS .OR. WNTUO )
     $         NRU = M
            IF( WNTUN )
     $         NRU = 0
            IF( WNTVAS .OR. WNTVO )
     $         NCVT = N
            IF( WNTVN )
     $         NCVT = 0
            IF( ( .NOT.WNTUO ) .AND. ( .NOT.WNTVO ) ) THEN
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in VT
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE IF( ( .NOT.WNTUO ) .AND. WNTVO ) THEN
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in A
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), A, LDA,
     $                      U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in A and computing right singular
*              vectors in VT
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, A, LDA, DUM, 1, WORK( IWORK ), INFO )
            END IF
*
         END IF
*
      ELSE
*
*        A has more columns than rows. If A has sufficiently more
*        columns than rows, first reduce using the LQ decomposition (if
*        sufficient workspace available)
*
         IF( N.GE.MNTHR ) THEN
*
            IF( WNTVN ) THEN
*
*              Path 1t(N much larger than M, JOBVT='N')
*              No right singular vectors to be computed
*
               ITAU = 1
               IWORK = ITAU + M
*
*              Compute A=L*Q
*              (Workspace: need 2*M, prefer M+M*NB)
*
               CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     $                      LWORK-IWORK+1, IERR )
*
*              Zero out above L
*
               CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), LDA )
               IE = 1
               ITAUQ = IE + M
               ITAUP = ITAUQ + M
               IWORK = ITAUP + M
*
*              Bidiagonalize L in A
*              (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
               CALL DGEBRD( M, M, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                      IERR )
               IF( WNTUO .OR. WNTUAS ) THEN
*
*                 If left singular vectors desired, generate Q
*                 (Workspace: need 4*M, prefer 3*M+M*NB)
*
                  CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
               END IF
               IWORK = IE + M
               NRU = 0
               IF( WNTUO .OR. WNTUAS )
     $            NRU = M
*
*              Perform bidiagonal QR iteration, computing left singular
*              vectors of A in A if desired
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', M, 0, NRU, 0, S, WORK( IE ), DUM, 1, A,
     $                      LDA, DUM, 1, WORK( IWORK ), INFO )
*
*              If left singular vectors desired in U, copy them there
*
               IF( WNTUAS )
     $            CALL DLACPY( 'F', M, M, A, LDA, U, LDU )
*
            ELSE IF( WNTVO .AND. WNTUN ) THEN
*
*              Path 2t(N much larger than M, JOBU='N', JOBVT='O')
*              M right singular vectors to be overwritten on A and
*              no left singular vectors to be computed
*
               IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+LDA*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+M*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is M by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = M
                  ELSE
*
*                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
*
                     LDWRKU = M
                     CHUNK = ( LWORK-M*M-M ) / M
                     LDWRKR = M
                  END IF
                  ITAU = IR + LDWRKR*M
                  IWORK = ITAU + M
*
*                 Compute A=L*Q
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy L to WORK(IR) and zero out above it
*
                  CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ), LDWRKR )
                  CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                         WORK( IR+LDWRKR ), LDWRKR )
*
*                 Generate Q in A
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize L in WORK(IR)
*                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                  CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing L
*                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
*
                  CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                         WORK( ITAUP ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
                  IWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing right
*                 singular vectors of L in WORK(IR)
*                 (Workspace: need M*M+BDSPAC)
*
                  CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
     $                         WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + M
*
*                 Multiply right singular vectors of L in WORK(IR) by Q
*                 in A, storing result in WORK(IU) and copying to A
*                 (Workspace: need M*M+2*M, prefer M*M+M*N+M)
*
                  DO 30 I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IR ),
     $                           LDWRKR, A( 1, I ), LDA, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', M, BLK, WORK( IU ), LDWRKU,
     $                            A( 1, I ), LDA )
   30             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  IE = 1
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize A
*                 (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
*
                  CALL DGEBRD( M, N, A, LDA, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing A
*                 (Workspace: need 4*M, prefer 3*M+M*NB)
*
                  CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing right
*                 singular vectors of A in A
*                 (Workspace: need BDSPAC)
*
                  CALL DBDSQR( 'L', M, N, 0, 0, S, WORK( IE ), A, LDA,
     $                         DUM, 1, DUM, 1, WORK( IWORK ), INFO )
*
               END IF
*
            ELSE IF( WNTVO .AND. WNTUAS ) THEN
*
*              Path 3t(N much larger than M, JOBU='S' or 'A', JOBVT='O')
*              M right singular vectors to be overwritten on A and
*              M left singular vectors to be computed in U
*
               IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+LDA*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+M*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is M by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = M
                  ELSE
*
*                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
*
                     LDWRKU = M
                     CHUNK = ( LWORK-M*M-M ) / M
                     LDWRKR = M
                  END IF
                  ITAU = IR + LDWRKR*M
                  IWORK = ITAU + M
*
*                 Compute A=L*Q
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy L to U, zeroing about above it
*
                  CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                  CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                         LDU )
*
*                 Generate Q in A
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize L in U, copying result to WORK(IR)
*                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                  CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  CALL DLACPY( 'U', M, M, U, LDU, WORK( IR ), LDWRKR )
*
*                 Generate right vectors bidiagonalizing L in WORK(IR)
*                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
*
                  CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                         WORK( ITAUP ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing L in U
*                 (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
*
                  CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of L in U, and computing right
*                 singular vectors of L in WORK(IR)
*                 (Workspace: need M*M+BDSPAC)
*
                  CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                         WORK( IR ), LDWRKR, U, LDU, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + M
*
*                 Multiply right singular vectors of L in WORK(IR) by Q
*                 in A, storing result in WORK(IU) and copying to A
*                 (Workspace: need M*M+2*M, prefer M*M+M*N+M))
*
                  DO 40 I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IR ),
     $                           LDWRKR, A( 1, I ), LDA, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', M, BLK, WORK( IU ), LDWRKU,
     $                            A( 1, I ), LDA )
   40             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  ITAU = 1
                  IWORK = ITAU + M
*
*                 Compute A=L*Q
*                 (Workspace: need 2*M, prefer M+M*NB)
*
                  CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy L to U, zeroing out above it
*
                  CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                  CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                         LDU )
*
*                 Generate Q in A
*                 (Workspace: need 2*M, prefer M+M*NB)
*
                  CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize L in U
*                 (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                  CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Multiply right vectors bidiagonalizing L by Q in A
*                 (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                  CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
     $                         WORK( ITAUP ), A, LDA, WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing L in U
*                 (Workspace: need 4*M, prefer 3*M+M*NB)
*
                  CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of A in U and computing right
*                 singular vectors of A in A
*                 (Workspace: need BDSPAC)
*
                  CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), A, LDA,
     $                         U, LDU, DUM, 1, WORK( IWORK ), INFO )
*
               END IF
*
            ELSE IF( WNTVS ) THEN
*
               IF( WNTUN ) THEN
*
*                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
*                 M right singular vectors to be computed in VT and
*                 no left singular vectors to be computed
*
                  IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
*
*                       WORK(IR) is LDA by M
*
                        LDWRKR = LDA
                     ELSE
*
*                       WORK(IR) is M by M
*
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IR), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IR+LDWRKR ), LDWRKR )
*
*                    Generate Q in A
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IR)
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right vectors bidiagonalizing L in
*                    WORK(IR)
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of L in WORK(IR)
*                    (Workspace: need M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IR) by
*                    Q in A, storing result in VT
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IR ),
     $                           LDWRKR, A, LDA, ZERO, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy result to VT
*
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
*
*                    Bidiagonalize L in A
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right vectors bidiagonalizing L by Q in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, 0, 0, S, WORK( IE ), VT,
     $                            LDVT, DUM, 1, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTUO ) THEN
*
*                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
*                 M right singular vectors to be computed in VT and
*                 M left singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*M*M+MAX( 4*M, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*M ) THEN
*
*                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+M )*M ) THEN
*
*                       WORK(IU) is LDA by M and WORK(IR) is M by M
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     ELSE
*
*                       WORK(IU) is M by M and WORK(IR) is M by M
*
                        LDWRKU = M
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q
*                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
*
*                    Generate Q in A
*                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
*
                     CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to
*                    WORK(IR)
*                    (Workspace: need 2*M*M+4*M,
*                                prefer 2*M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need 2*M*M+4*M-1,
*                                prefer 2*M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in WORK(IR) and computing
*                    right singular vectors of L in WORK(IU)
*                    (Workspace: need 2*M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, WORK( IR ),
     $                            LDWRKR, DUM, 1, WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in A, storing result in VT
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, A, LDA, ZERO, VT, LDVT )
*
*                    Copy left singular vectors of L to A
*                    (Workspace: need M*M)
*
                     CALL DLACPY( 'F', M, M, WORK( IR ), LDWRKR, A,
     $                            LDA )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
*
*                    Bidiagonalize L in A
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right vectors bidiagonalizing L by Q in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors of L in A
*                    (Workspace: need 4*M, prefer 3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, compute left
*                    singular vectors of A in A and compute right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, A, LDA, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTUAS ) THEN
*
*                 Path 6t(N much larger than M, JOBU='S' or 'A',
*                         JOBVT='S')
*                 M right singular vectors to be computed in VT and
*                 M left singular vectors to be computed in U
*
                  IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
*
*                       WORK(IU) is LDA by N
*
                        LDWRKU = LDA
                     ELSE
*
*                       WORK(IU) is LDA by M
*
                        LDWRKU = M
                     END IF
                     ITAU = IU + LDWRKU*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
*
*                    Generate Q in A
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to U
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU, U,
     $                            LDU )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need M*M+4*M-1,
*                                prefer M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in U and computing right
*                    singular vectors of L in WORK(IU)
*                    (Workspace: need M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, U, LDU, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in A, storing result in VT
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, A, LDA, ZERO, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to U, zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                            LDU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in U
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in U by Q
*                    in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (Workspace: need 4*M, prefer 3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               END IF
*
            ELSE IF( WNTVA ) THEN
*
               IF( WNTUN ) THEN
*
*                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
*                 N right singular vectors to be computed in VT and
*                 no left singular vectors to be computed
*
                  IF( LWORK.GE.M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
*
*                       WORK(IR) is LDA by M
*
                        LDWRKR = LDA
                     ELSE
*
*                       WORK(IR) is M by M
*
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Copy L to WORK(IR), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IR+LDWRKR ), LDWRKR )
*
*                    Generate Q in VT
*                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IR)
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need M*M+4*M-1,
*                                prefer M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of L in WORK(IR)
*                    (Workspace: need M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IR) by
*                    Q in VT, storing result in A
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IR ),
     $                           LDWRKR, VT, LDVT, ZERO, A, LDA )
*
*                    Copy right singular vectors of A from A to VT
*
                     CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need M+N, prefer M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
*
*                    Bidiagonalize L in A
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in A by Q
*                    in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, 0, 0, S, WORK( IE ), VT,
     $                            LDVT, DUM, 1, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTUO ) THEN
*
*                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
*                 N right singular vectors to be computed in VT and
*                 M left singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*M ) THEN
*
*                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+M )*M ) THEN
*
*                       WORK(IU) is LDA by M and WORK(IR) is M by M
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     ELSE
*
*                       WORK(IU) is M by M and WORK(IR) is M by M
*
                        LDWRKU = M
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need 2*M*M+M+N, prefer 2*M*M+M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to
*                    WORK(IR)
*                    (Workspace: need 2*M*M+4*M,
*                                prefer 2*M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need 2*M*M+4*M-1,
*                                prefer 2*M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in WORK(IR) and computing
*                    right singular vectors of L in WORK(IU)
*                    (Workspace: need 2*M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, WORK( IR ),
     $                            LDWRKR, DUM, 1, WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in VT, storing result in A
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, VT, LDVT, ZERO, A, LDA )
*
*                    Copy right singular vectors of A from A to VT
*
                     CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
*                    Copy left singular vectors of A from WORK(IR) to A
*
                     CALL DLACPY( 'F', M, M, WORK( IR ), LDWRKR, A,
     $                            LDA )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need M+N, prefer M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
*
*                    Bidiagonalize L in A
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in A by Q
*                    in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in A
*                    (Workspace: need 4*M, prefer 3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in A and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, A, LDA, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTUAS ) THEN
*
*                 Path 9t(N much larger than M, JOBU='S' or 'A',
*                         JOBVT='A')
*                 N right singular vectors to be computed in VT and
*                 M left singular vectors to be computed in U
*
                  IF( LWORK.GE.M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
*
*                       WORK(IU) is LDA by M
*
                        LDWRKU = LDA
                     ELSE
*
*                       WORK(IU) is M by M
*
                        LDWRKU = M
                     END IF
                     ITAU = IU + LDWRKU*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to U
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU, U,
     $                            LDU )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in U and computing right
*                    singular vectors of L in WORK(IU)
*                    (Workspace: need M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, U, LDU, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in VT, storing result in A
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, VT, LDVT, ZERO, A, LDA )
*
*                    Copy right singular vectors of A from A to VT
*
                     CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need M+N, prefer M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to U, zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                            LDU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in U
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in U by Q
*                    in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (Workspace: need 4*M, prefer 3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               END IF
*
            END IF
*
         ELSE
*
*           N .LT. MNTHR
*
*           Path 10t(N greater than M, but not much larger)
*           Reduce to bidiagonal form without LQ decomposition
*
            IE = 1
            ITAUQ = IE + M
            ITAUP = ITAUQ + M
            IWORK = ITAUP + M
*
*           Bidiagonalize A
*           (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
*
            CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                   IERR )
            IF( WNTUAS ) THEN
*
*              If left singular vectors desired in U, copy result to U
*              and generate left bidiagonalizing vectors in U
*              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
*
               CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
               CALL DORGBR( 'Q', M, M, N, U, LDU, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVAS ) THEN
*
*              If right singular vectors desired in VT, copy result to
*              VT and generate right bidiagonalizing vectors in VT
*              (Workspace: need 3*M+NRVT, prefer 3*M+NRVT*NB)
*
               CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
               IF( WNTVA )
     $            NRVT = N
               IF( WNTVS )
     $            NRVT = M
               CALL DORGBR( 'P', NRVT, N, M, VT, LDVT, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTUO ) THEN
*
*              If left singular vectors desired in A, generate left
*              bidiagonalizing vectors in A
*              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
*
               CALL DORGBR( 'Q', M, M, N, A, LDA, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVO ) THEN
*
*              If right singular vectors desired in A, generate right
*              bidiagonalizing vectors in A
*              (Workspace: need 4*M, prefer 3*M+M*NB)
*
               CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IWORK = IE + M
            IF( WNTUAS .OR. WNTUO )
     $         NRU = M
            IF( WNTUN )
     $         NRU = 0
            IF( WNTVAS .OR. WNTVO )
     $         NCVT = N
            IF( WNTVN )
     $         NCVT = 0
            IF( ( .NOT.WNTUO ) .AND. ( .NOT.WNTVO ) ) THEN
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in VT
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE IF( ( .NOT.WNTUO ) .AND. WNTVO ) THEN
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in A
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), A, LDA,
     $                      U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in A and computing right singular
*              vectors in VT
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, A, LDA, DUM, 1, WORK( IWORK ), INFO )
            END IF
*
         END IF
*
      END IF
*
*     If DBDSQR failed to converge, copy unconverged superdiagonals
*     to WORK( 2:MINMN )
*
      IF( INFO.NE.0 ) THEN
         IF( IE.GT.2 ) THEN
            DO 50 I = 1, MINMN - 1
               WORK( I+1 ) = WORK( I+IE-1 )
   50       CONTINUE
         END IF
         IF( IE.LT.2 ) THEN
            DO 60 I = MINMN - 1, 1, -1
               WORK( I+1 ) = WORK( I+IE-1 )
   60       CONTINUE
         END IF
      END IF
*
*     Undo scaling if necessary
*
      IF( ISCL.EQ.1 ) THEN
         IF( ANRM.GT.BIGNUM )
     $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
         IF( INFO.NE.0 .AND. ANRM.GT.BIGNUM )
     $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN-1, 1, WORK( 2 ),
     $                   MINMN, IERR )
         IF( ANRM.LT.SMLNUM )
     $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
         IF( INFO.NE.0 .AND. ANRM.LT.SMLNUM )
     $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN-1, 1, WORK( 2 ),
     $                   MINMN, IERR )
      END IF
*
*     Return optimal workspace in WORK(1)
*
      WORK( 1 ) = MAXWRK
*
      RETURN
*
*     End of DGESVD
*
      END
      SUBROUTINE DBDSQR( UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,
     $                   LDU, C, LDC, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDC, LDU, LDVT, N, NCC, NCVT, NRU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), D( * ), E( * ), U( LDU, * ),
     $                   VT( LDVT, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DBDSQR computes the singular value decomposition (SVD) of a real
*  N-by-N (upper or lower) bidiagonal matrix B:  B = Q * S * P' (P'
*  denotes the transpose of P), where S is a diagonal matrix with
*  non-negative diagonal elements (the singular values of B), and Q
*  and P are orthogonal matrices.
*
*  The routine computes S, and optionally computes U * Q, P' * VT,
*  or Q' * C, for given real input matrices U, VT, and C.
*
*  See "Computing  Small Singular Values of Bidiagonal Matrices With
*  Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
*  LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
*  no. 5, pp. 873-912, Sept 1990) and
*  "Accurate singular values and differential qd algorithms," by
*  B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
*  Department, University of California at Berkeley, July 1992
*  for a detailed description of the algorithm.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  B is upper bidiagonal;
*          = 'L':  B is lower bidiagonal.
*
*  N       (input) INTEGER
*          The order of the matrix B.  N >= 0.
*
*  NCVT    (input) INTEGER
*          The number of columns of the matrix VT. NCVT >= 0.
*
*  NRU     (input) INTEGER
*          The number of rows of the matrix U. NRU >= 0.
*
*  NCC     (input) INTEGER
*          The number of columns of the matrix C. NCC >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the bidiagonal matrix B.
*          On exit, if INFO=0, the singular values of B in decreasing
*          order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the elements of E contain the
*          offdiagonal elements of the bidiagonal matrix whose SVD
*          is desired. On normal exit (INFO = 0), E is destroyed.
*          If the algorithm does not converge (INFO > 0), D and E
*          will contain the diagonal and superdiagonal elements of a
*          bidiagonal matrix orthogonally equivalent to the one given
*          as input. E(N) is used for workspace.
*
*  VT      (input/output) DOUBLE PRECISION array, dimension (LDVT, NCVT)
*          On entry, an N-by-NCVT matrix VT.
*          On exit, VT is overwritten by P' * VT.
*          VT is not referenced if NCVT = 0.
*
*  LDVT    (input) INTEGER
*          The leading dimension of the array VT.
*          LDVT >= max(1,N) if NCVT > 0; LDVT >= 1 if NCVT = 0.
*
*  U       (input/output) DOUBLE PRECISION array, dimension (LDU, N)
*          On entry, an NRU-by-N matrix U.
*          On exit, U is overwritten by U * Q.
*          U is not referenced if NRU = 0.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= max(1,NRU).
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC, NCC)
*          On entry, an N-by-NCC matrix C.
*          On exit, C is overwritten by Q' * C.
*          C is not referenced if NCC = 0.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C.
*          LDC >= max(1,N) if NCC > 0; LDC >=1 if NCC = 0.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*            2*N  if only singular values wanted (NCVT = NRU = NCC = 0)
*            max( 1, 4*N-4 ) otherwise
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  If INFO = -i, the i-th argument had an illegal value
*          > 0:  the algorithm did not converge; D and E contain the
*                elements of a bidiagonal matrix which is orthogonally
*                similar to the input matrix B;  if INFO = i, i
*                elements of E have not converged to zero.
*
*  Internal Parameters
*  ===================
*
*  TOLMUL  DOUBLE PRECISION, default = max(10,min(100,EPS**(-1/8)))
*          TOLMUL controls the convergence criterion of the QR loop.
*          If it is positive, TOLMUL*EPS is the desired relative
*             precision in the computed singular values.
*          If it is negative, abs(TOLMUL*EPS*sigma_max) is the
*             desired absolute accuracy in the computed singular
*             values (corresponds to relative accuracy
*             abs(TOLMUL*EPS) in the largest singular value.
*          abs(TOLMUL) should be between 1 and 1/EPS, and preferably
*             between 10 (for fast convergence) and .1/EPS
*             (for there to be some accuracy in the results).
*          Default is to lose at either one eighth or 2 of the
*             available decimal digits in each computed singular value
*             (whichever is smaller).
*
*  MAXITR  INTEGER, default = 6
*          MAXITR controls the maximum number of passes of the
*          algorithm through its inner loop. The algorithms stops
*          (and so fails to converge) if the number of passes
*          through the inner loop exceeds MAXITR*N**2.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   NEGONE
      PARAMETER          ( NEGONE = -1.0D0 )
      DOUBLE PRECISION   HNDRTH
      PARAMETER          ( HNDRTH = 0.01D0 )
      DOUBLE PRECISION   TEN
      PARAMETER          ( TEN = 10.0D0 )
      DOUBLE PRECISION   HNDRD
      PARAMETER          ( HNDRD = 100.0D0 )
      DOUBLE PRECISION   MEIGTH
      PARAMETER          ( MEIGTH = -0.125D0 )
      INTEGER            MAXITR
      PARAMETER          ( MAXITR = 6 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ROTATE
      INTEGER            I, IDIR, IROT, ISUB, ITER, IUPLO, J, LL, LLL,
     $                   M, MAXIT, NM1, NM12, NM13, OLDLL, OLDM
      DOUBLE PRECISION   ABSE, ABSS, COSL, COSR, CS, EPS, F, G, H, MU,
     $                   OLDCS, OLDSN, R, SHIFT, SIGMN, SIGMX, SINL,
     $                   SINR, SLL, SMAX, SMIN, SMINL, SMINLO, SMINOA,
     $                   SN, THRESH, TOL, TOLMUL, UNFL
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARTG, DLAS2, DLASQ1, DLASR, DLASV2, DROT,
     $                   DSCAL, DSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IUPLO = 0
      IF( LSAME( UPLO, 'U' ) )
     $   IUPLO = 1
      IF( LSAME( UPLO, 'L' ) )
     $   IUPLO = 2
      IF( IUPLO.EQ.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NCVT.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRU.LT.0 ) THEN
         INFO = -4
      ELSE IF( NCC.LT.0 ) THEN
         INFO = -5
      ELSE IF( ( NCVT.EQ.0 .AND. LDVT.LT.1 ) .OR.
     $         ( NCVT.GT.0 .AND. LDVT.LT.MAX( 1, N ) ) ) THEN
         INFO = -9
      ELSE IF( LDU.LT.MAX( 1, NRU ) ) THEN
         INFO = -11
      ELSE IF( ( NCC.EQ.0 .AND. LDC.LT.1 ) .OR.
     $         ( NCC.GT.0 .AND. LDC.LT.MAX( 1, N ) ) ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DBDSQR', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      IF( N.EQ.1 )
     $   GO TO 150
*
*     ROTATE is true if any singular vectors desired, false otherwise
*
      ROTATE = ( NCVT.GT.0 ) .OR. ( NRU.GT.0 ) .OR. ( NCC.GT.0 )
*
*     If no singular vectors desired, use qd algorithm
*
      IF( .NOT.ROTATE ) THEN
         CALL DLASQ1( N, D, E, WORK, INFO )
         RETURN
      END IF
*
      NM1 = N - 1
      NM12 = NM1 + NM1
      NM13 = NM12 + NM1
*
*     Get machine constants
*
      EPS = DLAMCH( 'Epsilon' )
      UNFL = DLAMCH( 'Safe minimum' )
*
*     If matrix lower bidiagonal, rotate to be upper bidiagonal
*     by applying Givens rotations on the left
*
      IF( IUPLO.EQ.2 ) THEN
         DO 10 I = 1, N - 1
            CALL DLARTG( D( I ), E( I ), CS, SN, R )
            D( I ) = R
            E( I ) = SN*D( I+1 )
            D( I+1 ) = CS*D( I+1 )
            WORK( I ) = CS
            WORK( NM1+I ) = SN
   10    CONTINUE
*
*        Update singular vectors if desired
*
         IF( NRU.GT.0 )
     $      CALL DLASR( 'R', 'V', 'F', NRU, N, WORK( 1 ), WORK( N ), U,
     $                  LDU )
         IF( NCC.GT.0 )
     $      CALL DLASR( 'L', 'V', 'F', N, NCC, WORK( 1 ), WORK( N ), C,
     $                  LDC )
      END IF
*
*     Compute singular values to relative accuracy TOL
*     (By setting TOL to be negative, algorithm will compute
*     singular values to absolute accuracy ABS(TOL)*norm(input matrix))
*
      TOLMUL = MAX( TEN, MIN( HNDRD, EPS**MEIGTH ) )
      TOL = TOLMUL*EPS
*
*     Compute approximate maximum, minimum singular values
*
      SMAX = ABS( D( N ) )
      DO 20 I = 1, N - 1
         SMAX = MAX( SMAX, ABS( D( I ) ), ABS( E( I ) ) )
   20 CONTINUE
      SMINL = ZERO
      IF( TOL.GE.ZERO ) THEN
*
*        Relative accuracy desired
*
         SMINOA = ABS( D( 1 ) )
         IF( SMINOA.EQ.ZERO )
     $      GO TO 40
         MU = SMINOA
         DO 30 I = 2, N
            MU = ABS( D( I ) )*( MU / ( MU+ABS( E( I-1 ) ) ) )
            SMINOA = MIN( SMINOA, MU )
            IF( SMINOA.EQ.ZERO )
     $         GO TO 40
   30    CONTINUE
   40    CONTINUE
         SMINOA = SMINOA / SQRT( DBLE( N ) )
         THRESH = MAX( TOL*SMINOA, MAXITR*N*N*UNFL )
      ELSE
*
*        Absolute accuracy desired
*
         THRESH = MAX( ABS( TOL )*SMAX, MAXITR*N*N*UNFL )
      END IF
*
*     Prepare for main iteration loop for the singular values
*     (MAXIT is the maximum number of passes through the inner
*     loop permitted before nonconvergence signalled.)
*
      MAXIT = MAXITR*N*N
      ITER = 0
      OLDLL = -1
      OLDM = -1
*
*     M points to last element of unconverged part of matrix
*
      M = N
*
*     Begin main iteration loop
*
   50 CONTINUE
*
*     Check for convergence or exceeding iteration count
*
      IF( M.LE.1 )
     $   GO TO 150
      IF( ITER.GT.MAXIT )
     $   GO TO 190
*
*     Find diagonal block of matrix to work on
*
      IF( TOL.LT.ZERO .AND. ABS( D( M ) ).LE.THRESH )
     $   D( M ) = ZERO
      SMAX = ABS( D( M ) )
      SMIN = SMAX
      DO 60 LLL = 1, M
         LL = M - LLL
         IF( LL.EQ.0 )
     $      GO TO 80
         ABSS = ABS( D( LL ) )
         ABSE = ABS( E( LL ) )
         IF( TOL.LT.ZERO .AND. ABSS.LE.THRESH )
     $      D( LL ) = ZERO
         IF( ABSE.LE.THRESH )
     $      GO TO 70
         SMIN = MIN( SMIN, ABSS )
         SMAX = MAX( SMAX, ABSS, ABSE )
   60 CONTINUE
   70 CONTINUE
      E( LL ) = ZERO
*
*     Matrix splits since E(LL) = 0
*
      IF( LL.EQ.M-1 ) THEN
*
*        Convergence of bottom singular value, return to top of loop
*
         M = M - 1
         GO TO 50
      END IF
   80 CONTINUE
      LL = LL + 1
*
*     E(LL) through E(M-1) are nonzero, E(LL-1) is zero
*
      IF( LL.EQ.M-1 ) THEN
*
*        2 by 2 block, handle separately
*
         CALL DLASV2( D( M-1 ), E( M-1 ), D( M ), SIGMN, SIGMX, SINR,
     $                COSR, SINL, COSL )
         D( M-1 ) = SIGMX
         E( M-1 ) = ZERO
         D( M ) = SIGMN
*
*        Compute singular vectors, if desired
*
         IF( NCVT.GT.0 )
     $      CALL DROT( NCVT, VT( M-1, 1 ), LDVT, VT( M, 1 ), LDVT, COSR,
     $                 SINR )
         IF( NRU.GT.0 )
     $      CALL DROT( NRU, U( 1, M-1 ), 1, U( 1, M ), 1, COSL, SINL )
         IF( NCC.GT.0 )
     $      CALL DROT( NCC, C( M-1, 1 ), LDC, C( M, 1 ), LDC, COSL,
     $                 SINL )
         M = M - 2
         GO TO 50
      END IF
*
*     If working on new submatrix, choose shift direction
*     (from larger end diagonal element towards smaller)
*
      IF( LL.GT.OLDM .OR. M.LT.OLDLL ) THEN
         IF( ABS( D( LL ) ).GE.ABS( D( M ) ) ) THEN
*
*           Chase bulge from top (big end) to bottom (small end)
*
            IDIR = 1
         ELSE
*
*           Chase bulge from bottom (big end) to top (small end)
*
            IDIR = 2
         END IF
      END IF
*
*     Apply convergence tests
*
      IF( IDIR.EQ.1 ) THEN
*
*        Run convergence test in forward direction
*        First apply standard test to bottom of matrix
*
         IF( ABS( E( M-1 ) ).LE.ABS( TOL )*ABS( D( M ) ) .OR.
     $       ( TOL.LT.ZERO .AND. ABS( E( M-1 ) ).LE.THRESH ) ) THEN
            E( M-1 ) = ZERO
            GO TO 50
         END IF
*
         IF( TOL.GE.ZERO ) THEN
*
*           If relative accuracy desired,
*           apply convergence criterion forward
*
            MU = ABS( D( LL ) )
            SMINL = MU
            DO 90 LLL = LL, M - 1
               IF( ABS( E( LLL ) ).LE.TOL*MU ) THEN
                  E( LLL ) = ZERO
                  GO TO 50
               END IF
               SMINLO = SMINL
               MU = ABS( D( LLL+1 ) )*( MU / ( MU+ABS( E( LLL ) ) ) )
               SMINL = MIN( SMINL, MU )
   90       CONTINUE
         END IF
*
      ELSE
*
*        Run convergence test in backward direction
*        First apply standard test to top of matrix
*
         IF( ABS( E( LL ) ).LE.ABS( TOL )*ABS( D( LL ) ) .OR.
     $       ( TOL.LT.ZERO .AND. ABS( E( LL ) ).LE.THRESH ) ) THEN
            E( LL ) = ZERO
            GO TO 50
         END IF
*
         IF( TOL.GE.ZERO ) THEN
*
*           If relative accuracy desired,
*           apply convergence criterion backward
*
            MU = ABS( D( M ) )
            SMINL = MU
            DO 100 LLL = M - 1, LL, -1
               IF( ABS( E( LLL ) ).LE.TOL*MU ) THEN
                  E( LLL ) = ZERO
                  GO TO 50
               END IF
               SMINLO = SMINL
               MU = ABS( D( LLL ) )*( MU / ( MU+ABS( E( LLL ) ) ) )
               SMINL = MIN( SMINL, MU )
  100       CONTINUE
         END IF
      END IF
      OLDLL = LL
      OLDM = M
*
*     Compute shift.  First, test if shifting would ruin relative
*     accuracy, and if so set the shift to zero.
*
      IF( TOL.GE.ZERO .AND. N*TOL*( SMINL / SMAX ).LE.
     $    MAX( EPS, HNDRTH*TOL ) ) THEN
*
*        Use a zero shift to avoid loss of relative accuracy
*
         SHIFT = ZERO
      ELSE
*
*        Compute the shift from 2-by-2 block at end of matrix
*
         IF( IDIR.EQ.1 ) THEN
            SLL = ABS( D( LL ) )
            CALL DLAS2( D( M-1 ), E( M-1 ), D( M ), SHIFT, R )
         ELSE
            SLL = ABS( D( M ) )
            CALL DLAS2( D( LL ), E( LL ), D( LL+1 ), SHIFT, R )
         END IF
*
*        Test if shift negligible, and if so set to zero
*
         IF( SLL.GT.ZERO ) THEN
            IF( ( SHIFT / SLL )**2.LT.EPS )
     $         SHIFT = ZERO
         END IF
      END IF
*
*     Increment iteration count
*
      ITER = ITER + M - LL
*
*     If SHIFT = 0, do simplified QR iteration
*
      IF( SHIFT.EQ.ZERO ) THEN
         IF( IDIR.EQ.1 ) THEN
*
*           Chase bulge from top to bottom
*           Save cosines and sines for later singular vector updates
*
            CS = ONE
            OLDCS = ONE
            CALL DLARTG( D( LL )*CS, E( LL ), CS, SN, R )
            CALL DLARTG( OLDCS*R, D( LL+1 )*SN, OLDCS, OLDSN, D( LL ) )
            WORK( 1 ) = CS
            WORK( 1+NM1 ) = SN
            WORK( 1+NM12 ) = OLDCS
            WORK( 1+NM13 ) = OLDSN
            IROT = 1
            DO 110 I = LL + 1, M - 1
               CALL DLARTG( D( I )*CS, E( I ), CS, SN, R )
               E( I-1 ) = OLDSN*R
               CALL DLARTG( OLDCS*R, D( I+1 )*SN, OLDCS, OLDSN, D( I ) )
               IROT = IROT + 1
               WORK( IROT ) = CS
               WORK( IROT+NM1 ) = SN
               WORK( IROT+NM12 ) = OLDCS
               WORK( IROT+NM13 ) = OLDSN
  110       CONTINUE
            H = D( M )*CS
            D( M ) = H*OLDCS
            E( M-1 ) = H*OLDSN
*
*           Update singular vectors
*
            IF( NCVT.GT.0 )
     $         CALL DLASR( 'L', 'V', 'F', M-LL+1, NCVT, WORK( 1 ),
     $                     WORK( N ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DLASR( 'R', 'V', 'F', NRU, M-LL+1, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL DLASR( 'L', 'V', 'F', M-LL+1, NCC, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), C( LL, 1 ), LDC )
*
*           Test convergence
*
            IF( ABS( E( M-1 ) ).LE.THRESH )
     $         E( M-1 ) = ZERO
*
         ELSE
*
*           Chase bulge from bottom to top
*           Save cosines and sines for later singular vector updates
*
            CS = ONE
            OLDCS = ONE
            CALL DLARTG( D( M )*CS, E( M-1 ), CS, SN, R )
            CALL DLARTG( OLDCS*R, D( M-1 )*SN, OLDCS, OLDSN, D( M ) )
            WORK( M-LL ) = CS
            WORK( M-LL+NM1 ) = -SN
            WORK( M-LL+NM12 ) = OLDCS
            WORK( M-LL+NM13 ) = -OLDSN
            IROT = M - LL
            DO 120 I = M - 1, LL + 1, -1
               CALL DLARTG( D( I )*CS, E( I-1 ), CS, SN, R )
               E( I ) = OLDSN*R
               CALL DLARTG( OLDCS*R, D( I-1 )*SN, OLDCS, OLDSN, D( I ) )
               IROT = IROT - 1
               WORK( IROT ) = CS
               WORK( IROT+NM1 ) = -SN
               WORK( IROT+NM12 ) = OLDCS
               WORK( IROT+NM13 ) = -OLDSN
  120       CONTINUE
            H = D( LL )*CS
            D( LL ) = H*OLDCS
            E( LL ) = H*OLDSN
*
*           Update singular vectors
*
            IF( NCVT.GT.0 )
     $         CALL DLASR( 'L', 'V', 'B', M-LL+1, NCVT, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DLASR( 'R', 'V', 'B', NRU, M-LL+1, WORK( 1 ),
     $                     WORK( N ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL DLASR( 'L', 'V', 'B', M-LL+1, NCC, WORK( 1 ),
     $                     WORK( N ), C( LL, 1 ), LDC )
*
*           Test convergence
*
            IF( ABS( E( LL ) ).LE.THRESH )
     $         E( LL ) = ZERO
         END IF
      ELSE
*
*        Use nonzero shift
*
         IF( IDIR.EQ.1 ) THEN
*
*           Chase bulge from top to bottom
*           Save cosines and sines for later singular vector updates
*
            F = ( ABS( D( LL ) )-SHIFT )*
     $          ( SIGN( ONE, D( LL ) )+SHIFT / D( LL ) )
            G = E( LL )
            CALL DLARTG( F, G, COSR, SINR, R )
            F = COSR*D( LL ) + SINR*E( LL )
            E( LL ) = COSR*E( LL ) - SINR*D( LL )
            G = SINR*D( LL+1 )
            D( LL+1 ) = COSR*D( LL+1 )
            CALL DLARTG( F, G, COSL, SINL, R )
            D( LL ) = R
            F = COSL*E( LL ) + SINL*D( LL+1 )
            D( LL+1 ) = COSL*D( LL+1 ) - SINL*E( LL )
            G = SINL*E( LL+1 )
            E( LL+1 ) = COSL*E( LL+1 )
            WORK( 1 ) = COSR
            WORK( 1+NM1 ) = SINR
            WORK( 1+NM12 ) = COSL
            WORK( 1+NM13 ) = SINL
            IROT = 1
            DO 130 I = LL + 1, M - 2
               CALL DLARTG( F, G, COSR, SINR, R )
               E( I-1 ) = R
               F = COSR*D( I ) + SINR*E( I )
               E( I ) = COSR*E( I ) - SINR*D( I )
               G = SINR*D( I+1 )
               D( I+1 ) = COSR*D( I+1 )
               CALL DLARTG( F, G, COSL, SINL, R )
               D( I ) = R
               F = COSL*E( I ) + SINL*D( I+1 )
               D( I+1 ) = COSL*D( I+1 ) - SINL*E( I )
               G = SINL*E( I+1 )
               E( I+1 ) = COSL*E( I+1 )
               IROT = IROT + 1
               WORK( IROT ) = COSR
               WORK( IROT+NM1 ) = SINR
               WORK( IROT+NM12 ) = COSL
               WORK( IROT+NM13 ) = SINL
  130       CONTINUE
            CALL DLARTG( F, G, COSR, SINR, R )
            E( M-2 ) = R
            F = COSR*D( M-1 ) + SINR*E( M-1 )
            E( M-1 ) = COSR*E( M-1 ) - SINR*D( M-1 )
            G = SINR*D( M )
            D( M ) = COSR*D( M )
            CALL DLARTG( F, G, COSL, SINL, R )
            D( M-1 ) = R
            F = COSL*E( M-1 ) + SINL*D( M )
            D( M ) = COSL*D( M ) - SINL*E( M-1 )
            IROT = IROT + 1
            WORK( IROT ) = COSR
            WORK( IROT+NM1 ) = SINR
            WORK( IROT+NM12 ) = COSL
            WORK( IROT+NM13 ) = SINL
            E( M-1 ) = F
*
*           Update singular vectors
*
            IF( NCVT.GT.0 )
     $         CALL DLASR( 'L', 'V', 'F', M-LL+1, NCVT, WORK( 1 ),
     $                     WORK( N ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DLASR( 'R', 'V', 'F', NRU, M-LL+1, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL DLASR( 'L', 'V', 'F', M-LL+1, NCC, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), C( LL, 1 ), LDC )
*
*           Test convergence
*
            IF( ABS( E( M-1 ) ).LE.THRESH )
     $         E( M-1 ) = ZERO
*
         ELSE
*
*           Chase bulge from bottom to top
*           Save cosines and sines for later singular vector updates
*
            F = ( ABS( D( M ) )-SHIFT )*( SIGN( ONE, D( M ) )+SHIFT /
     $          D( M ) )
            G = E( M-1 )
            CALL DLARTG( F, G, COSR, SINR, R )
            F = COSR*D( M ) + SINR*E( M-1 )
            E( M-1 ) = COSR*E( M-1 ) - SINR*D( M )
            G = SINR*D( M-1 )
            D( M-1 ) = COSR*D( M-1 )
            CALL DLARTG( F, G, COSL, SINL, R )
            D( M ) = R
            F = COSL*E( M-1 ) + SINL*D( M-1 )
            D( M-1 ) = COSL*D( M-1 ) - SINL*E( M-1 )
            G = SINL*E( M-2 )
            E( M-2 ) = COSL*E( M-2 )
            WORK( M-LL ) = COSR
            WORK( M-LL+NM1 ) = -SINR
            WORK( M-LL+NM12 ) = COSL
            WORK( M-LL+NM13 ) = -SINL
            IROT = M - LL
            DO 140 I = M - 1, LL + 2, -1
               CALL DLARTG( F, G, COSR, SINR, R )
               E( I ) = R
               F = COSR*D( I ) + SINR*E( I-1 )
               E( I-1 ) = COSR*E( I-1 ) - SINR*D( I )
               G = SINR*D( I-1 )
               D( I-1 ) = COSR*D( I-1 )
               CALL DLARTG( F, G, COSL, SINL, R )
               D( I ) = R
               F = COSL*E( I-1 ) + SINL*D( I-1 )
               D( I-1 ) = COSL*D( I-1 ) - SINL*E( I-1 )
               G = SINL*E( I-2 )
               E( I-2 ) = COSL*E( I-2 )
               IROT = IROT - 1
               WORK( IROT ) = COSR
               WORK( IROT+NM1 ) = -SINR
               WORK( IROT+NM12 ) = COSL
               WORK( IROT+NM13 ) = -SINL
  140       CONTINUE
            CALL DLARTG( F, G, COSR, SINR, R )
            E( LL+1 ) = R
            F = COSR*D( LL+1 ) + SINR*E( LL )
            E( LL ) = COSR*E( LL ) - SINR*D( LL+1 )
            G = SINR*D( LL )
            D( LL ) = COSR*D( LL )
            CALL DLARTG( F, G, COSL, SINL, R )
            D( LL+1 ) = R
            F = COSL*E( LL ) + SINL*D( LL )
            D( LL ) = COSL*D( LL ) - SINL*E( LL )
            IROT = IROT - 1
            WORK( IROT ) = COSR
            WORK( IROT+NM1 ) = -SINR
            WORK( IROT+NM12 ) = COSL
            WORK( IROT+NM13 ) = -SINL
            E( LL ) = F
*
*           Test convergence
*
            IF( ABS( E( LL ) ).LE.THRESH )
     $         E( LL ) = ZERO
*
*           Update singular vectors if desired
*
            IF( NCVT.GT.0 )
     $         CALL DLASR( 'L', 'V', 'B', M-LL+1, NCVT, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DLASR( 'R', 'V', 'B', NRU, M-LL+1, WORK( 1 ),
     $                     WORK( N ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL DLASR( 'L', 'V', 'B', M-LL+1, NCC, WORK( 1 ),
     $                     WORK( N ), C( LL, 1 ), LDC )
         END IF
      END IF
*
*     QR iteration finished, go back and check convergence
*
      GO TO 50
*
*     All singular values converged, so make them positive
*
  150 CONTINUE
      DO 160 I = 1, N
         IF( D( I ).LT.ZERO ) THEN
            D( I ) = -D( I )
*
*           Change sign of singular vectors, if desired
*
            IF( NCVT.GT.0 )
     $         CALL DSCAL( NCVT, NEGONE, VT( I, 1 ), LDVT )
         END IF
  160 CONTINUE
*
*     Sort the singular values into decreasing order (insertion sort on
*     singular values, but only one transposition per singular vector)
*
      DO 180 I = 1, N - 1
*
*        Scan for smallest D(I)
*
         ISUB = 1
         SMIN = D( 1 )
         DO 170 J = 2, N + 1 - I
            IF( D( J ).LE.SMIN ) THEN
               ISUB = J
               SMIN = D( J )
            END IF
  170    CONTINUE
         IF( ISUB.NE.N+1-I ) THEN
*
*           Swap singular values and vectors
*
            D( ISUB ) = D( N+1-I )
            D( N+1-I ) = SMIN
            IF( NCVT.GT.0 )
     $         CALL DSWAP( NCVT, VT( ISUB, 1 ), LDVT, VT( N+1-I, 1 ),
     $                     LDVT )
            IF( NRU.GT.0 )
     $         CALL DSWAP( NRU, U( 1, ISUB ), 1, U( 1, N+1-I ), 1 )
            IF( NCC.GT.0 )
     $         CALL DSWAP( NCC, C( ISUB, 1 ), LDC, C( N+1-I, 1 ), LDC )
         END IF
  180 CONTINUE
      GO TO 210
*
*     Maximum number of iterations exceeded, failure to converge
*
  190 CONTINUE
      INFO = 0
      DO 200 I = 1, N - 1
         IF( E( I ).NE.ZERO )
     $      INFO = INFO + 1
  200 CONTINUE
  210 CONTINUE
      RETURN
*
*     End of DBDSQR
*
      END
      SUBROUTINE DGEBD2( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAUP( * ),
     $                   TAUQ( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEBD2 reduces a real general m by n matrix A to upper or lower
*  bidiagonal form B by an orthogonal transformation: Q' * A * P = B.
*
*  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows in the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns in the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the m by n general matrix to be reduced.
*          On exit,
*          if m >= n, the diagonal and the first superdiagonal are
*            overwritten with the upper bidiagonal matrix B; the
*            elements below the diagonal, with the array TAUQ, represent
*            the orthogonal matrix Q as a product of elementary
*            reflectors, and the elements above the first superdiagonal,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors;
*          if m < n, the diagonal and the first subdiagonal are
*            overwritten with the lower bidiagonal matrix B; the
*            elements below the first subdiagonal, with the array TAUQ,
*            represent the orthogonal matrix Q as a product of
*            elementary reflectors, and the elements above the diagonal,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  D       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The diagonal elements of the bidiagonal matrix B:
*          D(i) = A(i,i).
*
*  E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
*          The off-diagonal elements of the bidiagonal matrix B:
*          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
*          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
*
*  TAUQ    (output) DOUBLE PRECISION array dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix Q. See Further Details.
*
*  TAUP    (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix P. See Further Details.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (max(M,N))
*
*  INFO    (output) INTEGER
*          = 0: successful exit.
*          < 0: if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The matrices Q and P are represented as products of elementary
*  reflectors:
*
*  If m >= n,
*
*     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors;
*  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
*  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
*  tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  If m < n,
*
*     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors;
*  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
*  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
*  tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  The contents of A on exit are illustrated by the following examples:
*
*  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
*
*    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
*    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
*    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
*    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
*    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
*    (  v1  v2  v3  v4  v5 )
*
*  where d and e denote diagonal and off-diagonal elements of B, vi
*  denotes an element of the vector defining H(i), and ui an element of
*  the vector defining G(i).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, DLARFG, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.LT.0 ) THEN
         CALL XERBLA( 'DGEBD2', -INFO )
         RETURN
      END IF
*
      IF( M.GE.N ) THEN
*
*        Reduce to upper bidiagonal form
*
         DO 10 I = 1, N
*
*           Generate elementary reflector H(i) to annihilate A(i+1:m,i)
*
            CALL DLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                   TAUQ( I ) )
            D( I ) = A( I, I )
            A( I, I ) = ONE
*
*           Apply H(i) to A(i:m,i+1:n) from the left
*
            CALL DLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAUQ( I ),
     $                  A( I, I+1 ), LDA, WORK )
            A( I, I ) = D( I )
*
            IF( I.LT.N ) THEN
*
*              Generate elementary reflector G(i) to annihilate
*              A(i,i+2:n)
*
               CALL DLARFG( N-I, A( I, I+1 ), A( I, MIN( I+2, N ) ),
     $                      LDA, TAUP( I ) )
               E( I ) = A( I, I+1 )
               A( I, I+1 ) = ONE
*
*              Apply G(i) to A(i+1:m,i+1:n) from the right
*
               CALL DLARF( 'Right', M-I, N-I, A( I, I+1 ), LDA,
     $                     TAUP( I ), A( I+1, I+1 ), LDA, WORK )
               A( I, I+1 ) = E( I )
            ELSE
               TAUP( I ) = ZERO
            END IF
   10    CONTINUE
      ELSE
*
*        Reduce to lower bidiagonal form
*
         DO 20 I = 1, M
*
*           Generate elementary reflector G(i) to annihilate A(i,i+1:n)
*
            CALL DLARFG( N-I+1, A( I, I ), A( I, MIN( I+1, N ) ), LDA,
     $                   TAUP( I ) )
            D( I ) = A( I, I )
            A( I, I ) = ONE
*
*           Apply G(i) to A(i+1:m,i:n) from the right
*
            CALL DLARF( 'Right', M-I, N-I+1, A( I, I ), LDA, TAUP( I ),
     $                  A( MIN( I+1, M ), I ), LDA, WORK )
            A( I, I ) = D( I )
*
            IF( I.LT.M ) THEN
*
*              Generate elementary reflector H(i) to annihilate
*              A(i+2:m,i)
*
               CALL DLARFG( M-I, A( I+1, I ), A( MIN( I+2, M ), I ), 1,
     $                      TAUQ( I ) )
               E( I ) = A( I+1, I )
               A( I+1, I ) = ONE
*
*              Apply H(i) to A(i+1:m,i+1:n) from the left
*
               CALL DLARF( 'Left', M-I, N-I, A( I+1, I ), 1, TAUQ( I ),
     $                     A( I+1, I+1 ), LDA, WORK )
               A( I+1, I ) = E( I )
            ELSE
               TAUQ( I ) = ZERO
            END IF
   20    CONTINUE
      END IF
      RETURN
*
*     End of DGEBD2
*
      END
      SUBROUTINE DGEBRD( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,
     $                   INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAUP( * ),
     $                   TAUQ( * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DGEBRD reduces a general real M-by-N matrix A to upper or lower
*  bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
*
*  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows in the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns in the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N general matrix to be reduced.
*          On exit,
*          if m >= n, the diagonal and the first superdiagonal are
*            overwritten with the upper bidiagonal matrix B; the
*            elements below the diagonal, with the array TAUQ, represent
*            the orthogonal matrix Q as a product of elementary
*            reflectors, and the elements above the first superdiagonal,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors;
*          if m < n, the diagonal and the first subdiagonal are
*            overwritten with the lower bidiagonal matrix B; the
*            elements below the first subdiagonal, with the array TAUQ,
*            represent the orthogonal matrix Q as a product of
*            elementary reflectors, and the elements above the diagonal,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  D       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The diagonal elements of the bidiagonal matrix B:
*          D(i) = A(i,i).
*
*  E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
*          The off-diagonal elements of the bidiagonal matrix B:
*          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
*          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
*
*  TAUQ    (output) DOUBLE PRECISION array dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix Q. See Further Details.
*
*  TAUP    (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix P. See Further Details.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The length of the array WORK.  LWORK >= max(1,M,N).
*          For optimum performance LWORK >= (M+N)*NB, where NB
*          is the optimal blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The matrices Q and P are represented as products of elementary
*  reflectors:
*
*  If m >= n,
*
*     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors;
*  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
*  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
*  tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  If m < n,
*
*     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors;
*  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
*  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
*  tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  The contents of A on exit are illustrated by the following examples:
*
*  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
*
*    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
*    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
*    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
*    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
*    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
*    (  v1  v2  v3  v4  v5 )
*
*  where d and e denote diagonal and off-diagonal elements of B, vi
*  denotes an element of the vector defining H(i), and ui an element of
*  the vector defining G(i).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IINFO, J, LDWRKX, LDWRKY, MINMN, NB, NBMIN,
     $                   NX
      DOUBLE PRECISION   WS
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEBD2, DGEMM, DLABRD, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.MAX( 1, M, N ) ) THEN
         INFO = -10
      END IF
      IF( INFO.LT.0 ) THEN
         CALL XERBLA( 'DGEBRD', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      MINMN = MIN( M, N )
      IF( MINMN.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      WS = MAX( M, N )
      LDWRKX = M
      LDWRKY = N
*
*     Set the block size NB and the crossover point NX.
*
      NB = MAX (1, ILAENV (1, 'DGEBRD', ' ', M, N, -1))
*
      IF( NB.GT.1 .AND. NB.LT.MINMN ) THEN
*
*        Determine when to switch from blocked to unblocked code.
*
         NX = MAX (NB, ILAENV (3, 'DGEBRD', ' ', M, N, -1))
         IF( NX.LT.MINMN ) THEN
            WS = ( M+N )*NB
            IF( LWORK.LT.WS ) THEN
*
*              Not enough work space for the optimal NB, consider using
*              a smaller block size.
*
               NBMIN = ILAENV (2, 'DGEBRD', ' ', M, N, -1)
               IF( LWORK.GE.( M+N )*NBMIN ) THEN
                  NB = LWORK / ( M+N )
               ELSE
                  NB = 1
                  NX = MINMN
               END IF
            END IF
         END IF
      ELSE
         NX = MINMN
      END IF
*
      DO 30 I = 1, MINMN - NX, NB
*
*        Reduce rows and columns i:i+nb-1 to bidiagonal form and return
*        the matrices X and Y which are needed to update the unreduced
*        part of the matrix
*
         CALL DLABRD( M-I+1, N-I+1, NB, A( I, I ), LDA, D( I ), E( I ),
     $                TAUQ( I ), TAUP( I ), WORK, LDWRKX,
     $                WORK( LDWRKX*NB+1 ), LDWRKY )
*
*        Update the trailing submatrix A(i+nb:m,i+nb:n), using an update
*        of the form  A := A - V*Y' - X*U'
*
         CALL DGEMM( 'No transpose', 'Transpose', M-I-NB+1, N-I-NB+1,
     $               NB, -ONE, A( I+NB, I ), LDA,
     $               WORK( LDWRKX*NB+NB+1 ), LDWRKY, ONE,
     $               A( I+NB, I+NB ), LDA )
         CALL DGEMM( 'No transpose', 'No transpose', M-I-NB+1, N-I-NB+1,
     $               NB, -ONE, WORK( NB+1 ), LDWRKX, A( I, I+NB ), LDA,
     $               ONE, A( I+NB, I+NB ), LDA )
*
*        Copy diagonal and off-diagonal elements of B back into A
*
         IF( M.GE.N ) THEN
            DO 10 J = I, I + NB - 1
               A( J, J ) = D( J )
               A( J, J+1 ) = E( J )
   10       CONTINUE
         ELSE
            DO 20 J = I, I + NB - 1
               A( J, J ) = D( J )
               A( J+1, J ) = E( J )
   20       CONTINUE
         END IF
   30 CONTINUE
*
*     Use unblocked code to reduce the remainder of the matrix
*
      CALL DGEBD2( M-I+1, N-I+1, A( I, I ), LDA, D( I ), E( I ),
     $             TAUQ( I ), TAUP( I ), WORK, IINFO )
      WORK( 1 ) = WS
      RETURN
*
*     End of DGEBRD
*
      END
      SUBROUTINE DGELQ2( M, N, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGELQ2 computes an LQ factorization of a real m by n matrix A:
*  A = L * Q.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the m by n matrix A.
*          On exit, the elements on and below the diagonal of the array
*          contain the m by min(m,n) lower trapezoidal matrix L (L is
*          lower triangular if m <= n); the elements above the diagonal,
*          with the array TAU, represent the orthogonal matrix Q as a
*          product of elementary reflectors (see Further Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (M)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(k) . . . H(2) H(1), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a real scalar, and v is a real vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
*  and tau in TAU(i).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, K
      DOUBLE PRECISION   AII
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, DLARFG, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELQ2', -INFO )
         RETURN
      END IF
*
      K = MIN( M, N )
*
      DO 10 I = 1, K
*
*        Generate elementary reflector H(i) to annihilate A(i,i+1:n)
*
         CALL DLARFG( N-I+1, A( I, I ), A( I, MIN( I+1, N ) ), LDA,
     $                TAU( I ) )
         IF( I.LT.M ) THEN
*
*           Apply H(i) to A(i+1:m,i:n) from the right
*
            AII = A( I, I )
            A( I, I ) = ONE
            CALL DLARF( 'Right', M-I, N-I+1, A( I, I ), LDA, TAU( I ),
     $                  A( I+1, I ), LDA, WORK )
            A( I, I ) = AII
         END IF
   10 CONTINUE
      RETURN
*
*     End of DGELQ2
*
      END
      SUBROUTINE DGELQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DGELQF computes an LQ factorization of a real M-by-N matrix A:
*  A = L * Q.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the elements on and below the diagonal of the array
*          contain the m-by-min(m,n) lower trapezoidal matrix L (L is
*          lower triangular if m <= n); the elements above the diagonal,
*          with the array TAU, represent the orthogonal matrix Q as a
*          product of elementary reflectors (see Further Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,M).
*          For optimum performance LWORK >= M*NB, where NB is the
*          optimal blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(k) . . . H(2) H(1), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a real scalar, and v is a real vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
*  and tau in TAU(i).
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, IB, IINFO, IWS, K, LDWORK, NB, NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGELQ2, DLARFB, DLARFT, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.MAX( 1, M ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELQF', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      K = MIN( M, N )
      IF( K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Determine the block size.
*
      NB = ILAENV (1, 'DGELQF', ' ', M, N, -1)
      NBMIN = 2
      NX = 0
      IWS = M
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX (0, ILAENV (3, 'DGELQF', ' ', M, N, -1))
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = M
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX (2, ILAENV (2, 'DGELQF', ' ', M, N, -1))
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code initially
*
         DO 10 I = 1, K - NX, NB
            IB = MIN( K-I+1, NB )
*
*           Compute the LQ factorization of the current block
*           A(i:i+ib-1,i:n)
*
            CALL DGELQ2( IB, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
            IF( I+IB.LE.M ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL DLARFT( 'Forward', 'Rowwise', N-I+1, IB, A( I, I ),
     $                      LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H to A(i+ib:m,i:n) from the right
*
               CALL DLARFB( 'Right', 'No transpose', 'Forward',
     $                      'Rowwise', M-I-IB+1, N-I+1, IB, A( I, I ),
     $                      LDA, WORK, LDWORK, A( I+IB, I ), LDA,
     $                      WORK( IB+1 ), LDWORK )
            END IF
   10    CONTINUE
      ELSE
         I = 1
      END IF
*
*     Use unblocked code to factor the last or only block.
*
      IF( I.LE.K )
     $   CALL DGELQ2( M-I+1, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                IINFO )
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of DGELQF
*
      END
      SUBROUTINE DGEQR2( M, N, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEQR2 computes a QR factorization of a real m by n matrix A:
*  A = Q * R.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the m by n matrix A.
*          On exit, the elements on and above the diagonal of the array
*          contain the min(m,n) by n upper trapezoidal matrix R (R is
*          upper triangular if m >= n); the elements below the diagonal,
*          with the array TAU, represent the orthogonal matrix Q as a
*          product of elementary reflectors (see Further Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(1) H(2) . . . H(k), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a real scalar, and v is a real vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
*  and tau in TAU(i).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, K
      DOUBLE PRECISION   AII
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, DLARFG, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEQR2', -INFO )
         RETURN
      END IF
*
      K = MIN( M, N )
*
      DO 10 I = 1, K
*
*        Generate elementary reflector H(i) to annihilate A(i+1:m,i)
*
         CALL DLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                TAU( I ) )
         IF( I.LT.N ) THEN
*
*           Apply H(i) to A(i:m,i+1:n) from the left
*
            AII = A( I, I )
            A( I, I ) = ONE
            CALL DLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                  A( I, I+1 ), LDA, WORK )
            A( I, I ) = AII
         END IF
   10 CONTINUE
      RETURN
*
*     End of DGEQR2
*
      END
      SUBROUTINE DGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DGEQRF computes a QR factorization of a real M-by-N matrix A:
*  A = Q * R.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the elements on and above the diagonal of the array
*          contain the min(M,N)-by-N upper trapezoidal matrix R (R is
*          upper triangular if m >= n); the elements below the diagonal,
*          with the array TAU, represent the orthogonal matrix Q as a
*          product of min(m,n) elementary reflectors (see Further
*          Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,N).
*          For optimum performance LWORK >= N*NB, where NB is
*          the optimal blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(1) H(2) . . . H(k), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a real scalar, and v is a real vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
*  and tau in TAU(i).
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, IB, IINFO, IWS, K, LDWORK, NB, NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQR2, DLARFB, DLARFT, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEQRF', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      K = MIN( M, N )
      IF( K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Determine the block size.
*
      NB = ILAENV (1, 'DGEQRF', ' ', M, N, -1)
      NBMIN = 2
      NX = 0
      IWS = N
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX (0, ILAENV (3, 'DGEQRF', ' ', M, N, -1))
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX (2, ILAENV (2, 'DGEQRF', ' ', M, N, -1))
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code initially
*
         DO 10 I = 1, K - NX, NB
            IB = MIN( K-I+1, NB )
*
*           Compute the QR factorization of the current block
*           A(i:m,i:i+ib-1)
*
            CALL DGEQR2( M-I+1, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
            IF( I+IB.LE.N ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL DLARFT( 'Forward', 'Columnwise', M-I+1, IB,
     $                      A( I, I ), LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H' to A(i:m,i+ib:n) from the left
*
               CALL DLARFB( 'Left', 'Transpose', 'Forward',
     $                      'Columnwise', M-I+1, N-I-IB+1, IB,
     $                      A( I, I ), LDA, WORK, LDWORK, A( I, I+IB ),
     $                      LDA, WORK( IB+1 ), LDWORK )
            END IF
   10    CONTINUE
      ELSE
         I = 1
      END IF
*
*     Use unblocked code to factor the last or only block.
*
      IF( I.LE.K )
     $   CALL DGEQR2( M-I+1, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                IINFO )
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of DGEQRF
*
      END
      SUBROUTINE DLABRD( M, N, NB, A, LDA, D, E, TAUQ, TAUP, X, LDX, Y,
     $                   LDY )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LDX, LDY, M, N, NB
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAUP( * ),
     $                   TAUQ( * ), X( LDX, * ), Y( LDY, * )
*     ..
*
*  Purpose
*  =======
*
*  DLABRD reduces the first NB rows and columns of a real general
*  m by n matrix A to upper or lower bidiagonal form by an orthogonal
*  transformation Q' * A * P, and returns the matrices X and Y which
*  are needed to apply the transformation to the unreduced part of A.
*
*  If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
*  bidiagonal form.
*
*  This is an auxiliary routine called by DGEBRD
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows in the matrix A.
*
*  N       (input) INTEGER
*          The number of columns in the matrix A.
*
*  NB      (input) INTEGER
*          The number of leading rows and columns of A to be reduced.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the m by n general matrix to be reduced.
*          On exit, the first NB rows and columns of the matrix are
*          overwritten; the rest of the array is unchanged.
*          If m >= n, elements on and below the diagonal in the first NB
*            columns, with the array TAUQ, represent the orthogonal
*            matrix Q as a product of elementary reflectors; and
*            elements above the diagonal in the first NB rows, with the
*            array TAUP, represent the orthogonal matrix P as a product
*            of elementary reflectors.
*          If m < n, elements below the diagonal in the first NB
*            columns, with the array TAUQ, represent the orthogonal
*            matrix Q as a product of elementary reflectors, and
*            elements on and above the diagonal in the first NB rows,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  D       (output) DOUBLE PRECISION array, dimension (NB)
*          The diagonal elements of the first NB rows and columns of
*          the reduced matrix.  D(i) = A(i,i).
*
*  E       (output) DOUBLE PRECISION array, dimension (NB)
*          The off-diagonal elements of the first NB rows and columns of
*          the reduced matrix.
*
*  TAUQ    (output) DOUBLE PRECISION array dimension (NB)
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix Q. See Further Details.
*
*  TAUP    (output) DOUBLE PRECISION array, dimension (NB)
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix P. See Further Details.
*
*  X       (output) DOUBLE PRECISION array, dimension (LDX,NB)
*          The m-by-nb matrix X required to update the unreduced part
*          of A.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X. LDX >= M.
*
*  Y       (output) DOUBLE PRECISION array, dimension (LDY,NB)
*          The n-by-nb matrix Y required to update the unreduced part
*          of A.
*
*  LDY     (output) INTEGER
*          The leading dimension of the array Y. LDY >= N.
*
*  Further Details
*  ===============
*
*  The matrices Q and P are represented as products of elementary
*  reflectors:
*
*     Q = H(1) H(2) . . . H(nb)  and  P = G(1) G(2) . . . G(nb)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors.
*
*  If m >= n, v(1:i-1) = 0, v(i) = 1, and v(i:m) is stored on exit in
*  A(i:m,i); u(1:i) = 0, u(i+1) = 1, and u(i+1:n) is stored on exit in
*  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  If m < n, v(1:i) = 0, v(i+1) = 1, and v(i+1:m) is stored on exit in
*  A(i+2:m,i); u(1:i-1) = 0, u(i) = 1, and u(i:n) is stored on exit in
*  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  The elements of the vectors v and u together form the m-by-nb matrix
*  V and the nb-by-n matrix U' which are needed, with X and Y, to apply
*  the transformation to the unreduced part of the matrix, using a block
*  update of the form:  A := A - V*Y' - X*U'.
*
*  The contents of A on exit are illustrated by the following examples
*  with nb = 2:
*
*  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
*
*    (  1   1   u1  u1  u1 )           (  1   u1  u1  u1  u1  u1 )
*    (  v1  1   1   u2  u2 )           (  1   1   u2  u2  u2  u2 )
*    (  v1  v2  a   a   a  )           (  v1  1   a   a   a   a  )
*    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
*    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
*    (  v1  v2  a   a   a  )
*
*  where a denotes an element of the original matrix which is unchanged,
*  vi denotes an element of the vector defining H(i), and ui an element
*  of the vector defining G(i).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DLARFG, DSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
      IF( M.GE.N ) THEN
*
*        Reduce to upper bidiagonal form
*
         DO 10 I = 1, NB
*
*           Update A(i:m,i)
*
            CALL DGEMV( 'No transpose', M-I+1, I-1, -ONE, A( I, 1 ),
     $                  LDA, Y( I, 1 ), LDY, ONE, A( I, I ), 1 )
            CALL DGEMV( 'No transpose', M-I+1, I-1, -ONE, X( I, 1 ),
     $                  LDX, A( 1, I ), 1, ONE, A( I, I ), 1 )
*
*           Generate reflection Q(i) to annihilate A(i+1:m,i)
*
            CALL DLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                   TAUQ( I ) )
            D( I ) = A( I, I )
            IF( I.LT.N ) THEN
               A( I, I ) = ONE
*
*              Compute Y(i+1:n,i)
*
               CALL DGEMV( 'Transpose', M-I+1, N-I, ONE, A( I, I+1 ),
     $                     LDA, A( I, I ), 1, ZERO, Y( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', M-I+1, I-1, ONE, A( I, 1 ), LDA,
     $                     A( I, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL DGEMV( 'No transpose', N-I, I-1, -ONE, Y( I+1, 1 ),
     $                     LDY, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', M-I+1, I-1, ONE, X( I, 1 ), LDX,
     $                     A( I, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL DGEMV( 'Transpose', I-1, N-I, -ONE, A( 1, I+1 ),
     $                     LDA, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL DSCAL( N-I, TAUQ( I ), Y( I+1, I ), 1 )
*
*              Update A(i,i+1:n)
*
               CALL DGEMV( 'No transpose', N-I, I, -ONE, Y( I+1, 1 ),
     $                     LDY, A( I, 1 ), LDA, ONE, A( I, I+1 ), LDA )
               CALL DGEMV( 'Transpose', I-1, N-I, -ONE, A( 1, I+1 ),
     $                     LDA, X( I, 1 ), LDX, ONE, A( I, I+1 ), LDA )
*
*              Generate reflection P(i) to annihilate A(i,i+2:n)
*
               CALL DLARFG( N-I, A( I, I+1 ), A( I, MIN( I+2, N ) ),
     $                      LDA, TAUP( I ) )
               E( I ) = A( I, I+1 )
               A( I, I+1 ) = ONE
*
*              Compute X(i+1:m,i)
*
               CALL DGEMV( 'No transpose', M-I, N-I, ONE, A( I+1, I+1 ),
     $                     LDA, A( I, I+1 ), LDA, ZERO, X( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', N-I, I, ONE, Y( I+1, 1 ), LDY,
     $                     A( I, I+1 ), LDA, ZERO, X( 1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I, -ONE, A( I+1, 1 ),
     $                     LDA, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL DGEMV( 'No transpose', I-1, N-I, ONE, A( 1, I+1 ),
     $                     LDA, A( I, I+1 ), LDA, ZERO, X( 1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I-1, -ONE, X( I+1, 1 ),
     $                     LDX, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL DSCAL( M-I, TAUP( I ), X( I+1, I ), 1 )
            END IF
   10    CONTINUE
      ELSE
*
*        Reduce to lower bidiagonal form
*
         DO 20 I = 1, NB
*
*           Update A(i,i:n)
*
            CALL DGEMV( 'No transpose', N-I+1, I-1, -ONE, Y( I, 1 ),
     $                  LDY, A( I, 1 ), LDA, ONE, A( I, I ), LDA )
            CALL DGEMV( 'Transpose', I-1, N-I+1, -ONE, A( 1, I ), LDA,
     $                  X( I, 1 ), LDX, ONE, A( I, I ), LDA )
*
*           Generate reflection P(i) to annihilate A(i,i+1:n)
*
            CALL DLARFG( N-I+1, A( I, I ), A( I, MIN( I+1, N ) ), LDA,
     $                   TAUP( I ) )
            D( I ) = A( I, I )
            IF( I.LT.M ) THEN
               A( I, I ) = ONE
*
*              Compute X(i+1:m,i)
*
               CALL DGEMV( 'No transpose', M-I, N-I+1, ONE, A( I+1, I ),
     $                     LDA, A( I, I ), LDA, ZERO, X( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', N-I+1, I-1, ONE, Y( I, 1 ), LDY,
     $                     A( I, I ), LDA, ZERO, X( 1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I-1, -ONE, A( I+1, 1 ),
     $                     LDA, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL DGEMV( 'No transpose', I-1, N-I+1, ONE, A( 1, I ),
     $                     LDA, A( I, I ), LDA, ZERO, X( 1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I-1, -ONE, X( I+1, 1 ),
     $                     LDX, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL DSCAL( M-I, TAUP( I ), X( I+1, I ), 1 )
*
*              Update A(i+1:m,i)
*
               CALL DGEMV( 'No transpose', M-I, I-1, -ONE, A( I+1, 1 ),
     $                     LDA, Y( I, 1 ), LDY, ONE, A( I+1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I, -ONE, X( I+1, 1 ),
     $                     LDX, A( 1, I ), 1, ONE, A( I+1, I ), 1 )
*
*              Generate reflection Q(i) to annihilate A(i+2:m,i)
*
               CALL DLARFG( M-I, A( I+1, I ), A( MIN( I+2, M ), I ), 1,
     $                      TAUQ( I ) )
               E( I ) = A( I+1, I )
               A( I+1, I ) = ONE
*
*              Compute Y(i+1:n,i)
*
               CALL DGEMV( 'Transpose', M-I, N-I, ONE, A( I+1, I+1 ),
     $                     LDA, A( I+1, I ), 1, ZERO, Y( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', M-I, I-1, ONE, A( I+1, 1 ), LDA,
     $                     A( I+1, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL DGEMV( 'No transpose', N-I, I-1, -ONE, Y( I+1, 1 ),
     $                     LDY, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', M-I, I, ONE, X( I+1, 1 ), LDX,
     $                     A( I+1, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL DGEMV( 'Transpose', I, N-I, -ONE, A( 1, I+1 ), LDA,
     $                     Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL DSCAL( N-I, TAUQ( I ), Y( I+1, I ), 1 )
            END IF
   20    CONTINUE
      END IF
      RETURN
*
*     End of DLABRD
*
      END
      SUBROUTINE DLACPY( UPLO, M, N, A, LDA, B, LDB )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, LDB, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DLACPY copies all or part of a two-dimensional matrix A to another
*  matrix B.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies the part of the matrix A to be copied to B.
*          = 'U':      Upper triangular part
*          = 'L':      Lower triangular part
*          Otherwise:  All of the matrix A
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          The m by n matrix A.  If UPLO = 'U', only the upper triangle
*          or trapezoid is accessed; if UPLO = 'L', only the lower
*          triangle or trapezoid is accessed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (output) DOUBLE PRECISION array, dimension (LDB,N)
*          On exit, B = A in the locations specified by UPLO.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,M).
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
      IF( LSAME( UPLO, 'U' ) ) THEN
         DO 20 J = 1, N
            DO 10 I = 1, MIN( J, M )
               B( I, J ) = A( I, J )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
         DO 40 J = 1, N
            DO 30 I = J, M
               B( I, J ) = A( I, J )
   30       CONTINUE
   40    CONTINUE
      ELSE
         DO 60 J = 1, N
            DO 50 I = 1, M
               B( I, J ) = A( I, J )
   50       CONTINUE
   60    CONTINUE
      END IF
      RETURN
*
*     End of DLACPY
*
      END
      DOUBLE PRECISION FUNCTION DLANGE( NORM, M, N, A, LDA, WORK )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANGE  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real matrix A.
*
*  Description
*  ===========
*
*  DLANGE returns the value
*
*     DLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
*              (
*              ( norm1(A),         NORM = '1', 'O' or 'o'
*              (
*              ( normI(A),         NORM = 'I' or 'i'
*              (
*              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
*
*  where  norm1  denotes the  one norm of a matrix (maximum column sum),
*  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
*  normF  denotes the  Frobenius norm of a matrix (square root of sum of
*  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in DLANGE as described
*          above.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.  When M = 0,
*          DLANGE is set to zero.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.  When N = 0,
*          DLANGE is set to zero.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          The m by n matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(M,1).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK),
*          where LWORK >= M when NORM = 'I'; otherwise, WORK is not
*          referenced.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   SCALE, SUM, VALUE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      IF( MIN( M, N ).EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
*
*        Find max(abs(A(i,j))).
*
         VALUE = ZERO
         DO 20 J = 1, N
            DO 10 I = 1, M
               VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
*
*        Find norm1(A).
*
         VALUE = ZERO
         DO 40 J = 1, N
            SUM = ZERO
            DO 30 I = 1, M
               SUM = SUM + ABS( A( I, J ) )
   30       CONTINUE
            VALUE = MAX( VALUE, SUM )
   40    CONTINUE
      ELSE IF( LSAME( NORM, 'I' ) ) THEN
*
*        Find normI(A).
*
         DO 50 I = 1, M
            WORK( I ) = ZERO
   50    CONTINUE
         DO 70 J = 1, N
            DO 60 I = 1, M
               WORK( I ) = WORK( I ) + ABS( A( I, J ) )
   60       CONTINUE
   70    CONTINUE
         VALUE = ZERO
         DO 80 I = 1, M
            VALUE = MAX( VALUE, WORK( I ) )
   80    CONTINUE
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
*
*        Find normF(A).
*
         SCALE = ZERO
         SUM = ONE
         DO 90 J = 1, N
            CALL DLASSQ( M, A( 1, J ), 1, SCALE, SUM )
   90    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      DLANGE = VALUE
      RETURN
*
*     End of DLANGE
*
      END
      DOUBLE PRECISION FUNCTION DLAPY2( X, Y )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   X, Y
*     ..
*
*  Purpose
*  =======
*
*  DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
*  overflow.
*
*  Arguments
*  =========
*
*  X       (input) DOUBLE PRECISION
*  Y       (input) DOUBLE PRECISION
*          X and Y specify the values x and y.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   W, XABS, YABS, Z
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      XABS = ABS( X )
      YABS = ABS( Y )
      W = MAX( XABS, YABS )
      Z = MIN( XABS, YABS )
      IF( Z.EQ.ZERO ) THEN
         DLAPY2 = W
      ELSE
         DLAPY2 = W*SQRT( ONE+( Z / W )**2 )
      END IF
      RETURN
*
*     End of DLAPY2
*
      END
      SUBROUTINE DLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE
      INTEGER            INCV, LDC, M, N
      DOUBLE PRECISION   TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), V( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLARF applies a real elementary reflector H to a real m by n matrix
*  C, from either the left or the right. H is represented in the form
*
*        H = I - tau * v * v'
*
*  where tau is a real scalar and v is a real vector.
*
*  If tau = 0, then H is taken to be the unit matrix.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': form  H * C
*          = 'R': form  C * H
*
*  M       (input) INTEGER
*          The number of rows of the matrix C.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C.
*
*  V       (input) DOUBLE PRECISION array, dimension
*                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
*                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
*          The vector v in the representation of H. V is not used if
*          TAU = 0.
*
*  INCV    (input) INTEGER
*          The increment between elements of v. INCV <> 0.
*
*  TAU     (input) DOUBLE PRECISION
*          The value tau in the representation of H.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
*          or C * H if SIDE = 'R'.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*                         (N) if SIDE = 'L'
*                      or (M) if SIDE = 'R'
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DGER
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Executable Statements ..
*
      IF( LSAME( SIDE, 'L' ) ) THEN
*
*        Form  H * C
*
         IF( TAU.NE.ZERO ) THEN
*
*           w := C' * v
*
            CALL DGEMV( 'Transpose', M, N, ONE, C, LDC, V, INCV, ZERO,
     $                  WORK, 1 )
*
*           C := C - v * w'
*
            CALL DGER( M, N, -TAU, V, INCV, WORK, 1, C, LDC )
         END IF
      ELSE
*
*        Form  C * H
*
         IF( TAU.NE.ZERO ) THEN
*
*           w := C * v
*
            CALL DGEMV( 'No transpose', M, N, ONE, C, LDC, V, INCV,
     $                  ZERO, WORK, 1 )
*
*           C := C - w * v'
*
            CALL DGER( M, N, -TAU, WORK, 1, V, INCV, C, LDC )
         END IF
      END IF
      RETURN
*
*     End of DLARF
*
      END
      SUBROUTINE DLARFB( SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV,
     $                   T, LDT, C, LDC, WORK, LDWORK )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, SIDE, STOREV, TRANS
      INTEGER            K, LDC, LDT, LDV, LDWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), T( LDT, * ), V( LDV, * ),
     $                   WORK( LDWORK, * )
*     ..
*
*  Purpose
*  =======
*
*  DLARFB applies a real block reflector H or its transpose H' to a
*  real m by n matrix C, from either the left or the right.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply H or H' from the Left
*          = 'R': apply H or H' from the Right
*
*  TRANS   (input) CHARACTER*1
*          = 'N': apply H (No transpose)
*          = 'T': apply H' (Transpose)
*
*  DIRECT  (input) CHARACTER*1
*          Indicates how H is formed from a product of elementary
*          reflectors
*          = 'F': H = H(1) H(2) . . . H(k) (Forward)
*          = 'B': H = H(k) . . . H(2) H(1) (Backward)
*
*  STOREV  (input) CHARACTER*1
*          Indicates how the vectors which define the elementary
*          reflectors are stored:
*          = 'C': Columnwise
*          = 'R': Rowwise
*
*  M       (input) INTEGER
*          The number of rows of the matrix C.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C.
*
*  K       (input) INTEGER
*          The order of the matrix T (= the number of elementary
*          reflectors whose product defines the block reflector).
*
*  V       (input) DOUBLE PRECISION array, dimension
*                                (LDV,K) if STOREV = 'C'
*                                (LDV,M) if STOREV = 'R' and SIDE = 'L'
*                                (LDV,N) if STOREV = 'R' and SIDE = 'R'
*          The matrix V. See further details.
*
*  LDV     (input) INTEGER
*          The leading dimension of the array V.
*          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);
*          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);
*          if STOREV = 'R', LDV >= K.
*
*  T       (input) DOUBLE PRECISION array, dimension (LDT,K)
*          The triangular k by k matrix T in the representation of the
*          block reflector.
*
*  LDT     (input) INTEGER
*          The leading dimension of the array T. LDT >= K.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDA >= max(1,M).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LDWORK,K)
*
*  LDWORK  (input) INTEGER
*          The leading dimension of the array WORK.
*          If SIDE = 'L', LDWORK >= max(1,N);
*          if SIDE = 'R', LDWORK >= max(1,M).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          TRANST
      INTEGER            I, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMM, DTRMM
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
      IF( LSAME( TRANS, 'N' ) ) THEN
         TRANST = 'T'
      ELSE
         TRANST = 'N'
      END IF
*
      IF( LSAME( STOREV, 'C' ) ) THEN
*
         IF( LSAME( DIRECT, 'F' ) ) THEN
*
*           Let  V =  ( V1 )    (first K rows)
*                     ( V2 )
*           where  V1  is unit lower triangular.
*
            IF( LSAME( SIDE, 'L' ) ) THEN
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
*
*              W := C1'
*
               DO 10 J = 1, K
                  CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
   10          CONTINUE
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C2'*V2
*
                  CALL DGEMM( 'Transpose', 'No transpose', N, K, M-K,
     $                        ONE, C( K+1, 1 ), LDC, V( K+1, 1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V * W'
*
               IF( M.GT.K ) THEN
*
*                 C2 := C2 - V2 * W'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M-K, N, K,
     $                        -ONE, V( K+1, 1 ), LDV, WORK, LDWORK, ONE,
     $                        C( K+1, 1 ), LDC )
               END IF
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K,
     $                     ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W'
*
               DO 30 J = 1, K
                  DO 20 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
   20             CONTINUE
   30          CONTINUE
*
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
*
*              W := C1
*
               DO 40 J = 1, K
                  CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
   40          CONTINUE
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C2 * V2
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C( 1, K+1 ), LDC, V( K+1, 1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V'
*
               IF( N.GT.K ) THEN
*
*                 C2 := C2 - W * V2'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V( K+1, 1 ), LDV, ONE,
     $                        C( 1, K+1 ), LDC )
               END IF
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K,
     $                     ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W
*
               DO 60 J = 1, K
                  DO 50 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
   50             CONTINUE
   60          CONTINUE
            END IF
*
         ELSE
*
*           Let  V =  ( V1 )
*                     ( V2 )    (last K rows)
*           where  V2  is unit upper triangular.
*
            IF( LSAME( SIDE, 'L' ) ) THEN
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
*
*              W := C2'
*
               DO 70 J = 1, K
                  CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
   70          CONTINUE
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C1'*V1
*
                  CALL DGEMM( 'Transpose', 'No transpose', N, K, M-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V * W'
*
               IF( M.GT.K ) THEN
*
*                 C1 := C1 - V1 * W'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M-K, N, K,
     $                        -ONE, V, LDV, WORK, LDWORK, ONE, C, LDC )
               END IF
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K,
     $                     ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
*
*              C2 := C2 - W'
*
               DO 90 J = 1, K
                  DO 80 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
   80             CONTINUE
   90          CONTINUE
*
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
*
*              W := C2
*
               DO 100 J = 1, K
                  CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  100          CONTINUE
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
     $                     K, ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C1 * V1
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V'
*
               IF( N.GT.K ) THEN
*
*                 C1 := C1 - W * V1'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               END IF
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K,
     $                     ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
*
*              C2 := C2 - W
*
               DO 120 J = 1, K
                  DO 110 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  110             CONTINUE
  120          CONTINUE
            END IF
         END IF
*
      ELSE IF( LSAME( STOREV, 'R' ) ) THEN
*
         IF( LSAME( DIRECT, 'F' ) ) THEN
*
*           Let  V =  ( V1  V2 )    (V1: first K columns)
*           where  V1  is unit upper triangular.
*
            IF( LSAME( SIDE, 'L' ) ) THEN
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
*
*              W := C1'
*
               DO 130 J = 1, K
                  CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
  130          CONTINUE
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C2'*V2'
*
                  CALL DGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE,
     $                        C( K+1, 1 ), LDC, V( 1, K+1 ), LDV, ONE,
     $                        WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V' * W'
*
               IF( M.GT.K ) THEN
*
*                 C2 := C2 - V2' * W'
*
                  CALL DGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE,
     $                        V( 1, K+1 ), LDV, WORK, LDWORK, ONE,
     $                        C( K+1, 1 ), LDC )
               END IF
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W'
*
               DO 150 J = 1, K
                  DO 140 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
  140             CONTINUE
  150          CONTINUE
*
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
*
*              W := C1
*
               DO 160 J = 1, K
                  CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
  160          CONTINUE
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C2 * V2'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, K, N-K,
     $                        ONE, C( 1, K+1 ), LDC, V( 1, K+1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V
*
               IF( N.GT.K ) THEN
*
*                 C2 := C2 - W * V2
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V( 1, K+1 ), LDV, ONE,
     $                        C( 1, K+1 ), LDC )
               END IF
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
     $                     K, ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W
*
               DO 180 J = 1, K
                  DO 170 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
  170             CONTINUE
  180          CONTINUE
*
            END IF
*
         ELSE
*
*           Let  V =  ( V1  V2 )    (V2: last K columns)
*           where  V2  is unit lower triangular.
*
            IF( LSAME( SIDE, 'L' ) ) THEN
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
*
*              W := C2'
*
               DO 190 J = 1, K
                  CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
  190          CONTINUE
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K,
     $                     ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C1'*V1'
*
                  CALL DGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE,
     $                        C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V' * W'
*
               IF( M.GT.K ) THEN
*
*                 C1 := C1 - V1' * W'
*
                  CALL DGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE,
     $                        V, LDV, WORK, LDWORK, ONE, C, LDC )
               END IF
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
*
*              C2 := C2 - W'
*
               DO 210 J = 1, K
                  DO 200 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
  200             CONTINUE
  210          CONTINUE
*
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
*
*              W := C2
*
               DO 220 J = 1, K
                  CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  220          CONTINUE
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K,
     $                     ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C1 * V1'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, K, N-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V
*
               IF( N.GT.K ) THEN
*
*                 C1 := C1 - W * V1
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               END IF
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
     $                     K, ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
*
*              C1 := C1 - W
*
               DO 240 J = 1, K
                  DO 230 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  230             CONTINUE
  240          CONTINUE
*
            END IF
*
         END IF
      END IF
*
      RETURN
*
*     End of DLARFB
*
      END
      SUBROUTINE DLARFG( N, ALPHA, X, INCX, TAU )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   ALPHA, TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLARFG generates a real elementary reflector H of order n, such
*  that
*
*        H * ( alpha ) = ( beta ),   H' * H = I.
*            (   x   )   (   0  )
*
*  where alpha and beta are scalars, and x is an (n-1)-element real
*  vector. H is represented in the form
*
*        H = I - tau * ( 1 ) * ( 1 v' ) ,
*                      ( v )
*
*  where tau is a real scalar and v is a real (n-1)-element
*  vector.
*
*  If the elements of x are all zero, then tau = 0 and H is taken to be
*  the unit matrix.
*
*  Otherwise  1 <= tau <= 2.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the elementary reflector.
*
*  ALPHA   (input/output) DOUBLE PRECISION
*          On entry, the value alpha.
*          On exit, it is overwritten with the value beta.
*
*  X       (input/output) DOUBLE PRECISION array, dimension
*                         (1+(N-2)*abs(INCX))
*          On entry, the vector x.
*          On exit, it is overwritten with the vector v.
*
*  INCX    (input) INTEGER
*          The increment between elements of X. INCX > 0.
*
*  TAU     (output) DOUBLE PRECISION
*          The value tau.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            J, KNT
      DOUBLE PRECISION   BETA, RSAFMN, SAFMIN, XNORM
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY2, DNRM2
      EXTERNAL           DLAMCH, DLAPY2, DNRM2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN
*     ..
*     .. External Subroutines ..
      EXTERNAL           DSCAL
*     ..
*     .. Executable Statements ..
*
      IF( N.LE.1 ) THEN
         TAU = ZERO
         RETURN
      END IF
*
      XNORM = DNRM2( N-1, X, INCX )
*
      IF( XNORM.EQ.ZERO ) THEN
*
*        H  =  I
*
         TAU = ZERO
      ELSE
*
*        general case
*
         BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA )
         SAFMIN = DLAMCH( 'S' ) / DLAMCH( 'E' )
         IF( ABS( BETA ).LT.SAFMIN ) THEN
*
*           XNORM, BETA may be inaccurate; scale X and recompute them
*
            RSAFMN = ONE / SAFMIN
            KNT = 0
   10       CONTINUE
            KNT = KNT + 1
            CALL DSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHA = ALPHA*RSAFMN
            IF( ABS( BETA ).LT.SAFMIN )
     $         GO TO 10
*
*           New BETA is at most 1, at least SAFMIN
*
            XNORM = DNRM2( N-1, X, INCX )
            BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA )
            TAU = ( BETA-ALPHA ) / BETA
            CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
*
*           If ALPHA is subnormal, it may lose relative accuracy
*
            ALPHA = BETA
            DO 20 J = 1, KNT
               ALPHA = ALPHA*SAFMIN
   20       CONTINUE
         ELSE
            TAU = ( BETA-ALPHA ) / BETA
            CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
            ALPHA = BETA
         END IF
      END IF
*
      RETURN
*
*     End of DLARFG
*
      END
      SUBROUTINE DLARFT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, STOREV
      INTEGER            K, LDT, LDV, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   T( LDT, * ), TAU( * ), V( LDV, * )
*     ..
*
*  Purpose
*  =======
*
*  DLARFT forms the triangular factor T of a real block reflector H
*  of order n, which is defined as a product of k elementary reflectors.
*
*  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
*
*  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
*
*  If STOREV = 'C', the vector which defines the elementary reflector
*  H(i) is stored in the i-th column of the array V, and
*
*     H  =  I - V * T * V'
*
*  If STOREV = 'R', the vector which defines the elementary reflector
*  H(i) is stored in the i-th row of the array V, and
*
*     H  =  I - V' * T * V
*
*  Arguments
*  =========
*
*  DIRECT  (input) CHARACTER*1
*          Specifies the order in which the elementary reflectors are
*          multiplied to form the block reflector:
*          = 'F': H = H(1) H(2) . . . H(k) (Forward)
*          = 'B': H = H(k) . . . H(2) H(1) (Backward)
*
*  STOREV  (input) CHARACTER*1
*          Specifies how the vectors which define the elementary
*          reflectors are stored (see also Further Details):
*          = 'C': columnwise
*          = 'R': rowwise
*
*  N       (input) INTEGER
*          The order of the block reflector H. N >= 0.
*
*  K       (input) INTEGER
*          The order of the triangular factor T (= the number of
*          elementary reflectors). K >= 1.
*
*  V       (input/output) DOUBLE PRECISION array, dimension
*                               (LDV,K) if STOREV = 'C'
*                               (LDV,N) if STOREV = 'R'
*          The matrix V. See further details.
*
*  LDV     (input) INTEGER
*          The leading dimension of the array V.
*          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i).
*
*  T       (output) DOUBLE PRECISION array, dimension (LDT,K)
*          The k by k triangular factor T of the block reflector.
*          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
*          lower triangular. The rest of the array is not used.
*
*  LDT     (input) INTEGER
*          The leading dimension of the array T. LDT >= K.
*
*  Further Details
*  ===============
*
*  The shape of the matrix V and the storage of the vectors which define
*  the H(i) is best illustrated by the following example with n = 5 and
*  k = 3. The elements equal to 1 are not stored; the corresponding
*  array elements are modified but restored on exit. The rest of the
*  array is not used.
*
*  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':
*
*               V = (  1       )                 V = (  1 v1 v1 v1 v1 )
*                   ( v1  1    )                     (     1 v2 v2 v2 )
*                   ( v1 v2  1 )                     (        1 v3 v3 )
*                   ( v1 v2 v3 )
*                   ( v1 v2 v3 )
*
*  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':
*
*               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
*                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )
*                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
*                   (     1 v3 )
*                   (        1 )
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   VII
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DTRMV
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( LSAME( DIRECT, 'F' ) ) THEN
         DO 20 I = 1, K
            IF( TAU( I ).EQ.ZERO ) THEN
*
*              H(i)  =  I
*
               DO 10 J = 1, I
                  T( J, I ) = ZERO
   10          CONTINUE
            ELSE
*
*              general case
*
               VII = V( I, I )
               V( I, I ) = ONE
               IF( LSAME( STOREV, 'C' ) ) THEN
*
*                 T(1:i-1,i) := - tau(i) * V(i:n,1:i-1)' * V(i:n,i)
*
                  CALL DGEMV( 'Transpose', N-I+1, I-1, -TAU( I ),
     $                        V( I, 1 ), LDV, V( I, I ), 1, ZERO,
     $                        T( 1, I ), 1 )
               ELSE
*
*                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:n) * V(i,i:n)'
*
                  CALL DGEMV( 'No transpose', I-1, N-I+1, -TAU( I ),
     $                        V( 1, I ), LDV, V( I, I ), LDV, ZERO,
     $                        T( 1, I ), 1 )
               END IF
               V( I, I ) = VII
*
*              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i)
*
               CALL DTRMV( 'Upper', 'No transpose', 'Non-unit', I-1, T,
     $                     LDT, T( 1, I ), 1 )
               T( I, I ) = TAU( I )
            END IF
   20    CONTINUE
      ELSE
         DO 40 I = K, 1, -1
            IF( TAU( I ).EQ.ZERO ) THEN
*
*              H(i)  =  I
*
               DO 30 J = I, K
                  T( J, I ) = ZERO
   30          CONTINUE
            ELSE
*
*              general case
*
               IF( I.LT.K ) THEN
                  IF( LSAME( STOREV, 'C' ) ) THEN
                     VII = V( N-K+I, I )
                     V( N-K+I, I ) = ONE
*
*                    T(i+1:k,i) :=
*                            - tau(i) * V(1:n-k+i,i+1:k)' * V(1:n-k+i,i)
*
                     CALL DGEMV( 'Transpose', N-K+I, K-I, -TAU( I ),
     $                           V( 1, I+1 ), LDV, V( 1, I ), 1, ZERO,
     $                           T( I+1, I ), 1 )
                     V( N-K+I, I ) = VII
                  ELSE
                     VII = V( I, N-K+I )
                     V( I, N-K+I ) = ONE
*
*                    T(i+1:k,i) :=
*                            - tau(i) * V(i+1:k,1:n-k+i) * V(i,1:n-k+i)'
*
                     CALL DGEMV( 'No transpose', K-I, N-K+I, -TAU( I ),
     $                           V( I+1, 1 ), LDV, V( I, 1 ), LDV, ZERO,
     $                           T( I+1, I ), 1 )
                     V( I, N-K+I ) = VII
                  END IF
*
*                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,i)
*
                  CALL DTRMV( 'Lower', 'No transpose', 'Non-unit', K-I,
     $                        T( I+1, I+1 ), LDT, T( I+1, I ), 1 )
               END IF
               T( I, I ) = TAU( I )
            END IF
   40    CONTINUE
      END IF
      RETURN
*
*     End of DLARFT
*
      END
      SUBROUTINE DLARTG( F, G, CS, SN, R )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   CS, F, G, R, SN
*     ..
*
*  Purpose
*  =======
*
*  DLARTG generate a plane rotation so that
*
*     [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
*     [ -SN  CS  ]     [ G ]     [ 0 ]
*
*  This is a slower, more accurate version of the BLAS1 routine DROTG,
*  with the following other differences:
*     F and G are unchanged on return.
*     If G=0, then CS=1 and SN=0.
*     If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any
*        floating point operations (saves work in DBDSQR when
*        there are zeros on the diagonal).
*
*  If F exceeds G in magnitude, CS will be positive.
*
*  Arguments
*  =========
*
*  F       (input) DOUBLE PRECISION
*          The first component of vector to be rotated.
*
*  G       (input) DOUBLE PRECISION
*          The second component of vector to be rotated.
*
*  CS      (output) DOUBLE PRECISION
*          The cosine of the rotation.
*
*  SN      (output) DOUBLE PRECISION
*          The sine of the rotation.
*
*  R       (output) DOUBLE PRECISION
*          The nonzero component of the rotated vector.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FIRST
      INTEGER            COUNT, I
      DOUBLE PRECISION   EPS, F1, G1, SAFMIN, SAFMN2, SAFMX2, SCALE
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, LOG, MAX, SQRT
*     ..
*     .. Save statement ..
      SAVE               FIRST, SAFMX2, SAFMIN, SAFMN2
*     ..
*     .. Data statements ..
      DATA               FIRST / .TRUE. /
*     ..
*     .. Executable Statements ..
*
      IF( FIRST ) THEN
         FIRST = .FALSE.
         SAFMIN = DLAMCH( 'S' )
         EPS = DLAMCH( 'E' )
         SAFMN2 = DLAMCH( 'B' )**INT( LOG( SAFMIN / EPS ) /
     $            LOG( DLAMCH( 'B' ) ) / TWO )
         SAFMX2 = ONE / SAFMN2
      END IF
      IF( G.EQ.ZERO ) THEN
         CS = ONE
         SN = ZERO
         R = F
      ELSE IF( F.EQ.ZERO ) THEN
         CS = ZERO
         SN = ONE
         R = G
      ELSE
         F1 = F
         G1 = G
         SCALE = MAX( ABS( F1 ), ABS( G1 ) )
         IF( SCALE.GE.SAFMX2 ) THEN
            COUNT = 0
   10       CONTINUE
            COUNT = COUNT + 1
            F1 = F1*SAFMN2
            G1 = G1*SAFMN2
            SCALE = MAX( ABS( F1 ), ABS( G1 ) )
            IF( SCALE.GE.SAFMX2 )
     $         GO TO 10
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
            DO 20 I = 1, COUNT
               R = R*SAFMX2
   20       CONTINUE
         ELSE IF( SCALE.LE.SAFMN2 ) THEN
            COUNT = 0
   30       CONTINUE
            COUNT = COUNT + 1
            F1 = F1*SAFMX2
            G1 = G1*SAFMX2
            SCALE = MAX( ABS( F1 ), ABS( G1 ) )
            IF( SCALE.LE.SAFMN2 )
     $         GO TO 30
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
            DO 40 I = 1, COUNT
               R = R*SAFMN2
   40       CONTINUE
         ELSE
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
         END IF
         IF( ABS( F ).GT.ABS( G ) .AND. CS.LT.ZERO ) THEN
            CS = -CS
            SN = -SN
            R = -R
         END IF
      END IF
      RETURN
*
*     End of DLARTG
*
      END
      SUBROUTINE DLAS2( F, G, H, SSMIN, SSMAX )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   F, G, H, SSMAX, SSMIN
*     ..
*
*  Purpose
*  =======
*
*  DLAS2  computes the singular values of the 2-by-2 matrix
*     [  F   G  ]
*     [  0   H  ].
*  On return, SSMIN is the smaller singular value and SSMAX is the
*  larger singular value.
*
*  Arguments
*  =========
*
*  F       (input) DOUBLE PRECISION
*          The (1,1) element of the 2-by-2 matrix.
*
*  G       (input) DOUBLE PRECISION
*          The (1,2) element of the 2-by-2 matrix.
*
*  H       (input) DOUBLE PRECISION
*          The (2,2) element of the 2-by-2 matrix.
*
*  SSMIN   (output) DOUBLE PRECISION
*          The smaller singular value.
*
*  SSMAX   (output) DOUBLE PRECISION
*          The larger singular value.
*
*  Further Details
*  ===============
*
*  Barring over/underflow, all output quantities are correct to within
*  a few units in the last place (ulps), even in the absence of a guard
*  digit in addition/subtraction.
*
*  In IEEE arithmetic, the code works correctly if one matrix element is
*  infinite.
*
*  Overflow will not occur unless the largest singular value itself
*  overflows, or is within a few ulps of overflow. (On machines with
*  partial overflow, like the Cray, overflow may occur if the largest
*  singular value is within a factor of 2 of overflow.)
*
*  Underflow is harmless if underflow is gradual. Otherwise, results
*  may correspond to a matrix modified by perturbations of size near
*  the underflow threshold.
*
*  ====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   AS, AT, AU, C, FA, FHMN, FHMX, GA, HA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      FA = ABS( F )
      GA = ABS( G )
      HA = ABS( H )
      FHMN = MIN( FA, HA )
      FHMX = MAX( FA, HA )
      IF( FHMN.EQ.ZERO ) THEN
         SSMIN = ZERO
         IF( FHMX.EQ.ZERO ) THEN
            SSMAX = GA
         ELSE
            SSMAX = MAX( FHMX, GA )*SQRT( ONE+
     $              ( MIN( FHMX, GA ) / MAX( FHMX, GA ) )**2 )
         END IF
      ELSE
         IF( GA.LT.FHMX ) THEN
            AS = ONE + FHMN / FHMX
            AT = ( FHMX-FHMN ) / FHMX
            AU = ( GA / FHMX )**2
            C = TWO / ( SQRT( AS*AS+AU )+SQRT( AT*AT+AU ) )
            SSMIN = FHMN*C
            SSMAX = FHMX / C
         ELSE
            AU = FHMX / GA
            IF( AU.EQ.ZERO ) THEN
*
*              Avoid possible harmful underflow if exponent range
*              asymmetric (true SSMIN may not underflow even if
*              AU underflows)
*
               SSMIN = ( FHMN*FHMX ) / GA
               SSMAX = GA
            ELSE
               AS = ONE + FHMN / FHMX
               AT = ( FHMX-FHMN ) / FHMX
               C = ONE / ( SQRT( ONE+( AS*AU )**2 )+
     $             SQRT( ONE+( AT*AU )**2 ) )
               SSMIN = ( FHMN*C )*AU
               SSMIN = SSMIN + SSMIN
               SSMAX = GA / ( C+C )
            END IF
         END IF
      END IF
      RETURN
*
*     End of DLAS2
*
      END
      SUBROUTINE DLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          TYPE
      INTEGER            INFO, KL, KU, LDA, M, N
      DOUBLE PRECISION   CFROM, CTO
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  DLASCL multiplies the M by N real matrix A by the real scalar
*  CTO/CFROM.  This is done without over/underflow as long as the final
*  result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
*  A may be full, upper triangular, lower triangular, upper Hessenberg,
*  or banded.
*
*  Arguments
*  =========
*
*  TYPE    (input) CHARACTER*1
*          TYPE indices the storage type of the input matrix.
*          = 'G':  A is a full matrix.
*          = 'L':  A is a lower triangular matrix.
*          = 'U':  A is an upper triangular matrix.
*          = 'H':  A is an upper Hessenberg matrix.
*          = 'B':  A is a symmetric band matrix with lower bandwidth KL
*                  and upper bandwidth KU and with the only the lower
*                  half stored.
*          = 'Q':  A is a symmetric band matrix with lower bandwidth KL
*                  and upper bandwidth KU and with the only the upper
*                  half stored.
*          = 'Z':  A is a band matrix with lower bandwidth KL and upper
*                  bandwidth KU.
*
*  KL      (input) INTEGER
*          The lower bandwidth of A.  Referenced only if TYPE = 'B',
*          'Q' or 'Z'.
*
*  KU      (input) INTEGER
*          The upper bandwidth of A.  Referenced only if TYPE = 'B',
*          'Q' or 'Z'.
*
*  CFROM   (input) DOUBLE PRECISION
*  CTO     (input) DOUBLE PRECISION
*          The matrix A is multiplied by CTO/CFROM. A(I,J) is computed
*          without over/underflow if the final result CTO*A(I,J)/CFROM
*          can be represented without over/underflow.  CFROM must be
*          nonzero.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,M)
*          The matrix to be multiplied by CTO/CFROM.  See TYPE for the
*          storage type.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  INFO    (output) INTEGER
*          0  - successful exit
*          <0 - if INFO = -i, the i-th argument had an illegal value.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            DONE
      INTEGER            I, ITYPE, J, K1, K2, K3, K4
      DOUBLE PRECISION   BIGNUM, CFROM1, CFROMC, CTO1, CTOC, MUL, SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
*
      IF( LSAME( TYPE, 'G' ) ) THEN
         ITYPE = 0
      ELSE IF( LSAME( TYPE, 'L' ) ) THEN
         ITYPE = 1
      ELSE IF( LSAME( TYPE, 'U' ) ) THEN
         ITYPE = 2
      ELSE IF( LSAME( TYPE, 'H' ) ) THEN
         ITYPE = 3
      ELSE IF( LSAME( TYPE, 'B' ) ) THEN
         ITYPE = 4
      ELSE IF( LSAME( TYPE, 'Q' ) ) THEN
         ITYPE = 5
      ELSE IF( LSAME( TYPE, 'Z' ) ) THEN
         ITYPE = 6
      ELSE
         ITYPE = -1
      END IF
*
      IF( ITYPE.EQ.-1 ) THEN
         INFO = -1
      ELSE IF( CFROM.EQ.ZERO ) THEN
         INFO = -4
      ELSE IF( M.LT.0 ) THEN
         INFO = -6
      ELSE IF( N.LT.0 .OR. ( ITYPE.EQ.4 .AND. N.NE.M ) .OR.
     $         ( ITYPE.EQ.5 .AND. N.NE.M ) ) THEN
         INFO = -7
      ELSE IF( ITYPE.LE.3 .AND. LDA.LT.MAX( 1, M ) ) THEN
         INFO = -9
      ELSE IF( ITYPE.GE.4 ) THEN
         IF( KL.LT.0 .OR. KL.GT.MAX( M-1, 0 ) ) THEN
            INFO = -2
         ELSE IF( KU.LT.0 .OR. KU.GT.MAX( N-1, 0 ) .OR.
     $            ( ( ITYPE.EQ.4 .OR. ITYPE.EQ.5 ) .AND. KL.NE.KU ) )
     $             THEN
            INFO = -3
         ELSE IF( ( ITYPE.EQ.4 .AND. LDA.LT.KL+1 ) .OR.
     $            ( ITYPE.EQ.5 .AND. LDA.LT.KU+1 ) .OR.
     $            ( ITYPE.EQ.6 .AND. LDA.LT.2*KL+KU+1 ) ) THEN
            INFO = -9
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASCL', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. M.EQ.0 )
     $   RETURN
*
*     Get machine parameters
*
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
*
      CFROMC = CFROM
      CTOC = CTO
*
   10 CONTINUE
      CFROM1 = CFROMC*SMLNUM
      CTO1 = CTOC / BIGNUM
      IF( ABS( CFROM1 ).GT.ABS( CTOC ) .AND. CTOC.NE.ZERO ) THEN
         MUL = SMLNUM
         DONE = .FALSE.
         CFROMC = CFROM1
      ELSE IF( ABS( CTO1 ).GT.ABS( CFROMC ) ) THEN
         MUL = BIGNUM
         DONE = .FALSE.
         CTOC = CTO1
      ELSE
         MUL = CTOC / CFROMC
         DONE = .TRUE.
      END IF
*
      IF( ITYPE.EQ.0 ) THEN
*
*        Full matrix
*
         DO 30 J = 1, N
            DO 20 I = 1, M
               A( I, J ) = A( I, J )*MUL
   20       CONTINUE
   30    CONTINUE
*
      ELSE IF( ITYPE.EQ.1 ) THEN
*
*        Lower triangular matrix
*
         DO 50 J = 1, N
            DO 40 I = J, M
               A( I, J ) = A( I, J )*MUL
   40       CONTINUE
   50    CONTINUE
*
      ELSE IF( ITYPE.EQ.2 ) THEN
*
*        Upper triangular matrix
*
         DO 70 J = 1, N
            DO 60 I = 1, MIN( J, M )
               A( I, J ) = A( I, J )*MUL
   60       CONTINUE
   70    CONTINUE
*
      ELSE IF( ITYPE.EQ.3 ) THEN
*
*        Upper Hessenberg matrix
*
         DO 90 J = 1, N
            DO 80 I = 1, MIN( J+1, M )
               A( I, J ) = A( I, J )*MUL
   80       CONTINUE
   90    CONTINUE
*
      ELSE IF( ITYPE.EQ.4 ) THEN
*
*        Lower half of a symmetric band matrix
*
         K3 = KL + 1
         K4 = N + 1
         DO 110 J = 1, N
            DO 100 I = 1, MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  100       CONTINUE
  110    CONTINUE
*
      ELSE IF( ITYPE.EQ.5 ) THEN
*
*        Upper half of a symmetric band matrix
*
         K1 = KU + 2
         K3 = KU + 1
         DO 130 J = 1, N
            DO 120 I = MAX( K1-J, 1 ), K3
               A( I, J ) = A( I, J )*MUL
  120       CONTINUE
  130    CONTINUE
*
      ELSE IF( ITYPE.EQ.6 ) THEN
*
*        Band matrix
*
         K1 = KL + KU + 2
         K2 = KL + 1
         K3 = 2*KL + KU + 1
         K4 = KL + KU + 1 + M
         DO 150 J = 1, N
            DO 140 I = MAX( K1-J, K2 ), MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  140       CONTINUE
  150    CONTINUE
*
      END IF
*
      IF( .NOT.DONE )
     $   GO TO 10
*
      RETURN
*
*     End of DLASCL
*
      END
      SUBROUTINE DLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, M, N
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  DLASET initializes an m-by-n matrix A to BETA on the diagonal and
*  ALPHA on the offdiagonals.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies the part of the matrix A to be set.
*          = 'U':      Upper triangular part is set; the strictly lower
*                      triangular part of A is not changed.
*          = 'L':      Lower triangular part is set; the strictly upper
*                      triangular part of A is not changed.
*          Otherwise:  All of the matrix A is set.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  ALPHA   (input) DOUBLE PRECISION
*          The constant to which the offdiagonal elements are to be set.
*
*  BETA    (input) DOUBLE PRECISION
*          The constant to which the diagonal elements are to be set.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On exit, the leading m-by-n submatrix of A is set as follows:
*
*          if UPLO = 'U', A(i,j) = ALPHA, 1<=i<=j-1, 1<=j<=n,
*          if UPLO = 'L', A(i,j) = ALPHA, j+1<=i<=m, 1<=j<=n,
*          otherwise,     A(i,j) = ALPHA, 1<=i<=m, 1<=j<=n, i.ne.j,
*
*          and, for all UPLO, A(i,i) = BETA, 1<=i<=min(m,n).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
* =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
      IF( LSAME( UPLO, 'U' ) ) THEN
*
*        Set the strictly upper triangular or trapezoidal part of the
*        array to ALPHA.
*
         DO 20 J = 2, N
            DO 10 I = 1, MIN( J-1, M )
               A( I, J ) = ALPHA
   10       CONTINUE
   20    CONTINUE
*
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
*
*        Set the strictly lower triangular or trapezoidal part of the
*        array to ALPHA.
*
         DO 40 J = 1, MIN( M, N )
            DO 30 I = J + 1, M
               A( I, J ) = ALPHA
   30       CONTINUE
   40    CONTINUE
*
      ELSE
*
*        Set the leading m-by-n submatrix to ALPHA.
*
         DO 60 J = 1, N
            DO 50 I = 1, M
               A( I, J ) = ALPHA
   50       CONTINUE
   60    CONTINUE
      END IF
*
*     Set the first min(M,N) diagonal elements to BETA.
*
      DO 70 I = 1, MIN( M, N )
         A( I, I ) = BETA
   70 CONTINUE
*
      RETURN
*
*     End of DLASET
*
      END
      SUBROUTINE DLASQ1( N, D, E, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * ), WORK( * )
*     ..
*
*     Purpose
*     =======
*
*     DLASQ1 computes the singular values of a real N-by-N bidiagonal
*     matrix with diagonal D and off-diagonal E. The singular values are
*     computed to high relative accuracy, barring over/underflow or
*     denormalization. The algorithm is described in
*
*     "Accurate singular values and differential qd algorithms," by
*     K. V. Fernando and B. N. Parlett,
*     Numer. Math., Vol-67, No. 2, pp. 191-230,1994.
*
*     See also
*     "Implementation of differential qd algorithms," by
*     K. V. Fernando and B. N. Parlett, Technical Report,
*     Department of Mathematics, University of California at Berkeley,
*     1994 (Under preparation).
*
*     Arguments
*     =========
*
*  N       (input) INTEGER
*          The number of rows and columns in the matrix. N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, D contains the diagonal elements of the
*          bidiagonal matrix whose SVD is desired. On normal exit,
*          D contains the singular values in decreasing order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, elements E(1:N-1) contain the off-diagonal elements
*          of the bidiagonal matrix whose SVD is desired.
*          On exit, E is overwritten.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the algorithm did not converge;  i
*                specifies how many superdiagonals did not converge.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   MEIGTH
      PARAMETER          ( MEIGTH = -0.125D0 )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TEN
      PARAMETER          ( TEN = 10.0D0 )
      DOUBLE PRECISION   HUNDRD
      PARAMETER          ( HUNDRD = 100.0D0 )
      DOUBLE PRECISION   TWO56
      PARAMETER          ( TWO56 = 256.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            RESTRT
      INTEGER            I, IERR, J, KE, KEND, M, NY
      DOUBLE PRECISION   DM, DX, EPS, SCL, SFMIN, SIG1, SIG2, SIGMN,
     $                   SIGMX, SMALL2, THRESH, TOL, TOL2, TOLMUL
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLAS2, DLASCL, DLASQ2, DLASRT, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -2
         CALL XERBLA( 'DLASQ1', -INFO )
         RETURN
      ELSE IF( N.EQ.0 ) THEN
         RETURN
      ELSE IF( N.EQ.1 ) THEN
         D( 1 ) = ABS( D( 1 ) )
         RETURN
      ELSE IF( N.EQ.2 ) THEN
         CALL DLAS2( D( 1 ), E( 1 ), D( 2 ), SIGMN, SIGMX )
         D( 1 ) = SIGMX
         D( 2 ) = SIGMN
         RETURN
      END IF
*
*     Estimate the largest singular value
*
      SIGMX = ZERO
      DO 10 I = 1, N - 1
         SIGMX = MAX( SIGMX, ABS( E( I ) ) )
   10 CONTINUE
*
*     Early return if sigmx is zero (matrix is already diagonal)
*
      IF( SIGMX.EQ.ZERO )
     $   GO TO 70
*
      DO 20 I = 1, N
         D( I ) = ABS( D( I ) )
         SIGMX = MAX( SIGMX, D( I ) )
   20 CONTINUE
*
*     Get machine parameters
*
      EPS = DLAMCH( 'EPSILON' )
      SFMIN = DLAMCH( 'SAFE MINIMUM' )
*
*     Compute singular values to relative accuracy TOL
*     It is assumed that tol**2 does not underflow.
*
      TOLMUL = MAX( TEN, MIN( HUNDRD, EPS**( -MEIGTH ) ) )
      TOL = TOLMUL*EPS
      TOL2 = TOL**2
*
      THRESH = SIGMX*SQRT( SFMIN )*TOL
*
*     Scale matrix so the square of the largest element is
*     1 / ( 256 * SFMIN )
*
      SCL = SQRT( ONE / ( TWO56*SFMIN ) )
      SMALL2 = ONE / ( TWO56*TOLMUL**2 )
      CALL DCOPY( N, D, 1, WORK( 1 ), 1 )
      CALL DCOPY( N-1, E, 1, WORK( N+1 ), 1 )
      CALL DLASCL( 'G', 0, 0, SIGMX, SCL, N, 1, WORK( 1 ), N, IERR )
      CALL DLASCL( 'G', 0, 0, SIGMX, SCL, N-1, 1, WORK( N+1 ), N-1,
     $             IERR )
*
*     Square D and E (the input for the qd algorithm)
*
      DO 30 J = 1, 2*N - 1
         WORK( J ) = WORK( J )**2
   30 CONTINUE
*
*     Apply qd algorithm
*
      M = 0
      E( N ) = ZERO
      DX = WORK( 1 )
      DM = DX
      KE = 0
      RESTRT = .FALSE.
      DO 60 I = 1, N
         IF( ABS( E( I ) ).LE.THRESH .OR. WORK( N+I ).LE.TOL2*
     $       ( DM / DBLE( I-M ) ) ) THEN
            NY = I - M
            IF( NY.EQ.1 ) THEN
               GO TO 50
            ELSE IF( NY.EQ.2 ) THEN
               CALL DLAS2( D( M+1 ), E( M+1 ), D( M+2 ), SIG1, SIG2 )
               D( M+1 ) = SIG1
               D( M+2 ) = SIG2
            ELSE
               KEND = KE + 1 - M
               CALL DLASQ2( NY, D( M+1 ), E( M+1 ), WORK( M+1 ),
     $                      WORK( M+N+1 ), EPS, TOL2, SMALL2, DM, KEND,
     $                      INFO )
*
*                 Return, INFO = number of unconverged superdiagonals
*
               IF( INFO.NE.0 ) THEN
                  INFO = INFO + I
                  RETURN
               END IF
*
*                 Undo scaling
*
               DO 40 J = M + 1, M + NY
                  D( J ) = SQRT( D( J ) )
   40          CONTINUE
               CALL DLASCL( 'G', 0, 0, SCL, SIGMX, NY, 1, D( M+1 ), NY,
     $                      IERR )
            END IF
   50       CONTINUE
            M = I
            IF( I.NE.N ) THEN
               DX = WORK( I+1 )
               DM = DX
               KE = I
               RESTRT = .TRUE.
            END IF
         END IF
         IF( I.NE.N .AND. .NOT.RESTRT ) THEN
            DX = WORK( I+1 )*( DX / ( DX+WORK( N+I ) ) )
            IF( DM.GT.DX ) THEN
               DM = DX
               KE = I
            END IF
         END IF
         RESTRT = .FALSE.
   60 CONTINUE
      KEND = KE + 1
*
*     Sort the singular values into decreasing order
*
   70 CONTINUE
      CALL DLASRT( 'D', N, D, INFO )
      RETURN
*
*     End of DLASQ1
*
      END
      SUBROUTINE DLASQ2( M, Q, E, QQ, EE, EPS, TOL2, SMALL2, SUP, KEND,
     $                   INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, KEND, M
      DOUBLE PRECISION   EPS, SMALL2, SUP, TOL2
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   E( * ), EE( * ), Q( * ), QQ( * )
*     ..
*
*     Purpose
*     =======
*
*     DLASQ2 computes the singular values of a real N-by-N unreduced
*     bidiagonal matrix with squared diagonal elements in Q and
*     squared off-diagonal elements in E. The singular values are
*     computed to relative accuracy TOL, barring over/underflow or
*     denormalization.
*
*     Arguments
*     =========
*
*  M       (input) INTEGER
*          The number of rows and columns in the matrix. M >= 0.
*
*  Q       (output) DOUBLE PRECISION array, dimension (M)
*          On normal exit, contains the squared singular values.
*
*  E       (workspace) DOUBLE PRECISION array, dimension (M)
*
*  QQ      (input/output) DOUBLE PRECISION array, dimension (M)
*          On entry, QQ contains the squared diagonal elements of the
*          bidiagonal matrix whose SVD is desired.
*          On exit, QQ is overwritten.
*
*  EE      (input/output) DOUBLE PRECISION array, dimension (M)
*          On entry, EE(1:N-1) contains the squared off-diagonal
*          elements of the bidiagonal matrix whose SVD is desired.
*          On exit, EE is overwritten.
*
*  EPS     (input) DOUBLE PRECISION
*          Machine epsilon.
*
*  TOL2    (input) DOUBLE PRECISION
*          Desired relative accuracy of computed eigenvalues
*          as defined in DLASQ1.
*
*  SMALL2  (input) DOUBLE PRECISION
*          A threshold value as defined in DLASQ1.
*
*  SUP     (input/output) DOUBLE PRECISION
*          Upper bound for the smallest eigenvalue.
*
*  KEND    (input/output) INTEGER
*          Index where minimum d occurs.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the algorithm did not converge;  i
*                specifies how many superdiagonals did not converge.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
      DOUBLE PRECISION   FOUR, HALF
      PARAMETER          ( FOUR = 4.0D+0, HALF = 0.5D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            ICONV, IPHASE, ISP, N, OFF, OFF1
      DOUBLE PRECISION   QEMAX, SIGMA, XINF, XX, YY
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASQ3
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, NINT, SQRT
*     ..
*     .. Executable Statements ..
      N = M
*
*     Set the default maximum number of iterations
*
      OFF = 0
      OFF1 = OFF + 1
      SIGMA = ZERO
      XINF = ZERO
      ICONV = 0
      IPHASE = 2
*
*     Try deflation at the bottom
*
*     1x1 deflation
*
   10 CONTINUE
      IF( N.LE.2 )
     $   GO TO 20
      IF( EE( N-1 ).LE.MAX( QQ( N ), XINF, SMALL2 )*TOL2 ) THEN
         Q( N ) = QQ( N )
         N = N - 1
         IF( KEND.GT.N )
     $      KEND = N
         SUP = MIN( QQ( N ), QQ( N-1 ) )
         GO TO 10
      END IF
*
*     2x2 deflation
*
      IF( EE( N-2 ).LE.MAX( XINF, SMALL2,
     $    ( QQ( N ) / ( QQ( N )+EE( N-1 )+QQ( N-1 ) ) )*QQ( N-1 ) )*
     $    TOL2 ) THEN
         QEMAX = MAX( QQ( N ), QQ( N-1 ), EE( N-1 ) )
         IF( QEMAX.NE.ZERO ) THEN
            IF( QEMAX.EQ.QQ( N-1 ) ) THEN
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N )-QQ( N-1 )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*EE( N-1 ) / QEMAX ) )
            ELSE IF( QEMAX.EQ.QQ( N ) ) THEN
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N-1 )-QQ( N )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*EE( N-1 ) / QEMAX ) )
            ELSE
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N )-QQ( N-1 )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*QQ( N-1 ) / QEMAX ) )
            END IF
            YY = ( MAX( QQ( N ), QQ( N-1 ) ) / XX )*
     $           MIN( QQ( N ), QQ( N-1 ) )
         ELSE
            XX = ZERO
            YY = ZERO
         END IF
         Q( N-1 ) = XX
         Q( N ) = YY
         N = N - 2
         IF( KEND.GT.N )
     $      KEND = N
         SUP = QQ( N )
         GO TO 10
      END IF
*
   20 CONTINUE
      IF( N.EQ.0 ) THEN
*
*         The lower branch is finished
*
         IF( OFF.EQ.0 ) THEN
*
*         No upper branch; return to DLASQ1
*
            RETURN
         ELSE
*
*         Going back to upper branch
*
            XINF = ZERO
            IF( EE( OFF ).GT.ZERO ) THEN
               ISP = NINT( EE( OFF ) )
               IPHASE = 1
            ELSE
               ISP = -NINT( EE( OFF ) )
               IPHASE = 2
            END IF
            SIGMA = E( OFF )
            N = OFF - ISP + 1
            OFF1 = ISP
            OFF = OFF1 - 1
            IF( N.LE.2 )
     $         GO TO 20
            IF( IPHASE.EQ.1 ) THEN
               SUP = MIN( Q( N+OFF ), Q( N-1+OFF ), Q( N-2+OFF ) )
            ELSE
               SUP = MIN( QQ( N+OFF ), QQ( N-1+OFF ), QQ( N-2+OFF ) )
            END IF
            KEND = 0
            ICONV = -3
         END IF
      ELSE IF( N.EQ.1 ) THEN
*
*     1x1 Solver
*
         IF( IPHASE.EQ.1 ) THEN
            Q( OFF1 ) = Q( OFF1 ) + SIGMA
         ELSE
            Q( OFF1 ) = QQ( OFF1 ) + SIGMA
         END IF
         N = 0
         GO TO 20
*
*     2x2 Solver
*
      ELSE IF( N.EQ.2 ) THEN
         IF( IPHASE.EQ.2 ) THEN
            QEMAX = MAX( QQ( N+OFF ), QQ( N-1+OFF ), EE( N-1+OFF ) )
            IF( QEMAX.NE.ZERO ) THEN
               IF( QEMAX.EQ.QQ( N-1+OFF ) ) THEN
                  XX = HALF*( QQ( N+OFF )+QQ( N-1+OFF )+EE( N-1+OFF )+
     $                 QEMAX*SQRT( ( ( QQ( N+OFF )-QQ( N-1+OFF )+EE( N-
     $                 1+OFF ) ) / QEMAX )**2+FOUR*EE( OFF+N-1 ) /
     $                 QEMAX ) )
               ELSE IF( QEMAX.EQ.QQ( N+OFF ) ) THEN
                  XX = HALF*( QQ( N+OFF )+QQ( N-1+OFF )+EE( N-1+OFF )+
     $                 QEMAX*SQRT( ( ( QQ( N-1+OFF )-QQ( N+OFF )+EE( N-
     $                 1+OFF ) ) / QEMAX )**2+FOUR*EE( N-1+OFF ) /
     $                 QEMAX ) )
               ELSE
                  XX = HALF*( QQ( N+OFF )+QQ( N-1+OFF )+EE( N-1+OFF )+
     $                 QEMAX*SQRT( ( ( QQ( N+OFF )-QQ( N-1+OFF )+EE( N-
     $                 1+OFF ) ) / QEMAX )**2+FOUR*QQ( N-1+OFF ) /
     $                 QEMAX ) )
               END IF
               YY = ( MAX( QQ( N+OFF ), QQ( N-1+OFF ) ) / XX )*
     $              MIN( QQ( N+OFF ), QQ( N-1+OFF ) )
            ELSE
               XX = ZERO
               YY = ZERO
            END IF
         ELSE
            QEMAX = MAX( Q( N+OFF ), Q( N-1+OFF ), E( N-1+OFF ) )
            IF( QEMAX.NE.ZERO ) THEN
               IF( QEMAX.EQ.Q( N-1+OFF ) ) THEN
                  XX = HALF*( Q( N+OFF )+Q( N-1+OFF )+E( N-1+OFF )+
     $                 QEMAX*SQRT( ( ( Q( N+OFF )-Q( N-1+OFF )+E( N-1+
     $                 OFF ) ) / QEMAX )**2+FOUR*E( N-1+OFF ) /
     $                 QEMAX ) )
               ELSE IF( QEMAX.EQ.Q( N+OFF ) ) THEN
                  XX = HALF*( Q( N+OFF )+Q( N-1+OFF )+E( N-1+OFF )+
     $                 QEMAX*SQRT( ( ( Q( N-1+OFF )-Q( N+OFF )+E( N-1+
     $                 OFF ) ) / QEMAX )**2+FOUR*E( N-1+OFF ) /
     $                 QEMAX ) )
               ELSE
                  XX = HALF*( Q( N+OFF )+Q( N-1+OFF )+E( N-1+OFF )+
     $                 QEMAX*SQRT( ( ( Q( N+OFF )-Q( N-1+OFF )+E( N-1+
     $                 OFF ) ) / QEMAX )**2+FOUR*Q( N-1+OFF ) /
     $                 QEMAX ) )
               END IF
               YY = ( MAX( Q( N+OFF ), Q( N-1+OFF ) ) / XX )*
     $              MIN( Q( N+OFF ), Q( N-1+OFF ) )
            ELSE
               XX = ZERO
               YY = ZERO
            END IF
         END IF
         Q( N-1+OFF ) = SIGMA + XX
         Q( N+OFF ) = YY + SIGMA
         N = 0
         GO TO 20
      END IF
      CALL DLASQ3( N, Q( OFF1 ), E( OFF1 ), QQ( OFF1 ), EE( OFF1 ), SUP,
     $             SIGMA, KEND, OFF, IPHASE, ICONV, EPS, TOL2, SMALL2 )
      IF( SUP.LT.ZERO ) THEN
         INFO = N + OFF
         RETURN
      END IF
      OFF1 = OFF + 1
      GO TO 20
*
*     End of DLASQ2
*
      END
      SUBROUTINE DLASQ3( N, Q, E, QQ, EE, SUP, SIGMA, KEND, OFF, IPHASE,
     $                   ICONV, EPS, TOL2, SMALL2 )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            ICONV, IPHASE, KEND, N, OFF
      DOUBLE PRECISION   EPS, SIGMA, SMALL2, SUP, TOL2
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   E( * ), EE( * ), Q( * ), QQ( * )
*     ..
*
*     Purpose
*     =======
*
*     DLASQ3 is the workhorse of the whole bidiagonal SVD algorithm.
*     This can be described as the differential qd with shifts.
*
*     Arguments
*     =========
*
*  N       (input/output) INTEGER
*          On entry, N specifies the number of rows and columns
*          in the matrix. N must be at least 3.
*          On exit N is non-negative and less than the input value.
*
*  Q       (input/output) DOUBLE PRECISION array, dimension (N)
*          Q array in ping (see IPHASE below)
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          E array in ping (see IPHASE below)
*
*  QQ      (input/output) DOUBLE PRECISION array, dimension (N)
*          Q array in pong (see IPHASE below)
*
*  EE      (input/output) DOUBLE PRECISION array, dimension (N)
*          E array in pong (see IPHASE below)
*
*  SUP     (input/output) DOUBLE PRECISION
*          Upper bound for the smallest eigenvalue
*
*  SIGMA   (input/output) DOUBLE PRECISION
*          Accumulated shift for the present submatrix
*
*  KEND    (input/output) INTEGER
*          Index where minimum D(i) occurs in recurrence for
*          splitting criterion
*
*  OFF     (input/output) INTEGER
*          Offset for arrays
*
*  IPHASE  (input/output) INTEGER
*          If IPHASE = 1 (ping) then data is in Q and E arrays
*          If IPHASE = 2 (pong) then data is in QQ and EE arrays
*
*  ICONV   (input) INTEGER
*          If ICONV = 0 a bottom part of a matrix (with a split)
*          If ICONV =-3 a top part of a matrix (with a split)
*
*  EPS     (input) DOUBLE PRECISION
*          Machine epsilon
*
*  TOL2    (input) DOUBLE PRECISION
*          Square of the relative tolerance TOL as defined in DLASQ1
*
*  SMALL2  (input) DOUBLE PRECISION
*          A threshold value as defined in DLASQ1
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            NPP
      PARAMETER          ( NPP = 32 )
      INTEGER            IPP
      PARAMETER          ( IPP = 5 )
      DOUBLE PRECISION   HALF, FOUR
      PARAMETER          ( HALF = 0.5D+0, FOUR = 4.0D+0 )
      INTEGER            IFLMAX
      PARAMETER          ( IFLMAX = 2 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LDEF, LSPLIT
      INTEGER            I, IC, ICNT, IFL, IP, ISP, K1END, K2END, KE,
     $                   KS, MAXIT, N1, N2
      DOUBLE PRECISION   D, DM, QEMAX, T1, TAU, TOLX, TOLY, TOLZ, XX, YY
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLASQ4
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
      ICNT = 0
      TAU = ZERO
      DM = SUP
      TOLX = SIGMA*TOL2
      TOLZ = MAX( SMALL2, SIGMA )*TOL2
*
*     Set maximum number of iterations
*
      MAXIT = 100*N
*
*     Flipping
*
      IC = 2
      IF( N.GT.3 ) THEN
         IF( IPHASE.EQ.1 ) THEN
            DO 10 I = 1, N - 2
               IF( Q( I ).GT.Q( I+1 ) )
     $            IC = IC + 1
               IF( E( I ).GT.E( I+1 ) )
     $            IC = IC + 1
   10       CONTINUE
            IF( Q( N-1 ).GT.Q( N ) )
     $         IC = IC + 1
            IF( IC.LT.N ) THEN
               CALL DCOPY( N, Q, 1, QQ, -1 )
               CALL DCOPY( N-1, E, 1, EE, -1 )
               IF( KEND.NE.0 )
     $            KEND = N - KEND + 1
               IPHASE = 2
            END IF
         ELSE
            DO 20 I = 1, N - 2
               IF( QQ( I ).GT.QQ( I+1 ) )
     $            IC = IC + 1
               IF( EE( I ).GT.EE( I+1 ) )
     $            IC = IC + 1
   20       CONTINUE
            IF( QQ( N-1 ).GT.QQ( N ) )
     $         IC = IC + 1
            IF( IC.LT.N ) THEN
               CALL DCOPY( N, QQ, 1, Q, -1 )
               CALL DCOPY( N-1, EE, 1, E, -1 )
               IF( KEND.NE.0 )
     $            KEND = N - KEND + 1
               IPHASE = 1
            END IF
         END IF
      END IF
      IF( ICONV.EQ.-3 ) THEN
         IF( IPHASE.EQ.1 ) THEN
            GO TO 180
         ELSE
            GO TO 80
         END IF
      END IF
      IF( IPHASE.EQ.2 )
     $   GO TO 130
*
*     The ping section of the code
*
   30 CONTINUE
      IFL = 0
*
*     Compute the shift
*
      IF( KEND.EQ.0 .OR. SUP.EQ.ZERO ) THEN
         TAU = ZERO
      ELSE IF( ICNT.GT.0 .AND. DM.LE.TOLZ ) THEN
         TAU = ZERO
      ELSE
         IP = MAX( IPP, N / NPP )
         N2 = 2*IP + 1
         IF( N2.GE.N ) THEN
            N1 = 1
            N2 = N
         ELSE IF( KEND+IP.GT.N ) THEN
            N1 = N - 2*IP
         ELSE IF( KEND-IP.LT.1 ) THEN
            N1 = 1
         ELSE
            N1 = KEND - IP
         END IF
         CALL DLASQ4( N2, Q( N1 ), E( N1 ), TAU, SUP )
      END IF
   40 CONTINUE
      ICNT = ICNT + 1
      IF( ICNT.GT.MAXIT ) THEN
         SUP = -ONE
         RETURN
      END IF
      IF( TAU.EQ.ZERO ) THEN
*
*     dqd algorithm
*
         D = Q( 1 )
         DM = D
         KE = 0
         DO 50 I = 1, N - 3
            QQ( I ) = D + E( I )
            D = ( D / QQ( I ) )*Q( I+1 )
            IF( DM.GT.D ) THEN
               DM = D
               KE = I
            END IF
   50    CONTINUE
         KE = KE + 1
*
*     Penultimate dqd step (in ping)
*
         K2END = KE
         QQ( N-2 ) = D + E( N-2 )
         D = ( D / QQ( N-2 ) )*Q( N-1 )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
         END IF
*
*     Final dqd step (in ping)
*
         K1END = KE
         QQ( N-1 ) = D + E( N-1 )
         D = ( D / QQ( N-1 ) )*Q( N )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         QQ( N ) = D
      ELSE
*
*     The dqds algorithm (in ping)
*
         D = Q( 1 ) - TAU
         DM = D
         KE = 0
         IF( D.LT.ZERO )
     $      GO TO 120
         DO 60 I = 1, N - 3
            QQ( I ) = D + E( I )
            D = ( D / QQ( I ) )*Q( I+1 ) - TAU
            IF( DM.GT.D ) THEN
               DM = D
               KE = I
               IF( D.LT.ZERO )
     $            GO TO 120
            END IF
   60    CONTINUE
         KE = KE + 1
*
*     Penultimate dqds step (in ping)
*
         K2END = KE
         QQ( N-2 ) = D + E( N-2 )
         D = ( D / QQ( N-2 ) )*Q( N-1 ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
            IF( D.LT.ZERO )
     $         GO TO 120
         END IF
*
*     Final dqds step (in ping)
*
         K1END = KE
         QQ( N-1 ) = D + E( N-1 )
         D = ( D / QQ( N-1 ) )*Q( N ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         QQ( N ) = D
      END IF
*
*        Convergence when QQ(N) is small (in ping)
*
      IF( ABS( QQ( N ) ).LE.SIGMA*TOL2 ) THEN
         QQ( N ) = ZERO
         DM = ZERO
         KE = N
      END IF
      IF( QQ( N ).LT.ZERO )
     $   GO TO 120
*
*     Non-negative qd array: Update the e's
*
      DO 70 I = 1, N - 1
         EE( I ) = ( E( I ) / QQ( I ) )*Q( I+1 )
   70 CONTINUE
*
*     Updating sigma and iphase in ping
*
      SIGMA = SIGMA + TAU
      IPHASE = 2
   80 CONTINUE
      TOLX = SIGMA*TOL2
      TOLY = SIGMA*EPS
      TOLZ = MAX( SIGMA, SMALL2 )*TOL2
*
*     Checking for deflation and convergence (in ping)
*
   90 CONTINUE
      IF( N.LE.2 )
     $   RETURN
*
*        Deflation: bottom 1x1 (in ping)
*
      LDEF = .FALSE.
      IF( EE( N-1 ).LE.TOLZ ) THEN
         LDEF = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         IF( EE( N-1 ).LE.EPS*( SIGMA+QQ( N ) ) ) THEN
            IF( EE( N-1 )*( QQ( N ) / ( QQ( N )+SIGMA ) ).LE.TOL2*
     $          ( QQ( N )+SIGMA ) ) THEN
               LDEF = .TRUE.
            END IF
         END IF
      ELSE
         IF( EE( N-1 ).LE.QQ( N )*TOL2 ) THEN
            LDEF = .TRUE.
         END IF
      END IF
      IF( LDEF ) THEN
         Q( N ) = QQ( N ) + SIGMA
         N = N - 1
         ICONV = ICONV + 1
         GO TO 90
      END IF
*
*        Deflation: bottom 2x2 (in ping)
*
      LDEF = .FALSE.
      IF( EE( N-2 ).LE.TOLZ ) THEN
         LDEF = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         T1 = SIGMA + EE( N-1 )*( SIGMA / ( SIGMA+QQ( N ) ) )
         IF( EE( N-2 )*( T1 / ( QQ( N-1 )+T1 ) ).LE.TOLY ) THEN
            IF( EE( N-2 )*( QQ( N-1 ) / ( QQ( N-1 )+T1 ) ).LE.TOLX )
     $           THEN
               LDEF = .TRUE.
            END IF
         END IF
      ELSE
         IF( EE( N-2 ).LE.( QQ( N ) / ( QQ( N )+EE( N-1 )+QQ( N-1 ) ) )*
     $       QQ( N-1 )*TOL2 ) THEN
            LDEF = .TRUE.
         END IF
      END IF
      IF( LDEF ) THEN
         QEMAX = MAX( QQ( N ), QQ( N-1 ), EE( N-1 ) )
         IF( QEMAX.NE.ZERO ) THEN
            IF( QEMAX.EQ.QQ( N-1 ) ) THEN
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N )-QQ( N-1 )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*EE( N-1 ) / QEMAX ) )
            ELSE IF( QEMAX.EQ.QQ( N ) ) THEN
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N-1 )-QQ( N )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*EE( N-1 ) / QEMAX ) )
            ELSE
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N )-QQ( N-1 )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*QQ( N-1 ) / QEMAX ) )
            END IF
            YY = ( MAX( QQ( N ), QQ( N-1 ) ) / XX )*
     $           MIN( QQ( N ), QQ( N-1 ) )
         ELSE
            XX = ZERO
            YY = ZERO
         END IF
         Q( N-1 ) = SIGMA + XX
         Q( N ) = YY + SIGMA
         N = N - 2
         ICONV = ICONV + 2
         GO TO 90
      END IF
*
*     Updating bounds before going to pong
*
      IF( ICONV.EQ.0 ) THEN
         KEND = KE
         SUP = MIN( DM, SUP-TAU )
      ELSE IF( ICONV.GT.0 ) THEN
         SUP = MIN( QQ( N ), QQ( N-1 ), QQ( N-2 ), QQ( 1 ), QQ( 2 ),
     $         QQ( 3 ) )
         IF( ICONV.EQ.1 ) THEN
            KEND = K1END
         ELSE IF( ICONV.EQ.2 ) THEN
            KEND = K2END
         ELSE
            KEND = N
         END IF
         ICNT = 0
         MAXIT = 100*N
      END IF
*
*     Checking for splitting in ping
*
      LSPLIT = .FALSE.
      DO 100 KS = N - 3, 3, -1
         IF( EE( KS ).LE.TOLY ) THEN
            IF( EE( KS )*( MIN( QQ( KS+1 ),
     $          QQ( KS ) ) / ( MIN( QQ( KS+1 ), QQ( KS ) )+SIGMA ) ).LE.
     $          TOLX ) THEN
               LSPLIT = .TRUE.
               GO TO 110
            END IF
         END IF
  100 CONTINUE
*
      KS = 2
      IF( EE( 2 ).LE.TOLZ ) THEN
         LSPLIT = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         T1 = SIGMA + EE( 1 )*( SIGMA / ( SIGMA+QQ( 1 ) ) )
         IF( EE( 2 )*( T1 / ( QQ( 1 )+T1 ) ).LE.TOLY ) THEN
            IF( EE( 2 )*( QQ( 1 ) / ( QQ( 1 )+T1 ) ).LE.TOLX ) THEN
               LSPLIT = .TRUE.
            END IF
         END IF
      ELSE
         IF( EE( 2 ).LE.( QQ( 1 ) / ( QQ( 1 )+EE( 1 )+QQ( 2 ) ) )*
     $       QQ( 2 )*TOL2 ) THEN
            LSPLIT = .TRUE.
         END IF
      END IF
      IF( LSPLIT )
     $   GO TO 110
*
      KS = 1
      IF( EE( 1 ).LE.TOLZ ) THEN
         LSPLIT = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         IF( EE( 1 ).LE.EPS*( SIGMA+QQ( 1 ) ) ) THEN
            IF( EE( 1 )*( QQ( 1 ) / ( QQ( 1 )+SIGMA ) ).LE.TOL2*
     $          ( QQ( 1 )+SIGMA ) ) THEN
               LSPLIT = .TRUE.
            END IF
         END IF
      ELSE
         IF( EE( 1 ).LE.QQ( 1 )*TOL2 ) THEN
            LSPLIT = .TRUE.
         END IF
      END IF
*
  110 CONTINUE
      IF( LSPLIT ) THEN
         SUP = MIN( QQ( N ), QQ( N-1 ), QQ( N-2 ) )
         ISP = -( OFF+1 )
         OFF = OFF + KS
         N = N - KS
         KEND = MAX( 1, KEND-KS )
         E( KS ) = SIGMA
         EE( KS ) = ISP
         ICONV = 0
         RETURN
      END IF
*
*     Coincidence
*
      IF( TAU.EQ.ZERO .AND. DM.LE.TOLZ .AND. KEND.NE.N .AND. ICONV.EQ.
     $    0 .AND. ICNT.GT.0 ) THEN
         CALL DCOPY( N-KE, E( KE ), 1, QQ( KE ), 1 )
         QQ( N ) = ZERO
         CALL DCOPY( N-KE, Q( KE+1 ), 1, EE( KE ), 1 )
         SUP = ZERO
      END IF
      ICONV = 0
      GO TO 130
*
*     A new shift when the previous failed (in ping)
*
  120 CONTINUE
      IFL = IFL + 1
      SUP = TAU
*
*     SUP is small or
*     Too many bad shifts (ping)
*
      IF( SUP.LE.TOLZ .OR. IFL.GE.IFLMAX ) THEN
         TAU = ZERO
         GO TO 40
*
*     The asymptotic shift (in ping)
*
      ELSE
         TAU = MAX( TAU+D, ZERO )
         IF( TAU.LE.TOLZ )
     $      TAU = ZERO
         GO TO 40
      END IF
*
*     the pong section of the code
*
  130 CONTINUE
      IFL = 0
*
*     Compute the shift (in pong)
*
      IF( KEND.EQ.0 .AND. SUP.EQ.ZERO ) THEN
         TAU = ZERO
      ELSE IF( ICNT.GT.0 .AND. DM.LE.TOLZ ) THEN
         TAU = ZERO
      ELSE
         IP = MAX( IPP, N / NPP )
         N2 = 2*IP + 1
         IF( N2.GE.N ) THEN
            N1 = 1
            N2 = N
         ELSE IF( KEND+IP.GT.N ) THEN
            N1 = N - 2*IP
         ELSE IF( KEND-IP.LT.1 ) THEN
            N1 = 1
         ELSE
            N1 = KEND - IP
         END IF
         CALL DLASQ4( N2, QQ( N1 ), EE( N1 ), TAU, SUP )
      END IF
  140 CONTINUE
      ICNT = ICNT + 1
      IF( ICNT.GT.MAXIT ) THEN
         SUP = -SUP
         RETURN
      END IF
      IF( TAU.EQ.ZERO ) THEN
*
*     The dqd algorithm (in pong)
*
         D = QQ( 1 )
         DM = D
         KE = 0
         DO 150 I = 1, N - 3
            Q( I ) = D + EE( I )
            D = ( D / Q( I ) )*QQ( I+1 )
            IF( DM.GT.D ) THEN
               DM = D
               KE = I
            END IF
  150    CONTINUE
         KE = KE + 1
*
*     Penultimate dqd step (in pong)
*
         K2END = KE
         Q( N-2 ) = D + EE( N-2 )
         D = ( D / Q( N-2 ) )*QQ( N-1 )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
         END IF
*
*     Final dqd step (in pong)
*
         K1END = KE
         Q( N-1 ) = D + EE( N-1 )
         D = ( D / Q( N-1 ) )*QQ( N )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         Q( N ) = D
      ELSE
*
*     The dqds algorithm (in pong)
*
         D = QQ( 1 ) - TAU
         DM = D
         KE = 0
         IF( D.LT.ZERO )
     $      GO TO 220
         DO 160 I = 1, N - 3
            Q( I ) = D + EE( I )
            D = ( D / Q( I ) )*QQ( I+1 ) - TAU
            IF( DM.GT.D ) THEN
               DM = D
               KE = I
               IF( D.LT.ZERO )
     $            GO TO 220
            END IF
  160    CONTINUE
         KE = KE + 1
*
*     Penultimate dqds step (in pong)
*
         K2END = KE
         Q( N-2 ) = D + EE( N-2 )
         D = ( D / Q( N-2 ) )*QQ( N-1 ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
            IF( D.LT.ZERO )
     $         GO TO 220
         END IF
*
*     Final dqds step (in pong)
*
         K1END = KE
         Q( N-1 ) = D + EE( N-1 )
         D = ( D / Q( N-1 ) )*QQ( N ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         Q( N ) = D
      END IF
*
*        Convergence when is small (in pong)
*
      IF( ABS( Q( N ) ).LE.SIGMA*TOL2 ) THEN
         Q( N ) = ZERO
         DM = ZERO
         KE = N
      END IF
      IF( Q( N ).LT.ZERO )
     $   GO TO 220
*
*     Non-negative qd array: Update the e's
*
      DO 170 I = 1, N - 1
         E( I ) = ( EE( I ) / Q( I ) )*QQ( I+1 )
  170 CONTINUE
*
*     Updating sigma and iphase in pong
*
      SIGMA = SIGMA + TAU
  180 CONTINUE
      IPHASE = 1
      TOLX = SIGMA*TOL2
      TOLY = SIGMA*EPS
*
*     Checking for deflation and convergence (in pong)
*
  190 CONTINUE
      IF( N.LE.2 )
     $   RETURN
*
*        Deflation: bottom 1x1 (in pong)
*
      LDEF = .FALSE.
      IF( E( N-1 ).LE.TOLZ ) THEN
         LDEF = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         IF( E( N-1 ).LE.EPS*( SIGMA+Q( N ) ) ) THEN
            IF( E( N-1 )*( Q( N ) / ( Q( N )+SIGMA ) ).LE.TOL2*
     $          ( Q( N )+SIGMA ) ) THEN
               LDEF = .TRUE.
            END IF
         END IF
      ELSE
         IF( E( N-1 ).LE.Q( N )*TOL2 ) THEN
            LDEF = .TRUE.
         END IF
      END IF
      IF( LDEF ) THEN
         Q( N ) = Q( N ) + SIGMA
         N = N - 1
         ICONV = ICONV + 1
         GO TO 190
      END IF
*
*        Deflation: bottom 2x2 (in pong)
*
      LDEF = .FALSE.
      IF( E( N-2 ).LE.TOLZ ) THEN
         LDEF = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         T1 = SIGMA + E( N-1 )*( SIGMA / ( SIGMA+Q( N ) ) )
         IF( E( N-2 )*( T1 / ( Q( N-1 )+T1 ) ).LE.TOLY ) THEN
            IF( E( N-2 )*( Q( N-1 ) / ( Q( N-1 )+T1 ) ).LE.TOLX ) THEN
               LDEF = .TRUE.
            END IF
         END IF
      ELSE
         IF( E( N-2 ).LE.( Q( N ) / ( Q( N )+EE( N-1 )+Q( N-1 ) )*Q( N-
     $       1 ) )*TOL2 ) THEN
            LDEF = .TRUE.
         END IF
      END IF
      IF( LDEF ) THEN
         QEMAX = MAX( Q( N ), Q( N-1 ), E( N-1 ) )
         IF( QEMAX.NE.ZERO ) THEN
            IF( QEMAX.EQ.Q( N-1 ) ) THEN
               XX = HALF*( Q( N )+Q( N-1 )+E( N-1 )+QEMAX*
     $              SQRT( ( ( Q( N )-Q( N-1 )+E( N-1 ) ) / QEMAX )**2+
     $              FOUR*E( N-1 ) / QEMAX ) )
            ELSE IF( QEMAX.EQ.Q( N ) ) THEN
               XX = HALF*( Q( N )+Q( N-1 )+E( N-1 )+QEMAX*
     $              SQRT( ( ( Q( N-1 )-Q( N )+E( N-1 ) ) / QEMAX )**2+
     $              FOUR*E( N-1 ) / QEMAX ) )
            ELSE
               XX = HALF*( Q( N )+Q( N-1 )+E( N-1 )+QEMAX*
     $              SQRT( ( ( Q( N )-Q( N-1 )+E( N-1 ) ) / QEMAX )**2+
     $              FOUR*Q( N-1 ) / QEMAX ) )
            END IF
            YY = ( MAX( Q( N ), Q( N-1 ) ) / XX )*
     $           MIN( Q( N ), Q( N-1 ) )
         ELSE
            XX = ZERO
            YY = ZERO
         END IF
         Q( N-1 ) = SIGMA + XX
         Q( N ) = YY + SIGMA
         N = N - 2
         ICONV = ICONV + 2
         GO TO 190
      END IF
*
*     Updating bounds before going to pong
*
      IF( ICONV.EQ.0 ) THEN
         KEND = KE
         SUP = MIN( DM, SUP-TAU )
      ELSE IF( ICONV.GT.0 ) THEN
         SUP = MIN( Q( N ), Q( N-1 ), Q( N-2 ), Q( 1 ), Q( 2 ), Q( 3 ) )
         IF( ICONV.EQ.1 ) THEN
            KEND = K1END
         ELSE IF( ICONV.EQ.2 ) THEN
            KEND = K2END
         ELSE
            KEND = N
         END IF
         ICNT = 0
         MAXIT = 100*N
      END IF
*
*     Checking for splitting in pong
*
      LSPLIT = .FALSE.
      DO 200 KS = N - 3, 3, -1
         IF( E( KS ).LE.TOLY ) THEN
            IF( E( KS )*( MIN( Q( KS+1 ), Q( KS ) ) / ( MIN( Q( KS+1 ),
     $          Q( KS ) )+SIGMA ) ).LE.TOLX ) THEN
               LSPLIT = .TRUE.
               GO TO 210
            END IF
         END IF
  200 CONTINUE
*
      KS = 2
      IF( E( 2 ).LE.TOLZ ) THEN
         LSPLIT = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         T1 = SIGMA + E( 1 )*( SIGMA / ( SIGMA+Q( 1 ) ) )
         IF( E( 2 )*( T1 / ( Q( 1 )+T1 ) ).LE.TOLY ) THEN
            IF( E( 2 )*( Q( 1 ) / ( Q( 1 )+T1 ) ).LE.TOLX ) THEN
               LSPLIT = .TRUE.
            END IF
         END IF
      ELSE
         IF( E( 2 ).LE.( Q( 1 ) / ( Q( 1 )+E( 1 )+Q( 2 ) ) )*Q( 2 )*
     $       TOL2 ) THEN
            LSPLIT = .TRUE.
         END IF
      END IF
      IF( LSPLIT )
     $   GO TO 210
*
      KS = 1
      IF( E( 1 ).LE.TOLZ ) THEN
         LSPLIT = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         IF( E( 1 ).LE.EPS*( SIGMA+Q( 1 ) ) ) THEN
            IF( E( 1 )*( Q( 1 ) / ( Q( 1 )+SIGMA ) ).LE.TOL2*
     $          ( Q( 1 )+SIGMA ) ) THEN
               LSPLIT = .TRUE.
            END IF
         END IF
      ELSE
         IF( E( 1 ).LE.Q( 1 )*TOL2 ) THEN
            LSPLIT = .TRUE.
         END IF
      END IF
*
  210 CONTINUE
      IF( LSPLIT ) THEN
         SUP = MIN( Q( N ), Q( N-1 ), Q( N-2 ) )
         ISP = OFF + 1
         OFF = OFF + KS
         KEND = MAX( 1, KEND-KS )
         N = N - KS
         E( KS ) = SIGMA
         EE( KS ) = ISP
         ICONV = 0
         RETURN
      END IF
*
*     Coincidence
*
      IF( TAU.EQ.ZERO .AND. DM.LE.TOLZ .AND. KEND.NE.N .AND. ICONV.EQ.
     $    0 .AND. ICNT.GT.0 ) THEN
         CALL DCOPY( N-KE, EE( KE ), 1, Q( KE ), 1 )
         Q( N ) = ZERO
         CALL DCOPY( N-KE, QQ( KE+1 ), 1, E( KE ), 1 )
         SUP = ZERO
      END IF
      ICONV = 0
      GO TO 30
*
*     Computation of a new shift when the previous failed (in pong)
*
  220 CONTINUE
      IFL = IFL + 1
      SUP = TAU
*
*     SUP is small or
*     Too many bad shifts (in pong)
*
      IF( SUP.LE.TOLZ .OR. IFL.GE.IFLMAX ) THEN
         TAU = ZERO
         GO TO 140
*
*     The asymptotic shift (in pong)
*
      ELSE
         TAU = MAX( TAU+D, ZERO )
         IF( TAU.LE.TOLZ )
     $      TAU = ZERO
         GO TO 140
      END IF
*
*     End of DLASQ3
*
      END
      SUBROUTINE DLASQ4( N, Q, E, TAU, SUP )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            N
      DOUBLE PRECISION   SUP, TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   E( * ), Q( * )
*     ..
*
*     Purpose
*     =======
*
*     DLASQ4 estimates TAU, the smallest eigenvalue of a matrix. This
*     routine improves the input value of SUP which is an upper bound
*     for the smallest eigenvalue for this matrix .
*
*     Arguments
*     =========
*
*  N       (input) INTEGER
*          On entry, N specifies the number of rows and columns
*          in the matrix. N must be at least 0.
*
*  Q       (input) DOUBLE PRECISION array, dimension (N)
*          Q array
*
*  E       (input) DOUBLE PRECISION array, dimension (N)
*          E array
*
*  TAU     (output) DOUBLE PRECISION
*          Estimate of the shift
*
*  SUP     (input/output) DOUBLE PRECISION
*          Upper bound for the smallest singular value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
      DOUBLE PRECISION   BIS, BIS1
      PARAMETER          ( BIS = 0.9999D+0, BIS1 = 0.7D+0 )
      INTEGER            IFLMAX
      PARAMETER          ( IFLMAX = 5 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IFL
      DOUBLE PRECISION   D, DM, XINF
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
      IFL = 1
      SUP = MIN( SUP, Q( 1 ), Q( 2 ), Q( 3 ), Q( N ), Q( N-1 ),
     $      Q( N-2 ) )
      TAU = SUP*BIS
      XINF = ZERO
   10 CONTINUE
      IF( IFL.EQ.IFLMAX ) THEN
         TAU = XINF
         RETURN
      END IF
      D = Q( 1 ) - TAU
      DM = D
      DO 20 I = 1, N - 2
         D = ( D / ( D+E( I ) ) )*Q( I+1 ) - TAU
         IF( DM.GT.D )
     $      DM = D
         IF( D.LT.ZERO ) THEN
            SUP = TAU
            TAU = MAX( SUP*BIS1**IFL, D+TAU )
            IFL = IFL + 1
            GO TO 10
         END IF
   20 CONTINUE
      D = ( D / ( D+E( N-1 ) ) )*Q( N ) - TAU
      IF( DM.GT.D )
     $   DM = D
      IF( D.LT.ZERO ) THEN
         SUP = TAU
         XINF = MAX( XINF, D+TAU )
         IF( SUP*BIS1**IFL.LE.XINF ) THEN
            TAU = XINF
         ELSE
            TAU = SUP*BIS1**IFL
            IFL = IFL + 1
            GO TO 10
         END IF
      ELSE
         SUP = MIN( SUP, DM+TAU )
      END IF
      RETURN
*
*     End of DLASQ4
*
      END
      SUBROUTINE DLASR( SIDE, PIVOT, DIRECT, M, N, C, S, A, LDA )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, PIVOT, SIDE
      INTEGER            LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( * ), S( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASR   performs the transformation
*
*     A := P*A,   when SIDE = 'L' or 'l'  (  Left-hand side )
*
*     A := A*P',  when SIDE = 'R' or 'r'  ( Right-hand side )
*
*  where A is an m by n real matrix and P is an orthogonal matrix,
*  consisting of a sequence of plane rotations determined by the
*  parameters PIVOT and DIRECT as follows ( z = m when SIDE = 'L' or 'l'
*  and z = n when SIDE = 'R' or 'r' ):
*
*  When  DIRECT = 'F' or 'f'  ( Forward sequence ) then
*
*     P = P( z - 1 )*...*P( 2 )*P( 1 ),
*
*  and when DIRECT = 'B' or 'b'  ( Backward sequence ) then
*
*     P = P( 1 )*P( 2 )*...*P( z - 1 ),
*
*  where  P( k ) is a plane rotation matrix for the following planes:
*
*     when  PIVOT = 'V' or 'v'  ( Variable pivot ),
*        the plane ( k, k + 1 )
*
*     when  PIVOT = 'T' or 't'  ( Top pivot ),
*        the plane ( 1, k + 1 )
*
*     when  PIVOT = 'B' or 'b'  ( Bottom pivot ),
*        the plane ( k, z )
*
*  c( k ) and s( k )  must contain the  cosine and sine that define the
*  matrix  P( k ).  The two by two plane rotation part of the matrix
*  P( k ), R( k ), is assumed to be of the form
*
*     R( k ) = (  c( k )  s( k ) ).
*              ( -s( k )  c( k ) )
*
*  This version vectorises across rows of the array A when SIDE = 'L'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          Specifies whether the plane rotation matrix P is applied to
*          A on the left or the right.
*          = 'L':  Left, compute A := P*A
*          = 'R':  Right, compute A:= A*P'
*
*  DIRECT  (input) CHARACTER*1
*          Specifies whether P is a forward or backward sequence of
*          plane rotations.
*          = 'F':  Forward, P = P( z - 1 )*...*P( 2 )*P( 1 )
*          = 'B':  Backward, P = P( 1 )*P( 2 )*...*P( z - 1 )
*
*  PIVOT   (input) CHARACTER*1
*          Specifies the plane for which P(k) is a plane rotation
*          matrix.
*          = 'V':  Variable pivot, the plane (k,k+1)
*          = 'T':  Top pivot, the plane (1,k+1)
*          = 'B':  Bottom pivot, the plane (k,z)
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  If m <= 1, an immediate
*          return is effected.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  If n <= 1, an
*          immediate return is effected.
*
*  C, S    (input) DOUBLE PRECISION arrays, dimension
*                  (M-1) if SIDE = 'L'
*                  (N-1) if SIDE = 'R'
*          c(k) and s(k) contain the cosine and sine that define the
*          matrix P(k).  The two by two plane rotation part of the
*          matrix P(k), R(k), is assumed to be of the form
*          R( k ) = (  c( k )  s( k ) ).
*                   ( -s( k )  c( k ) )
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          The m by n matrix A.  On exit, A is overwritten by P*A if
*          SIDE = 'R' or by A*P' if SIDE = 'L'.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J
      DOUBLE PRECISION   CTEMP, STEMP, TEMP
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      IF( .NOT.( LSAME( SIDE, 'L' ) .OR. LSAME( SIDE, 'R' ) ) ) THEN
         INFO = 1
      ELSE IF( .NOT.( LSAME( PIVOT, 'V' ) .OR. LSAME( PIVOT,
     $         'T' ) .OR. LSAME( PIVOT, 'B' ) ) ) THEN
         INFO = 2
      ELSE IF( .NOT.( LSAME( DIRECT, 'F' ) .OR. LSAME( DIRECT, 'B' ) ) )
     $          THEN
         INFO = 3
      ELSE IF( M.LT.0 ) THEN
         INFO = 4
      ELSE IF( N.LT.0 ) THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASR ', INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( ( M.EQ.0 ) .OR. ( N.EQ.0 ) )
     $   RETURN
      IF( LSAME( SIDE, 'L' ) ) THEN
*
*        Form  P * A
*
         IF( LSAME( PIVOT, 'V' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 20 J = 1, M - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 10 I = 1, N
                        TEMP = A( J+1, I )
                        A( J+1, I ) = CTEMP*TEMP - STEMP*A( J, I )
                        A( J, I ) = STEMP*TEMP + CTEMP*A( J, I )
   10                CONTINUE
                  END IF
   20          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 40 J = M - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 30 I = 1, N
                        TEMP = A( J+1, I )
                        A( J+1, I ) = CTEMP*TEMP - STEMP*A( J, I )
                        A( J, I ) = STEMP*TEMP + CTEMP*A( J, I )
   30                CONTINUE
                  END IF
   40          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'T' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 60 J = 2, M
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 50 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = CTEMP*TEMP - STEMP*A( 1, I )
                        A( 1, I ) = STEMP*TEMP + CTEMP*A( 1, I )
   50                CONTINUE
                  END IF
   60          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 80 J = M, 2, -1
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 70 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = CTEMP*TEMP - STEMP*A( 1, I )
                        A( 1, I ) = STEMP*TEMP + CTEMP*A( 1, I )
   70                CONTINUE
                  END IF
   80          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'B' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 100 J = 1, M - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 90 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = STEMP*A( M, I ) + CTEMP*TEMP
                        A( M, I ) = CTEMP*A( M, I ) - STEMP*TEMP
   90                CONTINUE
                  END IF
  100          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 120 J = M - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 110 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = STEMP*A( M, I ) + CTEMP*TEMP
                        A( M, I ) = CTEMP*A( M, I ) - STEMP*TEMP
  110                CONTINUE
                  END IF
  120          CONTINUE
            END IF
         END IF
      ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*        Form A * P'
*
         IF( LSAME( PIVOT, 'V' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 140 J = 1, N - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 130 I = 1, M
                        TEMP = A( I, J+1 )
                        A( I, J+1 ) = CTEMP*TEMP - STEMP*A( I, J )
                        A( I, J ) = STEMP*TEMP + CTEMP*A( I, J )
  130                CONTINUE
                  END IF
  140          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 160 J = N - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 150 I = 1, M
                        TEMP = A( I, J+1 )
                        A( I, J+1 ) = CTEMP*TEMP - STEMP*A( I, J )
                        A( I, J ) = STEMP*TEMP + CTEMP*A( I, J )
  150                CONTINUE
                  END IF
  160          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'T' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 180 J = 2, N
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 170 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = CTEMP*TEMP - STEMP*A( I, 1 )
                        A( I, 1 ) = STEMP*TEMP + CTEMP*A( I, 1 )
  170                CONTINUE
                  END IF
  180          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 200 J = N, 2, -1
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 190 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = CTEMP*TEMP - STEMP*A( I, 1 )
                        A( I, 1 ) = STEMP*TEMP + CTEMP*A( I, 1 )
  190                CONTINUE
                  END IF
  200          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'B' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 220 J = 1, N - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 210 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = STEMP*A( I, N ) + CTEMP*TEMP
                        A( I, N ) = CTEMP*A( I, N ) - STEMP*TEMP
  210                CONTINUE
                  END IF
  220          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 240 J = N - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 230 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = STEMP*A( I, N ) + CTEMP*TEMP
                        A( I, N ) = CTEMP*A( I, N ) - STEMP*TEMP
  230                CONTINUE
                  END IF
  240          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of DLASR
*
      END
      SUBROUTINE DLASRT( ID, N, D, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          ID
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * )
*     ..
*
*  Purpose
*  =======
*
*  Sort the numbers in D in increasing order (if ID = 'I') or
*  in decreasing order (if ID = 'D' ).
*
*  Use Quick Sort, reverting to Insertion sort on arrays of
*  size <= 20. Dimension of STACK limits N to about 2**32.
*
*  Arguments
*  =========
*
*  ID      (input) CHARACTER*1
*          = 'I': sort D in increasing order;
*          = 'D': sort D in decreasing order.
*
*  N       (input) INTEGER
*          The length of the array D.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the array to be sorted.
*          On exit, D has been sorted into increasing order
*          (D(1) <= ... <= D(N) ) or into decreasing order
*          (D(1) >= ... >= D(N) ), depending on ID.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            SELECT
      PARAMETER          ( SELECT = 20 )
*     ..
*     .. Local Scalars ..
      INTEGER            DIR, ENDD, I, J, START, STKPNT
      DOUBLE PRECISION   D1, D2, D3, DMNMX, TMP
*     ..
*     .. Local Arrays ..
      INTEGER            STACK( 2, 32 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input paramters.
*
      INFO = 0
      DIR = -1
      IF( LSAME( ID, 'D' ) ) THEN
         DIR = 0
      ELSE IF( LSAME( ID, 'I' ) ) THEN
         DIR = 1
      END IF
      IF( DIR.EQ.-1 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASRT', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.LE.1 )
     $   RETURN
*
      STKPNT = 1
      STACK( 1, 1 ) = 1
      STACK( 2, 1 ) = N
   10 CONTINUE
      START = STACK( 1, STKPNT )
      ENDD = STACK( 2, STKPNT )
      STKPNT = STKPNT - 1
      IF( ENDD-START.LE.SELECT .AND. ENDD-START.GT.0 ) THEN
*
*        Do Insertion sort on D( START:ENDD )
*
         IF( DIR.EQ.0 ) THEN
*
*           Sort into decreasing order
*
            DO 30 I = START + 1, ENDD
               DO 20 J = I, START + 1, -1
                  IF( D( J ).GT.D( J-1 ) ) THEN
                     DMNMX = D( J )
                     D( J ) = D( J-1 )
                     D( J-1 ) = DMNMX
                  ELSE
                     GO TO 30
                  END IF
   20          CONTINUE
   30       CONTINUE
*
         ELSE
*
*           Sort into increasing order
*
            DO 50 I = START + 1, ENDD
               DO 40 J = I, START + 1, -1
                  IF( D( J ).LT.D( J-1 ) ) THEN
                     DMNMX = D( J )
                     D( J ) = D( J-1 )
                     D( J-1 ) = DMNMX
                  ELSE
                     GO TO 50
                  END IF
   40          CONTINUE
   50       CONTINUE
*
         END IF
*
      ELSE IF( ENDD-START.GT.SELECT ) THEN
*
*        Partition D( START:ENDD ) and stack parts, largest one first
*
*        Choose partition entry as median of 3
*
         D1 = D( START )
         D2 = D( ENDD )
         I = ( START+ENDD ) / 2
         D3 = D( I )
         IF( D1.LT.D2 ) THEN
            IF( D3.LT.D1 ) THEN
               DMNMX = D1
            ELSE IF( D3.LT.D2 ) THEN
               DMNMX = D3
            ELSE
               DMNMX = D2
            END IF
         ELSE
            IF( D3.LT.D2 ) THEN
               DMNMX = D2
            ELSE IF( D3.LT.D1 ) THEN
               DMNMX = D3
            ELSE
               DMNMX = D1
            END IF
         END IF
*
         IF( DIR.EQ.0 ) THEN
*
*           Sort into decreasing order
*
            I = START - 1
            J = ENDD + 1
   60       CONTINUE
   70       CONTINUE
            J = J - 1
            IF( D( J ).LT.DMNMX )
     $         GO TO 70
   80       CONTINUE
            I = I + 1
            IF( D( I ).GT.DMNMX )
     $         GO TO 80
            IF( I.LT.J ) THEN
               TMP = D( I )
               D( I ) = D( J )
               D( J ) = TMP
               GO TO 60
            END IF
            IF( J-START.GT.ENDD-J-1 ) THEN
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
            ELSE
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
            END IF
         ELSE
*
*           Sort into increasing order
*
            I = START - 1
            J = ENDD + 1
   90       CONTINUE
  100       CONTINUE
            J = J - 1
            IF( D( J ).GT.DMNMX )
     $         GO TO 100
  110       CONTINUE
            I = I + 1
            IF( D( I ).LT.DMNMX )
     $         GO TO 110
            IF( I.LT.J ) THEN
               TMP = D( I )
               D( I ) = D( J )
               D( J ) = TMP
               GO TO 90
            END IF
            IF( J-START.GT.ENDD-J-1 ) THEN
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
            ELSE
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
            END IF
         END IF
      END IF
      IF( STKPNT.GT.0 )
     $   GO TO 10
      RETURN
*
*     End of DLASRT
*
      END
      SUBROUTINE DLASSQ( N, X, INCX, SCALE, SUMSQ )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   SCALE, SUMSQ
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASSQ  returns the values  scl  and  smsq  such that
*
*     ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
*
*  where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
*  assumed to be non-negative and  scl  returns the value
*
*     scl = max( scale, abs( x( i ) ) ).
*
*  scale and sumsq must be supplied in SCALE and SUMSQ and
*  scl and smsq are overwritten on SCALE and SUMSQ respectively.
*
*  The routine makes only one pass through the vector x.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of elements to be used from the vector X.
*
*  X       (input) DOUBLE PRECISION
*          The vector for which a scaled sum of squares is computed.
*             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
*
*  INCX    (input) INTEGER
*          The increment between successive values of the vector X.
*          INCX > 0.
*
*  SCALE   (input/output) DOUBLE PRECISION
*          On entry, the value  scale  in the equation above.
*          On exit, SCALE is overwritten with  scl , the scaling factor
*          for the sum of squares.
*
*  SUMSQ   (input/output) DOUBLE PRECISION
*          On entry, the value  sumsq  in the equation above.
*          On exit, SUMSQ is overwritten with  smsq , the basic sum of
*          squares from which  scl  has been factored out.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            IX
      DOUBLE PRECISION   ABSXI
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
      IF( N.GT.0 ) THEN
         DO 10 IX = 1, 1 + ( N-1 )*INCX, INCX
            IF( X( IX ).NE.ZERO ) THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI ) THEN
                  SUMSQ = 1 + SUMSQ*( SCALE / ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SUMSQ = SUMSQ + ( ABSXI / SCALE )**2
               END IF
            END IF
   10    CONTINUE
      END IF
      RETURN
*
*     End of DLASSQ
*
      END
      SUBROUTINE DLASV2( F, G, H, SSMIN, SSMAX, SNR, CSR, SNL, CSL )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   CSL, CSR, F, G, H, SNL, SNR, SSMAX, SSMIN
*     ..
*
*  Purpose
*  =======
*
*  DLASV2 computes the singular value decomposition of a 2-by-2
*  triangular matrix
*     [  F   G  ]
*     [  0   H  ].
*  On return, abs(SSMAX) is the larger singular value, abs(SSMIN) is the
*  smaller singular value, and (CSL,SNL) and (CSR,SNR) are the left and
*  right singular vectors for abs(SSMAX), giving the decomposition
*
*     [ CSL  SNL ] [  F   G  ] [ CSR -SNR ]  =  [ SSMAX   0   ]
*     [-SNL  CSL ] [  0   H  ] [ SNR  CSR ]     [  0    SSMIN ].
*
*  Arguments
*  =========
*
*  F       (input) DOUBLE PRECISION
*          The (1,1) element of the 2-by-2 matrix.
*
*  G       (input) DOUBLE PRECISION
*          The (1,2) element of the 2-by-2 matrix.
*
*  H       (input) DOUBLE PRECISION
*          The (2,2) element of the 2-by-2 matrix.
*
*  SSMIN   (output) DOUBLE PRECISION
*          abs(SSMIN) is the smaller singular value.
*
*  SSMAX   (output) DOUBLE PRECISION
*          abs(SSMAX) is the larger singular value.
*
*  SNL     (output) DOUBLE PRECISION
*  CSL     (output) DOUBLE PRECISION
*          The vector (CSL, SNL) is a unit left singular vector for the
*          singular value abs(SSMAX).
*
*  SNR     (output) DOUBLE PRECISION
*  CSR     (output) DOUBLE PRECISION
*          The vector (CSR, SNR) is a unit right singular vector for the
*          singular value abs(SSMAX).
*
*  Further Details
*  ===============
*
*  Any input parameter may be aliased with any output parameter.
*
*  Barring over/underflow and assuming a guard digit in subtraction, all
*  output quantities are correct to within a few units in the last
*  place (ulps).
*
*  In IEEE arithmetic, the code works correctly if one matrix element is
*  infinite.
*
*  Overflow will not occur unless the largest singular value itself
*  overflows or is within a few ulps of overflow. (On machines with
*  partial overflow, like the Cray, overflow may occur if the largest
*  singular value is within a factor of 2 of overflow.)
*
*  Underflow is harmless if underflow is gradual. Otherwise, results
*  may correspond to a matrix modified by perturbations of size near
*  the underflow threshold.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = 0.5D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
      DOUBLE PRECISION   FOUR
      PARAMETER          ( FOUR = 4.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            GASMAL, SWAP
      INTEGER            PMAX
      DOUBLE PRECISION   A, CLT, CRT, D, FA, FT, GA, GT, HA, HT, L, M,
     $                   MM, R, S, SLT, SRT, T, TEMP, TSIGN, TT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN, SQRT
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Executable Statements ..
*
      FT = F
      FA = ABS( FT )
      HT = H
      HA = ABS( H )
*
*     PMAX points to the maximum absolute element of matrix
*       PMAX = 1 if F largest in absolute values
*       PMAX = 2 if G largest in absolute values
*       PMAX = 3 if H largest in absolute values
*
      PMAX = 1
      SWAP = ( HA.GT.FA )
      IF( SWAP ) THEN
         PMAX = 3
         TEMP = FT
         FT = HT
         HT = TEMP
         TEMP = FA
         FA = HA
         HA = TEMP
*
*        Now FA .ge. HA
*
      END IF
      GT = G
      GA = ABS( GT )
      IF( GA.EQ.ZERO ) THEN
*
*        Diagonal matrix
*
         SSMIN = HA
         SSMAX = FA
         CLT = ONE
         CRT = ONE
         SLT = ZERO
         SRT = ZERO
      ELSE
         GASMAL = .TRUE.
         IF( GA.GT.FA ) THEN
            PMAX = 2
            IF( ( FA / GA ).LT.DLAMCH( 'EPS' ) ) THEN
*
*              Case of very large GA
*
               GASMAL = .FALSE.
               SSMAX = GA
               IF( HA.GT.ONE ) THEN
                  SSMIN = FA / ( GA / HA )
               ELSE
                  SSMIN = ( FA / GA )*HA
               END IF
               CLT = ONE
               SLT = HT / GT
               SRT = ONE
               CRT = FT / GT
            END IF
         END IF
         IF( GASMAL ) THEN
*
*           Normal case
*
            D = FA - HA
            IF( D.EQ.FA ) THEN
*
*              Copes with infinite F or H
*
               L = ONE
            ELSE
               L = D / FA
            END IF
*
*           Note that 0 .le. L .le. 1
*
            M = GT / FT
*
*           Note that abs(M) .le. 1/macheps
*
            T = TWO - L
*
*           Note that T .ge. 1
*
            MM = M*M
            TT = T*T
            S = SQRT( TT+MM )
*
*           Note that 1 .le. S .le. 1 + 1/macheps
*
            IF( L.EQ.ZERO ) THEN
               R = ABS( M )
            ELSE
               R = SQRT( L*L+MM )
            END IF
*
*           Note that 0 .le. R .le. 1 + 1/macheps
*
            A = HALF*( S+R )
*
*           Note that 1 .le. A .le. 1 + abs(M)
*
            SSMIN = HA / A
            SSMAX = FA*A
            IF( MM.EQ.ZERO ) THEN
*
*              Note that M is very tiny
*
               IF( L.EQ.ZERO ) THEN
                  T = SIGN( TWO, FT )*SIGN( ONE, GT )
               ELSE
                  T = GT / SIGN( D, FT ) + M / T
               END IF
            ELSE
               T = ( M / ( S+T )+M / ( R+L ) )*( ONE+A )
            END IF
            L = SQRT( T*T+FOUR )
            CRT = TWO / L
            SRT = T / L
            CLT = ( CRT+SRT*M ) / A
            SLT = ( HT / FT )*SRT / A
         END IF
      END IF
      IF( SWAP ) THEN
         CSL = SRT
         SNL = CRT
         CSR = SLT
         SNR = CLT
      ELSE
         CSL = CLT
         SNL = SLT
         CSR = CRT
         SNR = SRT
      END IF
*
*     Correct signs of SSMAX and SSMIN
*
      IF( PMAX.EQ.1 )
     $   TSIGN = SIGN( ONE, CSR )*SIGN( ONE, CSL )*SIGN( ONE, F )
      IF( PMAX.EQ.2 )
     $   TSIGN = SIGN( ONE, SNR )*SIGN( ONE, CSL )*SIGN( ONE, G )
      IF( PMAX.EQ.3 )
     $   TSIGN = SIGN( ONE, SNR )*SIGN( ONE, SNL )*SIGN( ONE, H )
      SSMAX = SIGN( SSMAX, TSIGN )
      SSMIN = SIGN( SSMIN, TSIGN*SIGN( ONE, F )*SIGN( ONE, H ) )
      RETURN
*
*     End of DLASV2
*
      END
      SUBROUTINE DORG2R( M, N, K, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DORG2R generates an m by n real matrix Q with orthonormal columns,
*  which is defined as the first n columns of a product of k elementary
*  reflectors of order m
*
*        Q  =  H(1) H(2) . . . H(k)
*
*  as returned by DGEQRF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. M >= N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. N >= K >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the i-th column must contain the vector which
*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*          returned by DGEQRF in the first k columns of its array
*          argument A.
*          On exit, the m-by-n matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGEQRF.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, L
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. N.GT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.N ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORG2R', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.LE.0 )
     $   RETURN
*
*     Initialise columns k+1:n to columns of the unit matrix
*
      DO 20 J = K + 1, N
         DO 10 L = 1, M
            A( L, J ) = ZERO
   10    CONTINUE
         A( J, J ) = ONE
   20 CONTINUE
*
      DO 40 I = K, 1, -1
*
*        Apply H(i) to A(i:m,i:n) from the left
*
         IF( I.LT.N ) THEN
            A( I, I ) = ONE
            CALL DLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                  A( I, I+1 ), LDA, WORK )
         END IF
         IF( I.LT.M )
     $      CALL DSCAL( M-I, -TAU( I ), A( I+1, I ), 1 )
         A( I, I ) = ONE - TAU( I )
*
*        Set A(1:i-1,i) to zero
*
         DO 30 L = 1, I - 1
            A( L, I ) = ZERO
   30    CONTINUE
   40 CONTINUE
      RETURN
*
*     End of DORG2R
*
      END
      SUBROUTINE DORGBR( VECT, M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          VECT
      INTEGER            INFO, K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DORGBR generates one of the real orthogonal matrices Q or P**T
*  determined by DGEBRD when reducing a real matrix A to bidiagonal
*  form: A = Q * B * P**T.  Q and P**T are defined as products of
*  elementary reflectors H(i) or G(i) respectively.
*
*  If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
*  is of order M:
*  if m >= k, Q = H(1) H(2) . . . H(k) and DORGBR returns the first n
*  columns of Q, where m >= n >= k;
*  if m < k, Q = H(1) H(2) . . . H(m-1) and DORGBR returns Q as an
*  M-by-M matrix.
*
*  If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**T
*  is of order N:
*  if k < n, P**T = G(k) . . . G(2) G(1) and DORGBR returns the first m
*  rows of P**T, where n >= m >= k;
*  if k >= n, P**T = G(n-1) . . . G(2) G(1) and DORGBR returns P**T as
*  an N-by-N matrix.
*
*  Arguments
*  =========
*
*  VECT    (input) CHARACTER*1
*          Specifies whether the matrix Q or the matrix P**T is
*          required, as defined in the transformation applied by DGEBRD:
*          = 'Q':  generate Q;
*          = 'P':  generate P**T.
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q or P**T to be returned.
*          M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q or P**T to be returned.
*          N >= 0.
*          If VECT = 'Q', M >= N >= min(M,K);
*          if VECT = 'P', N >= M >= min(N,K).
*
*  K       (input) INTEGER
*          If VECT = 'Q', the number of columns in the original M-by-K
*          matrix reduced by DGEBRD.
*          If VECT = 'P', the number of rows in the original K-by-N
*          matrix reduced by DGEBRD.
*          K >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the vectors which define the elementary reflectors,
*          as returned by DGEBRD.
*          On exit, the M-by-N matrix Q or P**T.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) DOUBLE PRECISION array, dimension
*                                (min(M,K)) if VECT = 'Q'
*                                (min(N,K)) if VECT = 'P'
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i) or G(i), which determines Q or P**T, as
*          returned by DGEBRD in its array argument TAUQ or TAUP.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,min(M,N)).
*          For optimum performance LWORK >= min(M,N)*NB, where NB
*          is the optimal blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            WANTQ
      INTEGER            I, IINFO, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DORGLQ, DORGQR, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      WANTQ = LSAME( VECT, 'Q' )
      IF( .NOT.WANTQ .AND. .NOT.LSAME( VECT, 'P' ) ) THEN
         INFO = -1
      ELSE IF( M.LT.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 .OR. ( WANTQ .AND. ( N.GT.M .OR. N.LT.MIN( M,
     $         K ) ) ) .OR. ( .NOT.WANTQ .AND. ( M.GT.N .OR. M.LT.
     $         MIN( N, K ) ) ) ) THEN
         INFO = -3
      ELSE IF( K.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -6
      ELSE IF( LWORK.LT.MAX( 1, MIN( M, N ) ) ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORGBR', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      IF( WANTQ ) THEN
*
*        Form Q, determined by a call to DGEBRD to reduce an m-by-k
*        matrix
*
         IF( M.GE.K ) THEN
*
*           If m >= k, assume m >= n >= k
*
            CALL DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, IINFO )
*
         ELSE
*
*           If m < k, assume m = n
*
*           Shift the vectors which define the elementary reflectors one
*           column to the right, and set the first row and column of Q
*           to those of the unit matrix
*
            DO 20 J = M, 2, -1
               A( 1, J ) = ZERO
               DO 10 I = J + 1, M
                  A( I, J ) = A( I, J-1 )
   10          CONTINUE
   20       CONTINUE
            A( 1, 1 ) = ONE
            DO 30 I = 2, M
               A( I, 1 ) = ZERO
   30       CONTINUE
            IF( M.GT.1 ) THEN
*
*              Form Q(2:m,2:m)
*
               CALL DORGQR( M-1, M-1, M-1, A( 2, 2 ), LDA, TAU, WORK,
     $                      LWORK, IINFO )
            END IF
         END IF
      ELSE
*
*        Form P', determined by a call to DGEBRD to reduce a k-by-n
*        matrix
*
         IF( K.LT.N ) THEN
*
*           If k < n, assume k <= m <= n
*
            CALL DORGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, IINFO )
*
         ELSE
*
*           If k >= n, assume m = n
*
*           Shift the vectors which define the elementary reflectors one
*           row downward, and set the first row and column of P' to
*           those of the unit matrix
*
            A( 1, 1 ) = ONE
            DO 40 I = 2, N
               A( I, 1 ) = ZERO
   40       CONTINUE
            DO 60 J = 2, N
               DO 50 I = J - 1, 2, -1
                  A( I, J ) = A( I-1, J )
   50          CONTINUE
               A( 1, J ) = ZERO
   60       CONTINUE
            IF( N.GT.1 ) THEN
*
*              Form P'(2:n,2:n)
*
               CALL DORGLQ( N-1, N-1, N-1, A( 2, 2 ), LDA, TAU, WORK,
     $                      LWORK, IINFO )
            END IF
         END IF
      END IF
      RETURN
*
*     End of DORGBR
*
      END
      SUBROUTINE DORGL2( M, N, K, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DORGL2 generates an m by n real matrix Q with orthonormal rows,
*  which is defined as the first m rows of a product of k elementary
*  reflectors of order n
*
*        Q  =  H(k) . . . H(2) H(1)
*
*  as returned by DGELQF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. N >= M.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. M >= K >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the i-th row must contain the vector which defines
*          the elementary reflector H(i), for i = 1,2,...,k, as returned
*          by DGELQF in the first k rows of its array argument A.
*          On exit, the m-by-n matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGELQF.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (M)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, L
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.M ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORGL2', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.LE.0 )
     $   RETURN
*
      IF( K.LT.M ) THEN
*
*        Initialise rows k+1:m to rows of the unit matrix
*
         DO 20 J = 1, N
            DO 10 L = K + 1, M
               A( L, J ) = ZERO
   10       CONTINUE
            IF( J.GT.K .AND. J.LE.M )
     $         A( J, J ) = ONE
   20    CONTINUE
      END IF
*
      DO 40 I = K, 1, -1
*
*        Apply H(i) to A(i:m,i:n) from the right
*
         IF( I.LT.N ) THEN
            IF( I.LT.M ) THEN
               A( I, I ) = ONE
               CALL DLARF( 'Right', M-I, N-I+1, A( I, I ), LDA,
     $                     TAU( I ), A( I+1, I ), LDA, WORK )
            END IF
            CALL DSCAL( N-I, -TAU( I ), A( I, I+1 ), LDA )
         END IF
         A( I, I ) = ONE - TAU( I )
*
*        Set A(1:i-1,i) to zero
*
         DO 30 L = 1, I - 1
            A( I, L ) = ZERO
   30    CONTINUE
   40 CONTINUE
      RETURN
*
*     End of DORGL2
*
      END
      SUBROUTINE DORGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DORGLQ generates an M-by-N real matrix Q with orthonormal rows,
*  which is defined as the first M rows of a product of K elementary
*  reflectors of order N
*
*        Q  =  H(k) . . . H(2) H(1)
*
*  as returned by DGELQF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. N >= M.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. M >= K >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the i-th row must contain the vector which defines
*          the elementary reflector H(i), for i = 1,2,...,k, as returned
*          by DGELQF in the first k rows of its array argument A.
*          On exit, the M-by-N matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGELQF.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,M).
*          For optimum performance LWORK >= M*NB, where NB is
*          the optimal blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IB, IINFO, IWS, J, KI, KK, L, LDWORK, NB,
     $                   NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARFB, DLARFT, DORGL2, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.M ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LWORK.LT.MAX( 1, M ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORGLQ', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.LE.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Determine the block size.
*
      NB = ILAENV (1, 'DORGLQ', ' ', M, N, -1)
      NBMIN = 2
      NX = 0
      IWS = M
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX (0, ILAENV (3, 'DORGLQ', ' ', M, N, -1))
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = M
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX (2, ILAENV (2, 'DORGLQ', ' ', M, N, -1))
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code after the last block.
*        The first kk rows are handled by the block method.
*
         KI = ( ( K-NX-1 ) / NB )*NB
         KK = MIN( K, KI+NB )
*
*        Set A(kk+1:m,1:kk) to zero.
*
         DO 20 J = 1, KK
            DO 10 I = KK + 1, M
               A( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
      ELSE
         KK = 0
      END IF
*
*     Use unblocked code for the last or only block.
*
      IF( KK.LT.M )
     $   CALL DORGL2( M-KK, N-KK, K-KK, A( KK+1, KK+1 ), LDA,
     $                TAU( KK+1 ), WORK, IINFO )
*
      IF( KK.GT.0 ) THEN
*
*        Use blocked code
*
         DO 50 I = KI + 1, 1, -NB
            IB = MIN( NB, K-I+1 )
            IF( I+IB.LE.M ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL DLARFT( 'Forward', 'Rowwise', N-I+1, IB, A( I, I ),
     $                      LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H' to A(i+ib:m,i:n) from the right
*
               CALL DLARFB( 'Right', 'Transpose', 'Forward', 'Rowwise',
     $                      M-I-IB+1, N-I+1, IB, A( I, I ), LDA, WORK,
     $                      LDWORK, A( I+IB, I ), LDA, WORK( IB+1 ),
     $                      LDWORK )
            END IF
*
*           Apply H' to columns i:n of current block
*
            CALL DORGL2( IB, N-I+1, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
*
*           Set columns 1:i-1 of current block to zero
*
            DO 40 J = 1, I - 1
               DO 30 L = I, I + IB - 1
                  A( L, J ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
      END IF
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of DORGLQ
*
      END
      SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DORGQR generates an M-by-N real matrix Q with orthonormal columns,
*  which is defined as the first N columns of a product of K elementary
*  reflectors of order M
*
*        Q  =  H(1) H(2) . . . H(k)
*
*  as returned by DGEQRF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. M >= N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. N >= K >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the i-th column must contain the vector which
*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*          returned by DGEQRF in the first k columns of its array
*          argument A.
*          On exit, the M-by-N matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGEQRF.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,N).
*          For optimum performance LWORK >= N*NB, where NB is the
*          optimal blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IB, IINFO, IWS, J, KI, KK, L, LDWORK, NB,
     $                   NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARFB, DLARFT, DORG2R, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. N.GT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.N ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LWORK.LT.MAX( 1, N ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORGQR', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.LE.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Determine the block size.
*
      NB = ILAENV (1, 'DORGQR', ' ', M, N, -1)
      NBMIN = 2
      NX = 0
      IWS = N
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX (0, ILAENV (3, 'DORGQR', ' ', M, N, -1))
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX (2, ILAENV (2, 'DORGQR', ' ', M, N, -1))
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code after the last block.
*        The first kk columns are handled by the block method.
*
         KI = ( ( K-NX-1 ) / NB )*NB
         KK = MIN( K, KI+NB )
*
*        Set A(1:kk,kk+1:n) to zero.
*
         DO 20 J = KK + 1, N
            DO 10 I = 1, KK
               A( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
      ELSE
         KK = 0
      END IF
*
*     Use unblocked code for the last or only block.
*
      IF( KK.LT.N )
     $   CALL DORG2R( M-KK, N-KK, K-KK, A( KK+1, KK+1 ), LDA,
     $                TAU( KK+1 ), WORK, IINFO )
*
      IF( KK.GT.0 ) THEN
*
*        Use blocked code
*
         DO 50 I = KI + 1, 1, -NB
            IB = MIN( NB, K-I+1 )
            IF( I+IB.LE.N ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL DLARFT( 'Forward', 'Columnwise', M-I+1, IB,
     $                      A( I, I ), LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H to A(i:m,i+ib:n) from the left
*
               CALL DLARFB( 'Left', 'No transpose', 'Forward',
     $                      'Columnwise', M-I+1, N-I-IB+1, IB,
     $                      A( I, I ), LDA, WORK, LDWORK, A( I, I+IB ),
     $                      LDA, WORK( IB+1 ), LDWORK )
            END IF
*
*           Apply H to rows i:m of current block
*
            CALL DORG2R( M-I+1, IB, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
*
*           Set rows 1:i-1 of current block to zero
*
            DO 40 J = I, I + IB - 1
               DO 30 L = 1, I - 1
                  A( L, J ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
      END IF
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of DORGQR
*
      END
      SUBROUTINE DORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DORM2R overwrites the general real m by n matrix C with
*
*        Q * C  if SIDE = 'L' and TRANS = 'N', or
*
*        Q'* C  if SIDE = 'L' and TRANS = 'T', or
*
*        C * Q  if SIDE = 'R' and TRANS = 'N', or
*
*        C * Q' if SIDE = 'R' and TRANS = 'T',
*
*  where Q is a real orthogonal matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(1) H(2) . . . H(k)
*
*  as returned by DGEQRF. Q is of order m if SIDE = 'L' and of order n
*  if SIDE = 'R'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q or Q' from the Left
*          = 'R': apply Q or Q' from the Right
*
*  TRANS   (input) CHARACTER*1
*          = 'N': apply Q  (No transpose)
*          = 'T': apply Q' (Transpose)
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines
*          the matrix Q.
*          If SIDE = 'L', M >= K >= 0;
*          if SIDE = 'R', N >= K >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,K)
*          The i-th column must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          DGEQRF in the first k columns of its array argument A.
*          A is modified by the routine but restored on exit.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*          If SIDE = 'L', LDA >= max(1,M);
*          if SIDE = 'R', LDA >= max(1,N).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGEQRF.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*                                   (N) if SIDE = 'L',
*                                   (M) if SIDE = 'R'
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
      DOUBLE PRECISION   AII
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q
*
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORM2R', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     $   RETURN
*
      IF( ( LEFT .AND. .NOT.NOTRAN ) .OR. ( .NOT.LEFT .AND. NOTRAN ) )
     $     THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      END IF
*
      IF( LEFT ) THEN
         NI = N
         JC = 1
      ELSE
         MI = M
         IC = 1
      END IF
*
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
*
*           H(i) is applied to C(i:m,1:n)
*
            MI = M - I + 1
            IC = I
         ELSE
*
*           H(i) is applied to C(1:m,i:n)
*
            NI = N - I + 1
            JC = I
         END IF
*
*        Apply H(i)
*
         AII = A( I, I )
         A( I, I ) = ONE
         CALL DLARF( SIDE, MI, NI, A( I, I ), 1, TAU( I ), C( IC, JC ),
     $               LDC, WORK )
         A( I, I ) = AII
   10 CONTINUE
      RETURN
*
*     End of DORM2R
*
      END
      SUBROUTINE DORMBR( VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,
     $                   LDC, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS, VECT
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ),
     $                   WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  If VECT = 'Q', DORMBR overwrites the general real M-by-N matrix C
*  with
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      Q * C          C * Q
*  TRANS = 'T':      Q**T * C       C * Q**T
*
*  If VECT = 'P', DORMBR overwrites the general real M-by-N matrix C
*  with
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      P * C          C * P
*  TRANS = 'T':      P**T * C       C * P**T
*
*  Here Q and P**T are the orthogonal matrices determined by DGEBRD when
*  reducing a real matrix A to bidiagonal form: A = Q * B * P**T. Q and
*  P**T are defined as products of elementary reflectors H(i) and G(i)
*  respectively.
*
*  Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
*  order of the orthogonal matrix Q or P**T that is applied.
*
*  If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
*  if nq >= k, Q = H(1) H(2) . . . H(k);
*  if nq < k, Q = H(1) H(2) . . . H(nq-1).
*
*  If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
*  if k < nq, P = G(1) G(2) . . . G(k);
*  if k >= nq, P = G(1) G(2) . . . G(nq-1).
*
*  Arguments
*  =========
*
*  VECT    (input) CHARACTER*1
*          = 'Q': apply Q or Q**T;
*          = 'P': apply P or P**T.
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q, Q**T, P or P**T from the Left;
*          = 'R': apply Q, Q**T, P or P**T from the Right.
*
*  TRANS   (input) CHARACTER*1
*          = 'N':  No transpose, apply Q  or P;
*          = 'T':  Transpose, apply Q**T or P**T.
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          If VECT = 'Q', the number of columns in the original
*          matrix reduced by DGEBRD.
*          If VECT = 'P', the number of rows in the original
*          matrix reduced by DGEBRD.
*          K >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension
*                                (LDA,min(nq,K)) if VECT = 'Q'
*                                (LDA,nq)        if VECT = 'P'
*          The vectors which define the elementary reflectors H(i) and
*          G(i), whose products determine the matrices Q and P, as
*          returned by DGEBRD.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*          If VECT = 'Q', LDA >= max(1,nq);
*          if VECT = 'P', LDA >= max(1,min(nq,K)).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (min(nq,K))
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i) or G(i) which determines Q or P, as returned
*          by DGEBRD in the array argument TAUQ or TAUP.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the M-by-N matrix C.
*          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q
*          or P*C or P**T*C or C*P or C*P**T.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If SIDE = 'L', LWORK >= max(1,N);
*          if SIDE = 'R', LWORK >= max(1,M).
*          For optimum performance LWORK >= N*NB if SIDE = 'L', and
*          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
*          blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            APPLYQ, LEFT, NOTRAN
      CHARACTER          TRANST
      INTEGER            I1, I2, IINFO, MI, NI, NQ, NW
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DORMLQ, DORMQR, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      APPLYQ = LSAME( VECT, 'Q' )
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q or P and NW is the minimum dimension of WORK
*
      IF( LEFT ) THEN
         NQ = M
         NW = N
      ELSE
         NQ = N
         NW = M
      END IF
      IF( .NOT.APPLYQ .AND. .NOT.LSAME( VECT, 'P' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -3
      ELSE IF( M.LT.0 ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( K.LT.0 ) THEN
         INFO = -6
      ELSE IF( ( APPLYQ .AND. LDA.LT.MAX( 1, NQ ) ) .OR.
     $         ( .NOT.APPLYQ .AND. LDA.LT.MAX( 1, MIN( NQ, K ) ) ) )
     $          THEN
         INFO = -8
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -11
      ELSE IF( LWORK.LT.MAX( 1, NW ) ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORMBR', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      WORK( 1 ) = 1
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
*
      IF( APPLYQ ) THEN
*
*        Apply Q
*
         IF( NQ.GE.K ) THEN
*
*           Q was determined by a call to DGEBRD with nq >= k
*
            CALL DORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, IINFO )
         ELSE IF( NQ.GT.1 ) THEN
*
*           Q was determined by a call to DGEBRD with nq < k
*
            IF( LEFT ) THEN
               MI = M - 1
               NI = N
               I1 = 2
               I2 = 1
            ELSE
               MI = M
               NI = N - 1
               I1 = 1
               I2 = 2
            END IF
            CALL DORMQR( SIDE, TRANS, MI, NI, NQ-1, A( 2, 1 ), LDA, TAU,
     $                   C( I1, I2 ), LDC, WORK, LWORK, IINFO )
         END IF
      ELSE
*
*        Apply P
*
         IF( NOTRAN ) THEN
            TRANST = 'T'
         ELSE
            TRANST = 'N'
         END IF
         IF( NQ.GT.K ) THEN
*
*           P was determined by a call to DGEBRD with nq > k
*
            CALL DORMLQ( SIDE, TRANST, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, IINFO )
         ELSE IF( NQ.GT.1 ) THEN
*
*           P was determined by a call to DGEBRD with nq <= k
*
            IF( LEFT ) THEN
               MI = M - 1
               NI = N
               I1 = 2
               I2 = 1
            ELSE
               MI = M
               NI = N - 1
               I1 = 1
               I2 = 2
            END IF
            CALL DORMLQ( SIDE, TRANST, MI, NI, NQ-1, A( 1, 2 ), LDA,
     $                   TAU, C( I1, I2 ), LDC, WORK, LWORK, IINFO )
         END IF
      END IF
      RETURN
*
*     End of DORMBR
*
      END
      SUBROUTINE DORML2( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DORML2 overwrites the general real m by n matrix C with
*
*        Q * C  if SIDE = 'L' and TRANS = 'N', or
*
*        Q'* C  if SIDE = 'L' and TRANS = 'T', or
*
*        C * Q  if SIDE = 'R' and TRANS = 'N', or
*
*        C * Q' if SIDE = 'R' and TRANS = 'T',
*
*  where Q is a real orthogonal matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(k) . . . H(2) H(1)
*
*  as returned by DGELQF. Q is of order m if SIDE = 'L' and of order n
*  if SIDE = 'R'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q or Q' from the Left
*          = 'R': apply Q or Q' from the Right
*
*  TRANS   (input) CHARACTER*1
*          = 'N': apply Q  (No transpose)
*          = 'T': apply Q' (Transpose)
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines
*          the matrix Q.
*          If SIDE = 'L', M >= K >= 0;
*          if SIDE = 'R', N >= K >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension
*                               (LDA,M) if SIDE = 'L',
*                               (LDA,N) if SIDE = 'R'
*          The i-th row must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          DGELQF in the first k rows of its array argument A.
*          A is modified by the routine but restored on exit.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,K).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGELQF.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*                                   (N) if SIDE = 'L',
*                                   (M) if SIDE = 'R'
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
      DOUBLE PRECISION   AII
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q
*
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, K ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORML2', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     $   RETURN
*
      IF( ( LEFT .AND. NOTRAN ) .OR. ( .NOT.LEFT .AND. .NOT.NOTRAN ) )
     $     THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      END IF
*
      IF( LEFT ) THEN
         NI = N
         JC = 1
      ELSE
         MI = M
         IC = 1
      END IF
*
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
*
*           H(i) is applied to C(i:m,1:n)
*
            MI = M - I + 1
            IC = I
         ELSE
*
*           H(i) is applied to C(1:m,i:n)
*
            NI = N - I + 1
            JC = I
         END IF
*
*        Apply H(i)
*
         AII = A( I, I )
         A( I, I ) = ONE
         CALL DLARF( SIDE, MI, NI, A( I, I ), LDA, TAU( I ),
     $               C( IC, JC ), LDC, WORK )
         A( I, I ) = AII
   10 CONTINUE
      RETURN
*
*     End of DORML2
*
      END
      SUBROUTINE DORMLQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ),
     $                   WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DORMLQ overwrites the general real M-by-N matrix C with
*
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      Q * C          C * Q
*  TRANS = 'T':      Q**T * C       C * Q**T
*
*  where Q is a real orthogonal matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(k) . . . H(2) H(1)
*
*  as returned by DGELQF. Q is of order M if SIDE = 'L' and of order N
*  if SIDE = 'R'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q or Q**T from the Left;
*          = 'R': apply Q or Q**T from the Right.
*
*  TRANS   (input) CHARACTER*1
*          = 'N':  No transpose, apply Q;
*          = 'T':  Transpose, apply Q**T.
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines
*          the matrix Q.
*          If SIDE = 'L', M >= K >= 0;
*          if SIDE = 'R', N >= K >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension
*                               (LDA,M) if SIDE = 'L',
*                               (LDA,N) if SIDE = 'R'
*          The i-th row must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          DGELQF in the first k rows of its array argument A.
*          A is modified by the routine but restored on exit.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,K).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGELQF.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the M-by-N matrix C.
*          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If SIDE = 'L', LWORK >= max(1,N);
*          if SIDE = 'R', LWORK >= max(1,M).
*          For optimum performance LWORK >= N*NB if SIDE = 'L', and
*          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
*          blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NBMAX, LDT
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      CHARACTER          TRANST
      INTEGER            I, I1, I2, I3, IB, IC, IINFO, IWS, JC, LDWORK,
     $                   MI, NB, NBMIN, NI, NQ, NW
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   T( LDT, NBMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARFB, DLARFT, DORML2, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q and NW is the minimum dimension of WORK
*
      IF( LEFT ) THEN
         NQ = M
         NW = N
      ELSE
         NQ = N
         NW = M
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, K ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.MAX( 1, NW ) ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORMLQ', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Determine the block size.  NB may be at most NBMAX, where NBMAX
*     is used to define the local array T.
*
      NB = MIN (NBMAX, ILAENV (1, 'DORMLQ', SIDE // TRANS, M, N, -1))
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IWS = NW*NB
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX (2,
     *         ILAENV (2, 'DORMLQ', SIDE // TRANS, M, N, -1))
         END IF
      ELSE
         IWS = NW
      END IF
*
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
*
*        Use unblocked code
*
         CALL DORML2( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK,
     $                IINFO )
      ELSE
*
*        Use blocked code
*
         IF( ( LEFT .AND. NOTRAN ) .OR.
     $       ( .NOT.LEFT .AND. .NOT.NOTRAN ) ) THEN
            I1 = 1
            I2 = K
            I3 = NB
         ELSE
            I1 = ( ( K-1 ) / NB )*NB + 1
            I2 = 1
            I3 = -NB
         END IF
*
         IF( LEFT ) THEN
            NI = N
            JC = 1
         ELSE
            MI = M
            IC = 1
         END IF
*
         IF( NOTRAN ) THEN
            TRANST = 'T'
         ELSE
            TRANST = 'N'
         END IF
*
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
*
*           Form the triangular factor of the block reflector
*           H = H(i) H(i+1) . . . H(i+ib-1)
*
            CALL DLARFT( 'Forward', 'Rowwise', NQ-I+1, IB, A( I, I ),
     $                   LDA, TAU( I ), T, LDT )
            IF( LEFT ) THEN
*
*              H or H' is applied to C(i:m,1:n)
*
               MI = M - I + 1
               IC = I
            ELSE
*
*              H or H' is applied to C(1:m,i:n)
*
               NI = N - I + 1
               JC = I
            END IF
*
*           Apply H or H'
*
            CALL DLARFB( SIDE, TRANST, 'Forward', 'Rowwise', MI, NI, IB,
     $                   A( I, I ), LDA, T, LDT, C( IC, JC ), LDC, WORK,
     $                   LDWORK )
   10    CONTINUE
      END IF
      WORK( 1 ) = IWS
      RETURN
*
*     End of DORMLQ
*
      END
      SUBROUTINE DORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ),
     $                   WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DORMQR overwrites the general real M-by-N matrix C with
*
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      Q * C          C * Q
*  TRANS = 'T':      Q**T * C       C * Q**T
*
*  where Q is a real orthogonal matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(1) H(2) . . . H(k)
*
*  as returned by DGEQRF. Q is of order M if SIDE = 'L' and of order N
*  if SIDE = 'R'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q or Q**T from the Left;
*          = 'R': apply Q or Q**T from the Right.
*
*  TRANS   (input) CHARACTER*1
*          = 'N':  No transpose, apply Q;
*          = 'T':  Transpose, apply Q**T.
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines
*          the matrix Q.
*          If SIDE = 'L', M >= K >= 0;
*          if SIDE = 'R', N >= K >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,K)
*          The i-th column must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          DGEQRF in the first k columns of its array argument A.
*          A is modified by the routine but restored on exit.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*          If SIDE = 'L', LDA >= max(1,M);
*          if SIDE = 'R', LDA >= max(1,N).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGEQRF.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the M-by-N matrix C.
*          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If SIDE = 'L', LWORK >= max(1,N);
*          if SIDE = 'R', LWORK >= max(1,M).
*          For optimum performance LWORK >= N*NB if SIDE = 'L', and
*          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
*          blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NBMAX, LDT
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IB, IC, IINFO, IWS, JC, LDWORK,
     $                   MI, NB, NBMIN, NI, NQ, NW
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   T( LDT, NBMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARFB, DLARFT, DORM2R, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q and NW is the minimum dimension of WORK
*
      IF( LEFT ) THEN
         NQ = M
         NW = N
      ELSE
         NQ = N
         NW = M
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.MAX( 1, NW ) ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORMQR', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Determine the block size.  NB may be at most NBMAX, where NBMAX
*     is used to define the local array T.
*
      NB = MIN (NBMAX, ILAENV (1, 'DORMQR', SIDE // TRANS, M, N, -1))
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IWS = NW*NB
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX (2,
     *         ILAENV (2, 'DORMQR', SIDE // TRANS, M, N, -1))
         END IF
      ELSE
         IWS = NW
      END IF
*
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
*
*        Use unblocked code
*
         CALL DORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK,
     $                IINFO )
      ELSE
*
*        Use blocked code
*
         IF( ( LEFT .AND. .NOT.NOTRAN ) .OR.
     $       ( .NOT.LEFT .AND. NOTRAN ) ) THEN
            I1 = 1
            I2 = K
            I3 = NB
         ELSE
            I1 = ( ( K-1 ) / NB )*NB + 1
            I2 = 1
            I3 = -NB
         END IF
*
         IF( LEFT ) THEN
            NI = N
            JC = 1
         ELSE
            MI = M
            IC = 1
         END IF
*
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
*
*           Form the triangular factor of the block reflector
*           H = H(i) H(i+1) . . . H(i+ib-1)
*
            CALL DLARFT( 'Forward', 'Columnwise', NQ-I+1, IB, A( I, I ),
     $                   LDA, TAU( I ), T, LDT )
            IF( LEFT ) THEN
*
*              H or H' is applied to C(i:m,1:n)
*
               MI = M - I + 1
               IC = I
            ELSE
*
*              H or H' is applied to C(1:m,i:n)
*
               NI = N - I + 1
               JC = I
            END IF
*
*           Apply H or H'
*
            CALL DLARFB( SIDE, TRANS, 'Forward', 'Columnwise', MI, NI,
     $                   IB, A( I, I ), LDA, T, LDT, C( IC, JC ), LDC,
     $                   WORK, LDWORK )
   10    CONTINUE
      END IF
      WORK( 1 ) = IWS
      RETURN
*
*     End of DORMQR
*
      END
      DOUBLE PRECISION FUNCTION DLAMCH( CMACH )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          CMACH
*     ..
*
*  Purpose
*  =======
*
*  DLAMCH determines double precision machine parameters.
*
*  Arguments
*  =========
*
*  CMACH   (input) CHARACTER*1
*          Specifies the value to be returned by DLAMCH:
*          = 'E' or 'e',   DLAMCH := eps
*          = 'S' or 's ,   DLAMCH := sfmin
*          = 'B' or 'b',   DLAMCH := base
*          = 'P' or 'p',   DLAMCH := eps*base
*          = 'N' or 'n',   DLAMCH := t
*          = 'R' or 'r',   DLAMCH := rnd
*          = 'M' or 'm',   DLAMCH := emin
*          = 'U' or 'u',   DLAMCH := rmin
*          = 'L' or 'l',   DLAMCH := emax
*          = 'O' or 'o',   DLAMCH := rmax
*
*          where
*
*          eps   = relative machine precision
*          sfmin = safe minimum, such that 1/sfmin does not overflow
*          base  = base of the machine
*          prec  = eps*base
*          t     = number of (base) digits in the mantissa
*          rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
*          emin  = minimum exponent before (gradual) underflow
*          rmin  = underflow threshold - base**(emin-1)
*          emax  = largest exponent before overflow
*          rmax  = overflow threshold  - (base**emax)*(1-eps)
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FIRST, LRND
      INTEGER            BETA, IMAX, IMIN, IT
      DOUBLE PRECISION   BASE, EMAX, EMIN, EPS, PREC, RMACH, RMAX, RMIN,
     $                   RND, SFMIN, SMALL, T
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAMC2
*     ..
*     .. Save statement ..
      SAVE               FIRST, EPS, SFMIN, BASE, T, RND, EMIN, RMIN,
     $                   EMAX, RMAX, PREC
*     ..
*     .. Data statements ..
      DATA               FIRST / .TRUE. /
*     ..
*     .. Executable Statements ..
*
      IF( FIRST ) THEN
         FIRST = .FALSE.
         CALL DLAMC2( BETA, IT, LRND, EPS, IMIN, RMIN, IMAX, RMAX )
         BASE = BETA
         T = IT
         IF( LRND ) THEN
            RND = ONE
            EPS = ( BASE**( 1-IT ) ) / 2
         ELSE
            RND = ZERO
            EPS = BASE**( 1-IT )
         END IF
         PREC = EPS*BASE
         EMIN = IMIN
         EMAX = IMAX
         SFMIN = RMIN
         SMALL = ONE / RMAX
         IF( SMALL.GE.SFMIN ) THEN
*
*           Use SMALL plus a bit, to avoid the possibility of rounding
*           causing overflow when computing  1/sfmin.
*
            SFMIN = SMALL*( ONE+EPS )
         END IF
      END IF
*
      IF( LSAME( CMACH, 'E' ) ) THEN
         RMACH = EPS
      ELSE IF( LSAME( CMACH, 'S' ) ) THEN
         RMACH = SFMIN
      ELSE IF( LSAME( CMACH, 'B' ) ) THEN
         RMACH = BASE
      ELSE IF( LSAME( CMACH, 'P' ) ) THEN
         RMACH = PREC
      ELSE IF( LSAME( CMACH, 'N' ) ) THEN
         RMACH = T
      ELSE IF( LSAME( CMACH, 'R' ) ) THEN
         RMACH = RND
      ELSE IF( LSAME( CMACH, 'M' ) ) THEN
         RMACH = EMIN
      ELSE IF( LSAME( CMACH, 'U' ) ) THEN
         RMACH = RMIN
      ELSE IF( LSAME( CMACH, 'L' ) ) THEN
         RMACH = EMAX
      ELSE IF( LSAME( CMACH, 'O' ) ) THEN
         RMACH = RMAX
      END IF
*
      DLAMCH = RMACH
      RETURN
*
*     End of DLAMCH
*
      END
*
************************************************************************
*
      SUBROUTINE DLAMC1( BETA, T, RND, IEEE1 )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      LOGICAL            IEEE1, RND
      INTEGER            BETA, T
*     ..
*
*  Purpose
*  =======
*
*  DLAMC1 determines the machine parameters given by BETA, T, RND, and
*  IEEE1.
*
*  Arguments
*  =========
*
*  BETA    (output) INTEGER
*          The base of the machine.
*
*  T       (output) INTEGER
*          The number of ( BETA ) digits in the mantissa.
*
*  RND     (output) LOGICAL
*          Specifies whether proper rounding  ( RND = .TRUE. )  or
*          chopping  ( RND = .FALSE. )  occurs in addition. This may not
*          be a reliable guide to the way in which the machine performs
*          its arithmetic.
*
*  IEEE1   (output) LOGICAL
*          Specifies whether rounding appears to be done in the IEEE
*          'round to nearest' style.
*
*  Further Details
*  ===============
*
*  The routine is based on the routine  ENVRON  by Malcolm and
*  incorporates suggestions by Gentleman and Marovich. See
*
*     Malcolm M. A. (1972) Algorithms to reveal properties of
*        floating-point arithmetic. Comms. of the ACM, 15, 949-951.
*
*     Gentleman W. M. and Marovich S. B. (1974) More on algorithms
*        that reveal properties of floating point arithmetic units.
*        Comms. of the ACM, 17, 276-277.
*
* =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            FIRST, LIEEE1, LRND
      INTEGER            LBETA, LT
      DOUBLE PRECISION   A, B, C, F, ONE, QTR, SAVEC, T1, T2
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMC3
      EXTERNAL           DLAMC3
*     ..
*     .. Save statement ..
      SAVE               FIRST, LIEEE1, LBETA, LRND, LT
*     ..
*     .. Data statements ..
      DATA               FIRST / .TRUE. /
*     ..
*     .. Executable Statements ..
*
      IF( FIRST ) THEN
         FIRST = .FALSE.
         ONE = 1
*
*        LBETA,  LIEEE1,  LT and  LRND  are the  local values  of  BETA,
*        IEEE1, T and RND.
*
*        Throughout this routine  we use the function  DLAMC3  to ensure
*        that relevant values are  stored and not held in registers,  or
*        are not affected by optimizers.
*
*        Compute  a = 2.0**m  with the  smallest positive integer m such
*        that
*
*           fl( a + 1.0 ) = a.
*
         A = 1
         C = 1
*
*+       WHILE( C.EQ.ONE )LOOP
   10    CONTINUE
         IF( C.EQ.ONE ) THEN
            A = 2*A
            C = DLAMC3( A, ONE )
            C = DLAMC3( C, -A )
            GO TO 10
         END IF
*+       END WHILE
*
*        Now compute  b = 2.0**m  with the smallest positive integer m
*        such that
*
*           fl( a + b ) .gt. a.
*
         B = 1
         C = DLAMC3( A, B )
*
*+       WHILE( C.EQ.A )LOOP
   20    CONTINUE
         IF( C.EQ.A ) THEN
            B = 2*B
            C = DLAMC3( A, B )
            GO TO 20
         END IF
*+       END WHILE
*
*        Now compute the base.  a and c  are neighbouring floating point
*        numbers  in the  interval  ( beta**t, beta**( t + 1 ) )  and so
*        their difference is beta. Adding 0.25 to c is to ensure that it
*        is truncated to beta and not ( beta - 1 ).
*
         QTR = ONE / 4
         SAVEC = C
         C = DLAMC3( C, -A )
         LBETA = C + QTR
*
*        Now determine whether rounding or chopping occurs,  by adding a
*        bit  less  than  beta/2  and a  bit  more  than  beta/2  to  a.
*
         B = LBETA
         F = DLAMC3( B / 2, -B / 100 )
         C = DLAMC3( F, A )
         IF( C.EQ.A ) THEN
            LRND = .TRUE.
         ELSE
            LRND = .FALSE.
         END IF
         F = DLAMC3( B / 2, B / 100 )
         C = DLAMC3( F, A )
         IF( ( LRND ) .AND. ( C.EQ.A ) )
     $      LRND = .FALSE.
*
*        Try and decide whether rounding is done in the  IEEE  'round to
*        nearest' style. B/2 is half a unit in the last place of the two
*        numbers A and SAVEC. Furthermore, A is even, i.e. has last  bit
*        zero, and SAVEC is odd. Thus adding B/2 to A should not  change
*        A, but adding B/2 to SAVEC should change SAVEC.
*
         T1 = DLAMC3( B / 2, A )
         T2 = DLAMC3( B / 2, SAVEC )
         LIEEE1 = ( T1.EQ.A ) .AND. ( T2.GT.SAVEC ) .AND. LRND
*
*        Now find  the  mantissa, t.  It should  be the  integer part of
*        log to the base beta of a,  however it is safer to determine  t
*        by powering.  So we find t as the smallest positive integer for
*        which
*
*           fl( beta**t + 1.0 ) = 1.0.
*
         LT = 0
         A = 1
         C = 1
*
*+       WHILE( C.EQ.ONE )LOOP
   30    CONTINUE
         IF( C.EQ.ONE ) THEN
            LT = LT + 1
            A = A*LBETA
            C = DLAMC3( A, ONE )
            C = DLAMC3( C, -A )
            GO TO 30
         END IF
*+       END WHILE
*
      END IF
*
      BETA = LBETA
      T = LT
      RND = LRND
      IEEE1 = LIEEE1
      RETURN
*
*     End of DLAMC1
*
      END
*
************************************************************************
*
      SUBROUTINE DLAMC2( BETA, T, RND, EPS, EMIN, RMIN, EMAX, RMAX )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      LOGICAL            RND
      INTEGER            BETA, EMAX, EMIN, T
      DOUBLE PRECISION   EPS, RMAX, RMIN
*     ..
*
*  Purpose
*  =======
*
*  DLAMC2 determines the machine parameters specified in its argument
*  list.
*
*  Arguments
*  =========
*
*  BETA    (output) INTEGER
*          The base of the machine.
*
*  T       (output) INTEGER
*          The number of ( BETA ) digits in the mantissa.
*
*  RND     (output) LOGICAL
*          Specifies whether proper rounding  ( RND = .TRUE. )  or
*          chopping  ( RND = .FALSE. )  occurs in addition. This may not
*          be a reliable guide to the way in which the machine performs
*          its arithmetic.
*
*  EPS     (output) DOUBLE PRECISION
*          The smallest positive number such that
*
*             fl( 1.0 - EPS ) .LT. 1.0,
*
*          where fl denotes the computed value.
*
*  EMIN    (output) INTEGER
*          The minimum exponent before (gradual) underflow occurs.
*
*  RMIN    (output) DOUBLE PRECISION
*          The smallest normalized number for the machine, given by
*          BASE**( EMIN - 1 ), where  BASE  is the floating point value
*          of BETA.
*
*  EMAX    (output) INTEGER
*          The maximum exponent before overflow occurs.
*
*  RMAX    (output) DOUBLE PRECISION
*          The largest positive number for the machine, given by
*          BASE**EMAX * ( 1 - EPS ), where  BASE  is the floating point
*          value of BETA.
*
*  Further Details
*  ===============
*
*  The computation of  EPS  is based on a routine PARANOIA by
*  W. Kahan of the University of California at Berkeley.
*
* =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            FIRST, IEEE, IWARN, LIEEE1, LRND
      INTEGER            GNMIN, GPMIN, I, LBETA, LEMAX, LEMIN, LT,
     $                   NGNMIN, NGPMIN
      DOUBLE PRECISION   A, B, C, HALF, LEPS, LRMAX, LRMIN, ONE, RBASE,
     $                   SIXTH, SMALL, THIRD, TWO, ZERO
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMC3
      EXTERNAL           DLAMC3
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAMC1, DLAMC4, DLAMC5
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Save statement ..
      SAVE               FIRST, IWARN, LBETA, LEMAX, LEMIN, LEPS, LRMAX,
     $                   LRMIN, LT
*     ..
*     .. Data statements ..
      DATA               FIRST / .TRUE. / , IWARN / .FALSE. /
*     ..
*     .. Executable Statements ..
*
      IF( FIRST ) THEN
         FIRST = .FALSE.
         ZERO = 0
         ONE = 1
         TWO = 2
*
*        LBETA, LT, LRND, LEPS, LEMIN and LRMIN  are the local values of
*        BETA, T, RND, EPS, EMIN and RMIN.
*
*        Throughout this routine  we use the function  DLAMC3  to ensure
*        that relevant values are stored  and not held in registers,  or
*        are not affected by optimizers.
*
*        DLAMC1 returns the parameters  LBETA, LT, LRND and LIEEE1.
*
         CALL DLAMC1( LBETA, LT, LRND, LIEEE1 )
*
*        Start to find EPS.
*
         B = LBETA
         A = B**( -LT )
         LEPS = A
*
*        Try some tricks to see whether or not this is the correct  EPS.
*
         B = TWO / 3
         HALF = ONE / 2
         SIXTH = DLAMC3( B, -HALF )
         THIRD = DLAMC3( SIXTH, SIXTH )
         B = DLAMC3( THIRD, -HALF )
         B = DLAMC3( B, SIXTH )
         B = ABS( B )
         IF( B.LT.LEPS )
     $      B = LEPS
*
         LEPS = 1
*
*+       WHILE( ( LEPS.GT.B ).AND.( B.GT.ZERO ) )LOOP
   10    CONTINUE
         IF( ( LEPS.GT.B ) .AND. ( B.GT.ZERO ) ) THEN
            LEPS = B
            C = DLAMC3( HALF*LEPS, ( TWO**5 )*( LEPS**2 ) )
            C = DLAMC3( HALF, -C )
            B = DLAMC3( HALF, C )
            C = DLAMC3( HALF, -B )
            B = DLAMC3( HALF, C )
            GO TO 10
         END IF
*+       END WHILE
*
         IF( A.LT.LEPS )
     $      LEPS = A
*
*        Computation of EPS complete.
*
*        Now find  EMIN.  Let A = + or - 1, and + or - (1 + BASE**(-3)).
*        Keep dividing  A by BETA until (gradual) underflow occurs. This
*        is detected when we cannot recover the previous A.
*
         RBASE = ONE / LBETA
         SMALL = ONE
         DO 20 I = 1, 3
            SMALL = DLAMC3( SMALL*RBASE, ZERO )
   20    CONTINUE
         A = DLAMC3( ONE, SMALL )
         CALL DLAMC4( NGPMIN, ONE, LBETA )
         CALL DLAMC4( NGNMIN, -ONE, LBETA )
         CALL DLAMC4( GPMIN, A, LBETA )
         CALL DLAMC4( GNMIN, -A, LBETA )
         IEEE = .FALSE.
*
         IF( ( NGPMIN.EQ.NGNMIN ) .AND. ( GPMIN.EQ.GNMIN ) ) THEN
            IF( NGPMIN.EQ.GPMIN ) THEN
               LEMIN = NGPMIN
*            ( Non twos-complement machines, no gradual underflow;
*              e.g.,  VAX )
            ELSE IF( ( GPMIN-NGPMIN ).EQ.3 ) THEN
               LEMIN = NGPMIN - 1 + LT
               IEEE = .TRUE.
*            ( Non twos-complement machines, with gradual underflow;
*              e.g., IEEE standard followers )
            ELSE
               LEMIN = MIN( NGPMIN, GPMIN )
*            ( A guess; no known machine )
               IWARN = .TRUE.
            END IF
*
         ELSE IF( ( NGPMIN.EQ.GPMIN ) .AND. ( NGNMIN.EQ.GNMIN ) ) THEN
            IF( ABS( NGPMIN-NGNMIN ).EQ.1 ) THEN
               LEMIN = MAX( NGPMIN, NGNMIN )
*            ( Twos-complement machines, no gradual underflow;
*              e.g., CYBER 205 )
            ELSE
               LEMIN = MIN( NGPMIN, NGNMIN )
*            ( A guess; no known machine )
               IWARN = .TRUE.
            END IF
*
         ELSE IF( ( ABS( NGPMIN-NGNMIN ).EQ.1 ) .AND.
     $            ( GPMIN.EQ.GNMIN ) ) THEN
            IF( ( GPMIN-MIN( NGPMIN, NGNMIN ) ).EQ.3 ) THEN
               LEMIN = MAX( NGPMIN, NGNMIN ) - 1 + LT
*            ( Twos-complement machines with gradual underflow;
*              no known machine )
            ELSE
               LEMIN = MIN( NGPMIN, NGNMIN )
*            ( A guess; no known machine )
               IWARN = .TRUE.
            END IF
*
         ELSE
            LEMIN = MIN( NGPMIN, NGNMIN, GPMIN, GNMIN )
*         ( A guess; no known machine )
            IWARN = .TRUE.
         END IF
***
* Comment out this if block if EMIN is ok
         IF( IWARN ) THEN
            FIRST = .TRUE.
            WRITE( 6, FMT = 9999 )LEMIN
         END IF
***
*
*        Assume IEEE arithmetic if we found denormalised  numbers above,
*        or if arithmetic seems to round in the  IEEE style,  determined
*        in routine DLAMC1. A true IEEE machine should have both  things
*        true; however, faulty machines may have one or the other.
*
         IEEE = IEEE .OR. LIEEE1
*
*        Compute  RMIN by successive division by  BETA. We could compute
*        RMIN as BASE**( EMIN - 1 ),  but some machines underflow during
*        this computation.
*
         LRMIN = 1
         DO 30 I = 1, 1 - LEMIN
            LRMIN = DLAMC3( LRMIN*RBASE, ZERO )
   30    CONTINUE
*
*        Finally, call DLAMC5 to compute EMAX and RMAX.
*
         CALL DLAMC5( LBETA, LT, LEMIN, IEEE, LEMAX, LRMAX )
      END IF
*
      BETA = LBETA
      T = LT
      RND = LRND
      EPS = LEPS
      EMIN = LEMIN
      RMIN = LRMIN
      EMAX = LEMAX
      RMAX = LRMAX
*
      RETURN
*
 9999 FORMAT( / / ' WARNING. The value EMIN may be incorrect:-',
     $      '  EMIN = ', I8, /
     $      ' If, after inspection, the value EMIN looks',
     $      ' acceptable please comment out ',
     $      / ' the IF block as marked within the code of routine',
     $      ' DLAMC2,', / ' otherwise supply EMIN explicitly.', / )
*
*     End of DLAMC2
*
      END
*
************************************************************************
*
      DOUBLE PRECISION FUNCTION DLAMC3( A, B )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B
*     ..
*
*  Purpose
*  =======
*
*  DLAMC3  is intended to force  A  and  B  to be stored prior to doing
*  the addition of  A  and  B ,  for use in situations where optimizers
*  might hold one of these in a register.
*
*  Arguments
*  =========
*
*  A, B    (input) DOUBLE PRECISION
*          The values A and B.
*
* =====================================================================
*
*     .. Executable Statements ..
*
      DLAMC3 = A + B
*
      RETURN
*
*     End of DLAMC3
*
      END
*
************************************************************************
*
      SUBROUTINE DLAMC4( EMIN, START, BASE )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      INTEGER            BASE, EMIN
      DOUBLE PRECISION   START
*     ..
*
*  Purpose
*  =======
*
*  DLAMC4 is a service routine for DLAMC2.
*
*  Arguments
*  =========
*
*  EMIN    (output) EMIN
*          The minimum exponent before (gradual) underflow, computed by
*          setting A = START and dividing by BASE until the previous A
*          can not be recovered.
*
*  START   (input) DOUBLE PRECISION
*          The starting point for determining EMIN.
*
*  BASE    (input) INTEGER
*          The base of the machine.
*
* =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   A, B1, B2, C1, C2, D1, D2, ONE, RBASE, ZERO
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMC3
      EXTERNAL           DLAMC3
*     ..
*     .. Executable Statements ..
*
      A = START
      ONE = 1
      RBASE = ONE / BASE
      ZERO = 0
      EMIN = 1
      B1 = DLAMC3( A*RBASE, ZERO )
      C1 = A
      C2 = A
      D1 = A
      D2 = A
*+    WHILE( ( C1.EQ.A ).AND.( C2.EQ.A ).AND.
*    $       ( D1.EQ.A ).AND.( D2.EQ.A )      )LOOP
   10 CONTINUE
      IF( ( C1.EQ.A ) .AND. ( C2.EQ.A ) .AND. ( D1.EQ.A ) .AND.
     $    ( D2.EQ.A ) ) THEN
         EMIN = EMIN - 1
         A = B1
         B1 = DLAMC3( A / BASE, ZERO )
         C1 = DLAMC3( B1*BASE, ZERO )
         D1 = ZERO
         DO 20 I = 1, BASE
            D1 = D1 + B1
   20    CONTINUE
         B2 = DLAMC3( A*RBASE, ZERO )
         C2 = DLAMC3( B2 / RBASE, ZERO )
         D2 = ZERO
         DO 30 I = 1, BASE
            D2 = D2 + B2
   30    CONTINUE
         GO TO 10
      END IF
*+    END WHILE
*
      RETURN
*
*     End of DLAMC4
*
      END
*
************************************************************************
*
      SUBROUTINE DLAMC5( BETA, P, EMIN, IEEE, EMAX, RMAX )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      LOGICAL            IEEE
      INTEGER            BETA, EMAX, EMIN, P
      DOUBLE PRECISION   RMAX
*     ..
*
*  Purpose
*  =======
*
*  DLAMC5 attempts to compute RMAX, the largest machine floating-point
*  number, without overflow.  It assumes that EMAX + abs(EMIN) sum
*  approximately to a power of 2.  It will fail on machines where this
*  assumption does not hold, for example, the Cyber 205 (EMIN = -28625,
*  EMAX = 28718).  It will also fail if the value supplied for EMIN is
*  too large (i.e. too close to zero), probably with overflow.
*
*  Arguments
*  =========
*
*  BETA    (input) INTEGER
*          The base of floating-point arithmetic.
*
*  P       (input) INTEGER
*          The number of base BETA digits in the mantissa of a
*          floating-point value.
*
*  EMIN    (input) INTEGER
*          The minimum exponent before (gradual) underflow.
*
*  IEEE    (input) LOGICAL
*          A logical flag specifying whether or not the arithmetic
*          system is thought to comply with the IEEE standard.
*
*  EMAX    (output) INTEGER
*          The largest exponent before overflow
*
*  RMAX    (output) DOUBLE PRECISION
*          The largest machine floating-point number.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            EXBITS, EXPSUM, I, LEXP, NBITS, TRY, UEXP
      DOUBLE PRECISION   OLDY, RECBAS, Y, Z
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMC3
      EXTERNAL           DLAMC3
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MOD
*     ..
*     .. Executable Statements ..
*
*     First compute LEXP and UEXP, two powers of 2 that bound
*     abs(EMIN). We then assume that EMAX + abs(EMIN) will sum
*     approximately to the bound that is closest to abs(EMIN).
*     (EMAX is the exponent of the required number RMAX).
*
      LEXP = 1
      EXBITS = 1
   10 CONTINUE
      TRY = LEXP*2
      IF( TRY.LE.( -EMIN ) ) THEN
         LEXP = TRY
         EXBITS = EXBITS + 1
         GO TO 10
      END IF
      IF( LEXP.EQ.-EMIN ) THEN
         UEXP = LEXP
      ELSE
         UEXP = TRY
         EXBITS = EXBITS + 1
      END IF
*
*     Now -LEXP is less than or equal to EMIN, and -UEXP is greater
*     than or equal to EMIN. EXBITS is the number of bits needed to
*     store the exponent.
*
      IF( ( UEXP+EMIN ).GT.( -LEXP-EMIN ) ) THEN
         EXPSUM = 2*LEXP
      ELSE
         EXPSUM = 2*UEXP
      END IF
*
*     EXPSUM is the exponent range, approximately equal to
*     EMAX - EMIN + 1 .
*
      EMAX = EXPSUM + EMIN - 1
      NBITS = 1 + EXBITS + P
*
*     NBITS is the total number of bits needed to store a
*     floating-point number.
*
      IF( ( MOD( NBITS, 2 ).EQ.1 ) .AND. ( BETA.EQ.2 ) ) THEN
*
*        Either there are an odd number of bits used to store a
*        floating-point number, which is unlikely, or some bits are
*        not used in the representation of numbers, which is possible,
*        (e.g. Cray machines) or the mantissa has an implicit bit,
*        (e.g. IEEE machines, Dec Vax machines), which is perhaps the
*        most likely. We have to assume the last alternative.
*        If this is true, then we need to reduce EMAX by one because
*        there must be some way of representing zero in an implicit-bit
*        system. On machines like Cray, we are reducing EMAX by one
*        unnecessarily.
*
         EMAX = EMAX - 1
      END IF
*
      IF( IEEE ) THEN
*
*        Assume we are on an IEEE machine which reserves one exponent
*        for infinity and NaN.
*
         EMAX = EMAX - 1
      END IF
*
*     Now create RMAX, the largest machine number, which should
*     be equal to (1.0 - BETA**(-P)) * BETA**EMAX .
*
*     First compute 1.0 - BETA**(-P), being careful that the
*     result is less than 1.0 .
*
      RECBAS = ONE / BETA
      Z = BETA - ONE
      Y = ZERO
      DO 20 I = 1, P
         Z = Z*RECBAS
         IF( Y.LT.ONE )
     $      OLDY = Y
         Y = DLAMC3( Y, Z )
   20 CONTINUE
      IF( Y.GE.ONE )
     $   Y = OLDY
*
*     Now multiply by BETA**EMAX to get RMAX.
*
      DO 30 I = 1, EMAX
         Y = DLAMC3( Y*BETA, ZERO )
   30 CONTINUE
*
      RMAX = Y
      RETURN
*
*     End of DLAMC5
*
      END
      INTEGER FUNCTION ILAENV (ISPEC, NAME, OPTS, N1, N2, N4 )
C-----------------------------------------------------------------------
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    NAME, OPTS
      INTEGER            ISPEC, N1, N2, N4
*     ..
*
*  Purpose
*  =======
*
*  ILAENV is called from the LAPACK routines to choose problem-dependent
*  parameters for the local environment.  See ISPEC for a description of
*  the parameters.
*
*  This version provides a set of parameters which should give good,
*  but not optimal, performance on many of the currently available
*  computers.  Users are encouraged to modify this subroutine to set
*  the tuning parameters for their particular machine using the option
*  and problem size information in the arguments.
*
*  This routine will not function correctly if it is converted to all
*  lower case.  Converting it to all upper case is allowed.
*
*  Arguments
*  =========
*
*  ISPEC   (input) INTEGER
*          Specifies the parameter to be returned as the value of
*          ILAENV.
*          = 1: the optimal blocksize; if this value is 1, an unblocked
*               algorithm will give the best performance.
*          = 2: the minimum block size for which the block routine
*               should be used; if the usable block size is less than
*               this value, an unblocked routine should be used.
*          = 3: the crossover point (in a block routine, for N less
*               than this value, an unblocked routine should be used)
*          = 4: the number of shifts, used in the nonsymmetric
*               eigenvalue routines
*          = 5: the minimum column dimension for blocking to be used;
*               rectangular blocks must have dimension at least k by m,
*               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
*          = 6: the crossover point for the SVD (when reducing an m by n
*               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
*               this value, a QR factorization is used first to reduce
*               the matrix to a triangular form.)
*          = 7: the number of processors
*          = 8: the crossover point for the multishift QR and QZ methods
*               for nonsymmetric eigenvalue problems.
*
*  NAME    (input) CHARACTER*(*)
*          The name of the calling subroutine, in either upper case or
*          lower case.
*
*  OPTS    (input) CHARACTER*(*)
*          The character options to the subroutine NAME, concatenated
*          into a single character string.  For example, UPLO = 'U',
*          TRANS = 'T', and DIAG = 'N' for a triangular routine would
*          be specified as OPTS = 'UTN'.
*
*  N1      (input) INTEGER
*  N2      (input) INTEGER
*  N3      (input) INTEGER
*  N4      (input) INTEGER
*          Problem dimensions for the subroutine NAME; these may not all
*          be required.
*
* (ILAENV) (output) INTEGER
*          >= 0: the value of the parameter specified by ISPEC
*          < 0:  if ILAENV = -k, the k-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The following conventions have been used when calling ILAENV from the
*  LAPACK routines:
*  1)  OPTS is a concatenation of all of the character options to
*      subroutine NAME, in the same order that they appear in the
*      argument list for NAME, even if they are not used in determining
*      the value of the parameter specified by ISPEC.
*  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
*      that they appear in the argument list for NAME.  N1 is used
*      first, N2 second, and so on, and unused problem dimensions are
*      passed a value of -1.
*  3)  The parameter value returned by ILAENV is checked for validity in
*      the calling subroutine.  For example, ILAENV is used to retrieve
*      the optimal blocksize for STRTRI as follows:
*
*      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
*      IF( NB.LE.1 ) NB = MAX( 1, N )
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            CNAME, SNAME
      CHARACTER*1        C1
      CHARACTER*2        C2, C4
      CHARACTER*3        C3
      CHARACTER*6        SUBNAM
      INTEGER            I, IC, IZ, NB, NBMIN, NX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CHAR, ICHAR, INT, MIN, REAL
*     ..
*     .. Executable Statements ..
*
      GO TO ( 100, 100, 100, 400, 500, 600, 700, 800 ) ISPEC
*
*     Invalid value for ISPEC
*
      ILAENV = -1
      RETURN
*
  100 CONTINUE
*
*     Convert NAME to upper case if the first character is lower case.
*
      ILAENV = 1
      SUBNAM = NAME
      IC = ICHAR( SUBNAM( 1:1 ) )
      IZ = ICHAR( 'Z' )
      IF( IZ.EQ.90 .OR. IZ.EQ.122 ) THEN
*
*        ASCII character set
*
         IF( IC.GE.97 .AND. IC.LE.122 ) THEN
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 10 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( IC.GE.97 .AND. IC.LE.122 )
     $            SUBNAM( I:I ) = CHAR( IC-32 )
   10       CONTINUE
         END IF
*
      ELSE IF( IZ.EQ.233 .OR. IZ.EQ.169 ) THEN
*
*        EBCDIC character set
*
         IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $       ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $       ( IC.GE.162 .AND. IC.LE.169 ) ) THEN
            SUBNAM( 1:1 ) = CHAR( IC+64 )
            DO 20 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $             ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $             ( IC.GE.162 .AND. IC.LE.169 ) )
     $            SUBNAM( I:I ) = CHAR( IC+64 )
   20       CONTINUE
         END IF
*
      ELSE IF( IZ.EQ.218 .OR. IZ.EQ.250 ) THEN
*
*        Prime machines:  ASCII+128
*
         IF( IC.GE.225 .AND. IC.LE.250 ) THEN
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 30 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( IC.GE.225 .AND. IC.LE.250 )
     $            SUBNAM( I:I ) = CHAR( IC-32 )
   30       CONTINUE
         END IF
      END IF
*
      C1 = SUBNAM( 1:1 )
      SNAME = C1.EQ.'S' .OR. C1.EQ.'D'
      CNAME = C1.EQ.'C' .OR. C1.EQ.'Z'
      IF( .NOT.( CNAME .OR. SNAME ) )
     $   RETURN
      C2 = SUBNAM( 2:3 )
      C3 = SUBNAM( 4:6 )
      C4 = C3( 2:3 )
*
      GO TO ( 110, 200, 300 ) ISPEC
*
  110 CONTINUE
*
*     ISPEC = 1:  block size
*
*     In these examples, separate code is provided for setting NB for
*     real and complex.  We assume that NB will take the same value in
*     single or double precision.
*
      NB = 1
*
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $            C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'PO' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NB = 1
         ELSE IF( SNAME .AND. C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            NB = 64
         ELSE IF( C3.EQ.'TRD' ) THEN
            NB = 1
         ELSE IF( C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         END IF
      ELSE IF( C2.EQ.'GB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'PB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'TR' ) THEN
         IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'LA' ) THEN
         IF( C3.EQ.'UUM' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'ST' ) THEN
         IF( C3.EQ.'EBZ' ) THEN
            NB = 1
         END IF
      END IF
      ILAENV = NB
      RETURN
*
  200 CONTINUE
*
*     ISPEC = 2:  minimum block size
*
      NBMIN = 2
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $       C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 8
            ELSE
               NBMIN = 8
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         END IF
      END IF
      ILAENV = NBMIN
      RETURN
*
  300 CONTINUE
*
*     ISPEC = 3:  crossover point
*
      NX = 0
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $       C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NX = 1
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NX = 1
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NX = 128
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NX = 128
            END IF
         END IF
      END IF
      ILAENV = NX
      RETURN
*
  400 CONTINUE
*
*     ISPEC = 4:  number of shifts (used by xHSEQR)
*
      ILAENV = 6
      RETURN
*
  500 CONTINUE
*
*     ISPEC = 5:  minimum column dimension (not used)
*
      ILAENV = 2
      RETURN
*
  600 CONTINUE
*
*     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)
*
      ILAENV = INT( REAL( MIN( N1, N2 ) )*1.6E0 )
      RETURN
*
  700 CONTINUE
*
*     ISPEC = 7:  number of processors (not used)
*
      ILAENV = 1
      RETURN
*
  800 CONTINUE
*
*     ISPEC = 8:  crossover point for multishift (used by xHSEQR)
*
      ILAENV = 50
      RETURN
*
*     End of ILAENV
*
      END
      LOGICAL          FUNCTION LSAME( CA, CB )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          CA, CB
*     ..
*
*  Purpose
*  =======
*
*  LSAME returns .TRUE. if CA is the same letter as CB regardless of
*  case.
*
*  Arguments
*  =========
*
*  CA      (input) CHARACTER*1
*  CB      (input) CHARACTER*1
*          CA and CB specify the single characters to be compared.
*
* =====================================================================
*
*     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
*     ..
*     .. Local Scalars ..
      INTEGER            INTA, INTB, ZCODE
*     ..
*     .. Executable Statements ..
*
*     Test if the characters are equal
*
      LSAME = CA.EQ.CB
      IF( LSAME )
     $   RETURN
*
*     Now test for equivalence if both characters are alphabetic.
*
      ZCODE = ICHAR( 'Z' )
*
*     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
*     machines, on which ICHAR returns a value with bit 8 set.
*     ICHAR('A') on Prime machines returns 193 which is the same as
*     ICHAR('A') on an EBCDIC machine.
*
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
*
      IF( ZCODE.EQ.90 .OR. ZCODE.EQ.122 ) THEN
*
*        ASCII is assumed - ZCODE is the ASCII code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.97 .AND. INTA.LE.122 ) INTA = INTA - 32
         IF( INTB.GE.97 .AND. INTB.LE.122 ) INTB = INTB - 32
*
      ELSE IF( ZCODE.EQ.233 .OR. ZCODE.EQ.169 ) THEN
*
*        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.129 .AND. INTA.LE.137 .OR.
     $       INTA.GE.145 .AND. INTA.LE.153 .OR.
     $       INTA.GE.162 .AND. INTA.LE.169 ) INTA = INTA + 64
         IF( INTB.GE.129 .AND. INTB.LE.137 .OR.
     $       INTB.GE.145 .AND. INTB.LE.153 .OR.
     $       INTB.GE.162 .AND. INTB.LE.169 ) INTB = INTB + 64
*
      ELSE IF( ZCODE.EQ.218 .OR. ZCODE.EQ.250 ) THEN
*
*        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
*        plus 128 of either lower or upper case 'Z'.
*
         IF( INTA.GE.225 .AND. INTA.LE.250 ) INTA = INTA - 32
         IF( INTB.GE.225 .AND. INTB.LE.250 ) INTB = INTB - 32
      END IF
      LSAME = INTA.EQ.INTB
*
*     RETURN
*
*     End of LSAME
*
      END
      SUBROUTINE XERBLA( SRNAME, INFO )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      INTEGER            INFO
*     ..
*
*  Purpose
*  =======
*
*  XERBLA  is an error handler for the LAPACK routines.
*  It is called by an LAPACK routine if an input parameter has an
*  invalid value.  A message is printed and execution stops.
*
*  Installers may consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Arguments
*  =========
*
*  SRNAME  (input) CHARACTER*6
*          The name of the routine which called XERBLA.
*
*  INFO    (input) INTEGER
*          The position of the invalid parameter in the parameter list
*          of the calling routine.
*
* =====================================================================
*
*     .. Executable Statements ..
*
      WRITE( *, FMT = 9999 )SRNAME, INFO
*
      STOP
*
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',
     $      'an illegal value' )
*
*     End of XERBLA
*
      END

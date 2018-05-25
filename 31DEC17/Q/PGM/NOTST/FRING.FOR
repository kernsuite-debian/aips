LOCAL INCLUDE 'FRING.INC'
C                                       Include FRING
C                                       Local include for FRING
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXPRM, XBFSZ
C                                       MAXPRM = maximum no. parms in
C                                       Least squares solutions
      PARAMETER (MAXPRM = MAXANT * 3)
C                                       XBFSZ = buffer size
      PARAMETER (XBFSZ = UVBFSL)
      INTEGER   CATIN(256), SEQIN, SEQ2, SEQOUT, DISKIN, DISK2, DISOUT,
     *   CNOIN, CNOOUT, JBUFSZ, BUFFS(XBFSZ), NANT, CNOIN2, CCTVER,
     *   NFREQ, REFANT, NPOL, NVAL, SNVER, VISDSK, VISCNO, VER, NUMNOD,
     *   NUMBL, NUMTIM, PRTLV, BLDO, MINNO, LOCIF, LOCF, CHINC, LBIF,
     *   LEIF, IBUFF1(XBFSZ)
      LOGICAL   SINGLE, DOMODL, TSMOTH, AVGIF, DOIF, DOMS, AVGPOL,
     *   DODRLS, DAVOUT, GDSOLV(MAXANT+1), ZDEL, ZRAT, ZPHS, NORAT
      INTEGER   NCOMP(MAXFLD), REFUSE(MAXANT), PRIRTY(1+MAXANT),
     *   REFUSS(MAXANT,100), DOEVLA, DODISP
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC, XNAME2(3),
     *   XCLAS2(2), XNAMOU(3), XCLAOU(2), XCMETH, XCMOD
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XCALCO*4, NAME2*12,
     *   CLAS2*6, NAMOUT*12, CLAOUT*6, CMETH*4, CMOD*4
      REAL      XSI, XDI, XQUAL, XBAND, XFREQ, XFQID, XTIME(8), XBCHAN,
     *   XECHAN, XCHINC, XANTS(50), XSUBA, XUVRA(2), XWTUV, XWTIT,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH(3), XS2, XD2, XVER, XSO, XDO, XNCOMP(MAXAFL), XFLUX,
     *   XNMAP, SMODEL(7), DOAPLY, XREFA, XSOLIN, XSOLS, XSOLM,
     *   APARM(10), DPARM(10), XSNVER, XANT(30), XBIF, XEIF, XBADD(10),
     *   DELTIM, BUFF1(XBFSZ), BUFF2(XBFSZ), ANTWT(MAXANT), IATOFF,
     *   TINT, TINTG, SOLINT, DELWIN, RATWIN, SNRMIN, MXPABL, MNPABL,
     *   WTPABL, RINWIN, XORD(10), XDOFIT(30)
      DOUBLE PRECISION  RANOD, DECNOD
      REAL      FINC(MAXIF)
      INTEGER   ISBAND(MAXIF)
      EQUIVALENCE (IBUFF1, BUFF1)
      COMMON /CINFO/ RANOD, DECNOD, CATIN, DELTIM, TINT, IATOFF, ANTWT,
     *   TINTG, SOLINT, DELWIN, RATWIN, SNRMIN, MXPABL, MNPABL, WTPABL,
     *   NCOMP, REFUSE, SINGLE, DOMODL, TSMOTH, AVGIF, DOIF, DOMS,
     *   AVGPOL, DODRLS, DAVOUT, RINWIN, PRIRTY, GDSOLV, DOEVLA, DODISP,
     *   CNOIN, CNOOUT, NANT, NFREQ, NPOL, NVAL, REFANT, REFUSS, CNOIN2,
     *   CCTVER, SNVER, VISDSK, VISCNO, VER, NUMNOD, NUMBL, NUMTIM,
     *   PRTLV, BLDO, MINNO, LOCIF, LOCF, ZDEL, ZRAT, ZPHS,DISKIN,
     *   DISK2, DISOUT, SEQIN, SEQ2, SEQOUT, NORAT, CHINC, LBIF,LEIF
      COMMON /BUFRS/ BUFF1, BUFF2, BUFFS, FINC, ISBAND, JBUFSZ
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XCALCO, NAME2, CLAS2,
     *   NAMOUT, CLAOUT, CMETH, CMOD
      COMMON /XINPUT/ XNAMEI, XCLAIN, XSI, XDI, XXSOUR, XQUAL, XXCALC,
     *   XBAND, XFREQ, XFQID, XTIME, XBCHAN, XECHAN, XCHINC, XANTS,
     *   XDOFIT, XSUBA, XUVRA, XWTUV, XWTIT, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XNAME2, XCLAS2,
     *   XS2, XD2, XVER, XNCOMP, XFLUX, XNMAP, XCMETH, XCMOD, SMODEL,
     *   DOAPLY, XNAMOU, XCLAOU, XSO, XDO, XREFA, XORD, XSOLIN, XSOLS,
     *   XSOLM, APARM, DPARM, XSNVER, XANT, XBIF, XEIF, XBADD
C                                                          End FRING
LOCAL END
      PROGRAM FRING
C-----------------------------------------------------------------------
C! Fringe fit interferometer data.
C# UV Calibration EXT-appl AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2016
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
C   NMAPS             0.0       16.0   No. Clean map files
C                                      Output uv data file.
C   OUTNAME                               UV file name (name)
C   OUTCLASS                              UV file name (class)
C   OUTSEQ            -1.0      9999.0    UV file name (seq. #)
C   OUTDISK            0.0         9.0    UV file disk drive #
C   APARM                              General parameters
C   DPARM                              Delay-rate parameters
C   SNVER                              Ouptut SN table.
C   ANTWT                              Ant. weights (0=>1.0)
C   GAINERR(30)                        Std. Dev. of antenna gains.
C   BADDISK            0.0         9.0 Disk no. not to use for
C                                         scratch files.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   MAXBL, MAXFRQ, MAXTIM
      INTEGER   NUMSUB, ISUB, IS1, IS2, IST, IRET
      LOGICAL   GOTD
      INCLUDE 'FRING.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'FRING '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL FRNGIN (PRGM, MAXBL, MAXFRQ, MAXTIM, NUMSUB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Loop over subarrays
      GOTD = .FALSE.
      IS1 = SUBARR
      IS2 = SUBARR
      IST = SUBARR
      IF (SUBARR.EQ.0) IS1 = 1
      IF (SUBARR.EQ.0) IS2 = NUMSUB
      DO 100 ISUB = IS1,IS2
         WRITE (MSGTXT,1000) ISUB, IS2
         IF (IS1.NE.IS2) CALL MSGWRT (4)
         SUBARR = ISUB
C                                       Select data.
         CALL FRNSEL (IRET)
         IF (IRET.GT.0) GO TO 990
C                                       Check if data found
         IF ((NVIS.LE.0) .OR. (IRET.LT.0)) GO TO 100
         GOTD = .TRUE.
C                                       Divide data by model if nec.
         IF (DOMODL) CALL FRNMOD (IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Do solutions.
         CALL FRNSOL (MAXBL, MAXTIM, MAXFRQ, IRET)
         IF (IRET.NE.0) GO TO 990
 100     CONTINUE
C                                       Make sure did something
      IF (.NOT.GOTD) THEN
C                                       No data selected
         IRET = 1
         WRITE (MSGTXT,1100)
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Restore subarray
      SUBARR = IST
C                                       Smooth solutions
      IF ((DPARM(7).LE.0.0) .OR. (DPARM(8).GT.0.0)) CALL FRNADJ (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Apply solns. and average data.
C                                       Single source files only.
      IF ((SINGLE) .AND. (DOMODL) .AND. (DOAPLY.GE.0.0)) THEN
         CALL FRNAPL (IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       Write history.
      CALL FRNHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT (' START SUBARRAY ',I4,' OF ',I4)
 1100 FORMAT (' WARNING: NO DATA SELECTED')
      END
      SUBROUTINE FRNGIN (PRGN, MAXBL, MAXFRQ, MAXTIM, NUMSUB, IRET)
C-----------------------------------------------------------------------
C   FRNGIN gets input parameters for FRING and creates an output file
C   if necessary.
C   Inputs:  PRGN    C*6       Program name
C   Output:  MAXBL   I         Maximum number of baselines in data.
C            MAXFRQ  I         Maximum number of frequency channels.
C            MAXTIM  I         Maximum number of integrations per
C                              solution interval.
C            NUMSUB  I         Number of subarrays (AN tables)
C            IRET    I         Error code: 0 => ok
C                                1 => too few frequency channels.
C                                5 => catalog troubles
C                                7 => Too many ant. for ls.
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in FRING for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6, STAT*4, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   MAXBL, MAXFRQ, MAXTIM, NUMSUB, IRET
      INTEGER   IERR, NPARM, I, MXFLD, IROUND, MXANT, NUMFRQ, LUN1,
     *   I4TEMP, ANVER, LIM1, LIM2, NTIM, J, K, CURANT
      LOGICAL   T, TABLE, EXIST, FITASC, MATCH, WASERR, DUM, COMPRS
      REAL      CATR(256), T1, T2
      DOUBLE PRECISION CATD(128)
      INCLUDE 'FRING.INC'
      REAL TAU(MXBASE), TAUMIN, TAUMAX
      INTEGER IBLAVG(MXBASE), NBLAVG
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DANT.INC'
      EQUIVALENCE (CATR, CATBLK, CATH, CATD)
      DATA LUN1 /28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSL * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
      NSOUWD = 1
C                                       Get input parameters.
      MXFLD = MAXAFL
      NPARM = 303 + MXFLD + 40
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSI)
      SEQ2 = IROUND (XS2)
      SEQOUT = IROUND (XSO)
      DISKIN = IROUND (XDI)
      DISK2 = IROUND (XD2)
      DISOUT = IROUND (XDO)
      CCTVER = IROUND (XVER)
      CCTVER = MAX (0, CCTVER)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (4, 1, XCMETH, CMETH)
      CALL H2CHR (4, 1, XCMOD, CMOD)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 20      CONTINUE
C                                      Default ant. wt = 1.0
      MXANT = MAXANT
      CALL RFILL (MXANT, 1.0, ANTWT)
      DO 30 I = 1, 30
         ANTWT(I) = XANT(I)
         IF (XANT(I).LE.0.0) ANTWT(I) = 1.0
 30      CONTINUE
C                                       Zero ref. ant. count.
      CALL FILL (MAXANT, 0, REFUSE)
      I = 100 * MAXANT
      CALL FILL (I, 0, REFUSS)
C                                       Get CATBLK from old file.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 990
         END IF
C                                       Save Input file info
      VISDSK = DISKIN
      VISCNO = CNOIN
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       See if a multiple source file
      LUNS(1) = 29
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNS, BUFF1, TABLE, EXIST,
     *   FITASC, IERR)
      SINGLE = (.NOT.EXIST) .OR. (IERR.NE.0) .OR. (ILOCSU.LT.0)
C                                       Save IF and freq pointers
      LOCIF = JLOCIF
      LOCF = JLOCF
      COMPRS = CATBLK(KINAX).EQ.1
C                                       Freq id
      IF (.NOT.SINGLE) THEN
         IF (XBAND.GT.0.0) SELBAN = XBAND
         IF (XFREQ.GT.0.0) SELFRQ = XFREQ
         FRQSEL = IROUND (XFQID)
         IF (FRQSEL.EQ.0) FRQSEL = -1
         CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN1, SELBAN, SELFRQ,
     *      MATCH, FRQSEL, IRET)
         IF (.NOT.MATCH) THEN
            WRITE (MSGTXT,1040)
            IRET = 1
            GO TO 990
            END IF
         IF (IRET.GT.0) GO TO 999
         END IF
C                                       General parms
      SOLINT = XSOLIN / (24.0 * 60.0)
      MXPABL = XUVRA(2)
      IF (MXPABL.LE.1.0E-20) MXPABL = 1.0E15
      MNPABL = XUVRA(1)
      WTPABL = XWTUV
      REFANT = IROUND (XREFA)
C                                       Def. min. no. of antennas = 3
      MINNO = IROUND (APARM(1))
      IF (MINNO.LE.0) MINNO = 3
      DOMODL = APARM(2).LE.1.0E-20
      AVGPOL = APARM(3).GT.0.0
      AVGIF = APARM(4).GT.0.0
C                                       pieces
      DOEVLA = IROUND (APARM(5))
      IF (DOEVLA.EQ.2) DOEVLA = 1
      IF (JLOCIF.LT.0) DOEVLA = 0
      DOIF = DOEVLA.LE.0
      IF (DOEVLA.GT.0) THEN
         DOEVLA = MAX (1, DOEVLA-1)
         IF ((CATBLK(KINAX+JLOCIF)/DOEVLA)*DOEVLA.NE.
     *      CATBLK(KINAX+JLOCIF)) THEN
            WRITE (MSGTXT,1035) DOEVLA
            IRET = 1
            GO TO 990
            END IF
         END IF
C                                       dispersion fit forces SB fitting
      DODISP = 0
      IF ((APARM(10).GT.0.0) .AND. (JLOCIF.GT.0) .AND.
     *   (CATBLK(KINAX+JLOCIF).GT.1)) THEN
         DODISP = DOEVLA
         APARM(5) = 0.0
         DOIF = .TRUE.
         DOEVLA = 0
         END IF
C                                       DOMS requires multiple IFs.
      DOMS = (APARM(5).GT.1.5) .AND. (APARM(5).LT.2.5) .AND.
     *   ((JLOCIF.GT.0) .AND. (CATBLK(KINAX+JLOCIF).GT.1))
C                                       DOMS overrides AVGIF
      IF (DOMS) THEN
         AVGIF = .FALSE.
         APARM(4) = 0.0
         END IF
      PRTLV = IROUND (APARM(6))
      SNRMIN = APARM(7)
C                                       Def. SNR min = 5.0
      IF (SNRMIN.LE.1.0E-30) SNRMIN = 5.0
C                                       Default s.i. = 10 min.
      IF (SOLINT.LE.1.0E-10) SOLINT = 10.0 / (24.0 * 60.0)
      BLDO = IROUND (DPARM(1))
      IF ((BLDO.LT.1) .OR. (BLDO.GT.3)) BLDO = 3
      DODRLS = DPARM(5).LE.0.0
      DAVOUT = (DPARM(6).LE.0.0)
      IF (DPARM(8).GT.0) THEN
         I = IROUND(DPARM(8))
         J = MOD(I,2)
         I = (I-J)/2
         ZRAT = J.EQ.1
         J = MOD(I,2)
         I = (I-J)/2
         ZDEL = J.EQ.1
         J = MOD(I,2)
         I = (I-J)/2
         ZPHS = J.EQ.1
      ELSE
         ZRAT = .FALSE.
         ZDEL = .FALSE.
         ZPHS = .FALSE.
         END IF
      NORAT = DPARM(9).GT.0.0
C                                       BADDISK
      DO 70 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 70      CONTINUE
C                                       Check sort order, must be T*
      IF (ISORT(1:1) .NE.'T') THEN
         IRET = 4
         WRITE (MSGTXT,1070) ISORT, 'T*'
         GO TO 990
         END IF
C                                       Save input header.
      CALL COPY (256, CATBLK, CATIN)
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      IF (.NOT.SINGLE) THEN
         DO 80 I = 1,30
            SOURCS(I) = XSOUR(I)
            CALSOU(I) = XSOUR(I)
 80         CONTINUE
         SELQUA = IROUND (XQUAL)
         SELCOD = XCALCO
         END IF
      CALL RCOPY (8, XTIME, TIMRNG)
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      CHINC = XCHINC + 0.5
      CHINC = MAX (1, MIN (ECHAN-BCHAN+1, CHINC))
      I = (ECHAN - BCHAN + 1) / CHINC
      ECHAN = BCHAN + I * CHINC - 1
C                                       If DPARM(2) < 0.0 then there
C                                       is no delay search;
C                                       if DPARM(2) = 0.0 then use
C                                       default range.
      IF (ABS(DPARM(2)).LT.1.0E-20) THEN
         DELWIN = 1.0D+9 / (ABS (CATR(KRCIC+JLOCF)) * CHINC)
      ELSE
         DELWIN = DPARM(2)
         END IF
      IF ((BCHAN.EQ.ECHAN) .OR. (DELWIN.LT.0.0)) THEN
         RINWIN = -1.0
      ELSE
         RINWIN = 0.0
         END IF
      BIF = 1
      EIF = 1
      IF (JLOCIF.GE.0) EIF = CATBLK(KINAX+JLOCIF)
      IF ((DOEVLA.GT.0) .AND. (DOEVLA.LT.EIF)) THEN
         LBIF = XBIF + 0.1
         LBIF = MAX (1, MIN (EIF, LBIF))
         LEIF = XEIF + 0.1
         IF (LEIF.LT.LBIF) LEIF = EIF
         LEIF = MAX (1, MIN (EIF, LEIF))
      ELSE
         LBIF = 1
         LEIF = EIF
         END IF
C                                       0 relative
      LBIF = LBIF - 1
      LEIF = LEIF - 1
C                                       Find number of subarrays
      CALL FNDEXT ('AN', CATH, NUMSUB)
      NANT = 0
C                                       Check selected subarrays
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.GT.0) THEN
         LIM1 = SUBARR
         LIM2 = SUBARR
      ELSE
         LIM1 = 1
         LIM2 = NUMSUB
         END IF
      WASERR = .FALSE.
      DO 165 ANVER = LIM1,LIM2
         CALL ANMAXA (DISKIN, CNOIN, ANVER, CATIN, I, IRET)
         IF (IRET.NE.0) THEN
            WASERR = .TRUE.
         ELSE
            NANT = MAX (NANT, I)
            END IF
 165     CONTINUE
      IF (WASERR) NANT = MAX (IROUND (APARM(8)), NANT)
      IF (NANT.LE.1) THEN
         MSGTXT = 'ENCOUNTERED PROBLEM DETERMINING NO. ANTENNAS'
         CALL MSGWRT (6)
         MSGTXT = 'SET APARM(8) AND RERUN'
         IRET = 8
         GO TO 990
         END IF
C                                       Determine max. bl., time.
      NUMANT = NANT
      IF (REFANT.GT.NANT) REFANT = 0
C                                       Antennas
      DO 85 I = 1,50
         ANTENS(I) = IROUND (XANTS(I))
 85      CONTINUE
C                                       Use new adverb [loaded as XORD]
C                                       to construct prioritized
C                                       antennas list which will be
C                                       consulted when FFTing to find
C                                       initial delay/rate/phase solns
C                                       default is no priority list at
C                                       all
      PRIRTY(1) = -1
      DO 84 I = 1,MAXANT
         PRIRTY(1+I) = 0
 84      CONTINUE
      IF (APARM(9).GT.0) THEN
C                                       use order of order adverb as
C                                       priority list
C
         J = 1
         IF (REFANT.GT.0) THEN
C                                       default is to put REFANT in the first
C                                       position [highest priority]
            PRIRTY(1+J) = REFANT
            J = 2
            END IF
         DO 95 I = 1,10
C                                       Go through XORD list
            CURANT = IROUND (XORD(I))
C                                       but only nonzero entries
            IF (CURANT.GT.0) THEN
               DO 90 K = 1,J-1
C                                       check if CURANT has appeared already
                  IF (CURANT.EQ.PRIRTY(1+K)) CURANT = 0
 90               CONTINUE
               END IF
C                                       If curant is new, add to end of PRIRTY
C                                       list
            IF (CURANT.GT.0) THEN
               PRIRTY(1+J) = CURANT
               J = J + 1
               END IF
 95         CONTINUE
C                                       save length of PRIRTY list in PRIRTY(1)
C                                       actual length = 1+J
         PRIRTY(1) = J-1
         END IF
C                                       If any GSOLV entries are non-blank
C                                       solve only for those antennas.
C                                       excepting the reference antenna
      GDSOLV(1) = .TRUE.
      DO 100 I = 1,30
         J = IROUND (XDOFIT(I))
         IF (J.LT.0) GDSOLV(1) = .FALSE.
 100     CONTINUE
      CALL LFILL (MAXANT, GDSOLV(1), GDSOLV(2))
      GDSOLV(1) = .NOT.GDSOLV(1)
      IF (GDSOLV(1)) THEN
         DO 105 I = 1,30
            J = IROUND (XDOFIT(I))
            J = ABS (J)
            IF ((J.GT.0) .AND. (J.LE.MAXANT)) GDSOLV(J+1) = .TRUE.
 105        CONTINUE
      ELSE
         DO 110 I = 1,30
            J = IROUND (XDOFIT(I))
            IF ((J.GT.0) .AND. (J.LE.MAXANT)) THEN
               GDSOLV(J+1) = .FALSE.
               GDSOLV(1) = .TRUE.
               END IF
 110        CONTINUE
         END IF
C                                       Be sure that REFANT is listed as
C                                       nosolv
      IF (GDSOLV(1)) THEN
         IF (REFANT.LE.0) THEN
            IRET = 8
            MSGTXT = 'DOFIT OPTION REQUIRES REFANT > 0'
            GO TO 990
         ELSE IF (.NOT.GDSOLV(1+REFANT)) THEN
            IRET = 8
            MSGTXT = 'REFANT MUST NOT APPEAR IN DOFIT LIST'
            GO TO 990
            END IF
         END IF
C     IF (GDSOLV(1).AND.REFANT.GT.0) GDSOLV(1+REFANT) = .TRUE.
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      SNVER = IROUND (XSNVER)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      BLVER = IROUND (XBLVER)
C                                        Spectral smoothing
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       set no. frequencies
      NFREQ = CATBLK(KINAX+JLOCF)
      IRET = 0
      IATOFF = 0.0
C                                       Find pre-average time if
C                                       not specified.
      TINTG = DPARM(4) / 86400.0
C                                       Find minumum pre-average time
C                                       even if supplied so if
C                                       specified int. time > than
C                                       found int. time warning can
C                                       be given.
      T1= XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / 1440.0 +
     *   XTIME(4) / 86400.0
      T2 = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / 1440.0 +
     *   XTIME(8) / 86400.0
      IF (T2.LE.T1) T2 = T1 + 1.0E20
C                                       Read the uv-data to find
C                                       the pre-average times
      NBLAVG = 0
      DO 200 J = 1, NANT-1
         DO 180 K = J+1, NANT
            NBLAVG = NBLAVG + 1
            IBLAVG(NBLAVG) = 32768 * J + K
180         CONTINUE
200      CONTINUE
C
      CALL AVERT (T1, T2, IBLAVG, NBLAVG, BUFF1, TAU, TAUMIN, TAUMAX,
     *   IRET)
      IF (IRET.GT.0) THEN
         MSGTXT = 'AVERT RETURNS FATAL ERROR'
         GO TO 990
         END IF
C                                       units days??
      IF (TAUMIN.LT.0.005) TAUMIN = TAUMIN * 86400.0
C
      IF (TINTG.LE.1.0E-20) THEN
C                                       Integration time not found
         IF (TAUMAX.EQ.0.0) THEN
            IRET = 9
            MSGTXT = 'CANNOT DETERMINE INT. TIME FROM DATA;' //
     *         ' SET DPARM(4)'
            GO TO 990
            END IF
         TINTG = TAUMIN / 86400.0
         WRITE (MSGTXT,1200) TAUMIN
         CALL MSGWRT (4)
      ELSE
         IF (TINTG.GT.TAUMIN/86400.0) THEN
            MSGTXT = 'WARNING: Input int. time (DPARM(4)) is greater'
     *         // ' than'
            CALL MSGWRT (6)
            MSGTXT = '         than that found in the data.  Please be'
     *         // ' aware'
            CALL MSGWRT(6)
            MSGTXT = '         that this can cause odd errors.'
            CALL MSGWRT(6)
            END IF
         END IF
C                                       Correct SOLINT by TINTG/2
      SOLINT = SOLINT - TINTG * 0.5
C                                       Set rate window
      RATWIN = DPARM(3)
      IF (RATWIN.LE.1.0E-20) RATWIN = 1000.0 / (TINTG * 86400.0)
C                                       Set integration time for
C                                       rate smearing correction
C                                       in UVGET.
      IF (ILOCIT.GE.0) THEN
         DXTIME = 0.0
      ELSE
         DXTIME = TINTG
         END IF
C
      NUMIF = 1
      IF (JLOCIF.GE.0) NUMIF = CATBLK(KINAX+JLOCIF)
      MAXBL = (NANT * (NANT - 1)) / 2
      MAXFRQ = NFREQ * NUMIF
C                                       Number of channels to process
      I4TEMP = ((ECHAN - BCHAN) / CHINC + 1) * NUMIF
      IF (AVGIF) I4TEMP = NUMIF
      IF (MAXFRQ.GT.I4TEMP) MAXFRQ = I4TEMP
      I4TEMP = MAXBL * MAXFRQ
C                                       How big to make the buffers
      MAXTIM = 2 * SOLINT / TINTG + 1.5
      MAXTIM = MAX (2, MAXTIM)
      IRET = 0
C                                       Check SOLINT
      IF (XSOLIN.GT.0.0) THEN
         NTIM = SOLINT / TINTG
         IF (NTIM.GT.MAXTIM) THEN
            T1 = SOLINT * 24. * 60.
            T2 = (MAXTIM * TINTG) * 24. * 60.
            WRITE (MSGTXT,1205) T1
            CALL MSGWRT (6)
            WRITE (MSGTXT,1210) T2
            CALL MSGWRT (6)
            SOLINT = T2 / (24. * 60.)
            SOLINT = SOLINT - TINTG * 0.5
            END IF
         END IF
C                                       Create output file for SINGLE
C                                       and not already divided by
C                                       model.
      IF ((SINGLE) .AND. (DOMODL) .AND. (DOAPLY.GE.0.0)) THEN
         STOKES = ' '
         CALL UVGET ('INIT', DUM, DUM, IRET)
         IF (IRET.EQ.0) CALL UVGET ('CLOS', DUM, DUM, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1149) IRET
            GO TO 990
            END IF
         CNOOUT = 1
         FRW(NCFILE+1) = 3
         IRET = 4
         IF (.NOT.DOMODL) CATBLK(KIGCN) = 1
         NUMFRQ = CATBLK(KINAX+JLOCF)
         IF (COMPRS) THEN
            CATBLK(KINAX) = 1
            CALL CHR2H (8, 'WEIGHT  ', 1, CATH(KHPTP+2*CATBLK(KIPCN)))
            CATBLK(KIPCN) = CATBLK(KIPCN) + 1
            CALL CHR2H (8, 'SCALE   ', 1, CATH(KHPTP+2*CATBLK(KIPCN)))
            CATBLK(KIPCN) = CATBLK(KIPCN) + 1
            END IF
C                                       Put new values in CATBLK.
         IF (CLAOUT.EQ.' ') CLAOUT = 'FRING '
         CALL MAKOUT (NAMEIN, CLAIN, SEQIN, ' ', NAMOUT, CLAOUT,
     *      SEQOUT)
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQOUT
C                                       Check if averaging output freq.
         IF (DAVOUT) CATBLK(KINAX+JLOCF) = 1
         CALL UVCREA (DISOUT, CNOOUT, BUFF1, IRET)
         CATBLK(KINAX+JLOCF) = NUMFRQ
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1150) IRET
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISOUT
         FCNO(NCFILE) = CNOOUT
         FRW(NCFILE) = FRW(NCFILE) - 1
C                                       copy keywords
         CALL KEYCOP (DISKIN, CNOIN, DISOUT, CNOOUT, IRET)
C                                       Save CATBLK for output.
         CALL CATIO ('UPDT', DISOUT, CNOOUT, CATBLK, 'REST', BUFF1,
     *      IRET)
         IF ((IRET.NE.0) .AND. (IRET.LT.5)) THEN
            WRITE (MSGTXT,1170) IRET
            GO TO 990
            END IF
C                                       Other "single" parameters
C                                       allow cal and flag of single

C         DOBAND = 0
C         FGVER = -1
C                                       Make SN be cumulative not
C                                       incremental for single source
         SNVER = 0
         END IF
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Stokes' - ask for what's needed
      STOKES = '    '
      IF (CATD(KDCRV+JLOCS).GT.0.0) STOKES = 'I'
      IF ((CATD(KDCRV+JLOCS).LT.0.0) .AND. (CATBLK(KINAX+JLOCS).GE.2))
     *   STOKES = 'HALF'
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FRNGIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,' DISK=',I3,' USID=',I4)
 1035 FORMAT ('NUMBER IFS NOT INTEGER TIMES',I2,' QUITTING')
 1040 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1050 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('SORT ORDER IS ',A2,' NOT ',A2,' AS REQUIRED')
 1149 FORMAT ('ERROR',I3,' OPENING INPUT DATA')
 1150 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1170 FORMAT ('ERROR',I3,' UPDATING OUTPUT FILE CATBLK')
 1200 FORMAT ('Set integration time to',F10.6,
     *   ' seconds, hope that is ok')
 1205 FORMAT ('WARNING: Soln int. ',F7.2,' min, too long,')
 1210 FORMAT ('         resetting to ',F7.2,' min.')
      END
      SUBROUTINE FRNSEL (IRET)
C-----------------------------------------------------------------------
C   FRNSEL will read a multi source data set into a temporary scratch
C   file.  Editing and calibration may be applied.
C   Inputs via common /SELCAL/  (Include DSEL.INC)
C      UNAME        C    AIPS name of input file.
C      UCLAS        C    AIPS class of input file.
C      UDISK        R    AIPS disk of input file.
C      USEQ         R    AIPS sequence of input file.
C      SOURCS(30)   C    Names (16 char) of up to 30 sources, *=>all
C                        First character of name '-' => all except those
C                        specified.
C      TIMRNG(8)    R    Start day, hour, min, sec, end day, hour,
C                        min,sec. 0's => all
C      UVRNG(2)     R    Minimum and maximum baseline lengths in
C                        1000's wavelengths. 0's => all
C      STOKES       C    Stokes types wanted.
C                        'I','Q','U','V','R','L','IQU','IQUV'
C      BCHAN        I    First channel number selected, 1 rel. to first
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
C                        -1 => end of data
C                        >0 => failed, abort process.
C-----------------------------------------------------------------------
      INTEGER  IRET, LUN1, LUN2, IIVER, OOVER
      REAL     DUM
      INCLUDE 'FRING.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DOUBLE PRECISION FOFF(MAXIF), FQBIF
      INTEGER   NIF, I, CHIF, J, I1, I2
      CHARACTER BNDCOD(MAXIF)*8
      DATA LUN1, LUN2 /28, 29/
C-----------------------------------------------------------------------
C                                       Setup
      CALL UVGET ('INIT', DUM, DUM, IRET)
      IF (IRET.GT.0) GO TO 999
      IF (IRET.LT.0) NVIS = 0
      IF ((NVIS.LE.0) .OR. (IRET.LT.0)) GO TO 100
C                                       Message
      WRITE (MSGTXT,2000)
      IF (DOCAL) WRITE (MSGTXT,2001)
      IF (DOFLAG) WRITE (MSGTXT,2002)
      IF (DOCAL.AND.DOFLAG) WRITE (MSGTXT,2003)
      CALL MSGWRT (6)
C                                       Copy
      VISDSK = 0
      VISCNO = 0
      CALL CALCOP (VISDSK, VISCNO, BUFF1, JBUFSZ, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy relevant portion of IF
C                                       table. Read all IFs from old
C                                       CH/FQ table
      IIVER = 1
      CALL CHNDAT ('READ', BUFF1, DISKIN, CNOIN, IIVER, CATUV, LUN1,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,2040) IRET
         GO TO 990
         END IF
C                                       Correct for ref. freq.
C                                       change in UVGET
      FQBIF =  FREQ - UVFREQ
      DO 45 I = 1, NIF
         FOFF(I) = FOFF(I) - FQBIF
 45      CONTINUE
C                                       check signs
      IF (DOEVLA.GT.0) THEN
         CHIF = NIF / DOEVLA
         DO 60 J = 1,NIF,CHIF
            I1 = (J-1) * CHIF + 1
            I2 = J * CHIF
            DO 50 I = I1,I2-1
               IF (FINC(I)*(FOFF(I+1)-FOFF(I)).LT.0.0D0) THEN
                  IRET = 1
                  MSGTXT = 'FREQUENCY INCREMENTS NOT ALL SAME' //
     *               ' SIGN IN GROUP'
                  CALL MSGWRT (8)
                  MSGTXT = 'YOU MAY NEED TASK FLOPM'
                  GO TO 990
                  END IF
 50            CONTINUE
 60         CONTINUE
         END IF
C                                       Write new FQ table
      CALL CHNDAT ('WRIT', BUFF1, SCRVOL(VISCNO), SCRCNO(VISCNO),
     *   OOVER, CATBLK, LUN2, NIF, FOFF, ISBAND, FINC, BNDCOD,
     *   FRQSEL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,2050) IRET
         GO TO 990
         END IF
      GO TO 999
C                                       No data
 100  CALL UVGET ('CLOS', DUM, DUM, I)
      GO TO 999
C                                       Error message
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 2000 FORMAT ('Selecting the data')
 2001 FORMAT ('Selecting and calibrating the data')
 2002 FORMAT ('Selecting and editing the data')
 2003 FORMAT ('Selecting, editing and calibrating the data')
 2040 FORMAT ('FRNSEL: ERROR',I3,' READING OLD FQ TABLE')
 2050 FORMAT ('FRNSEL: ERROR',I3,' WRITING NEW FQ TABLE')
      END
      SUBROUTINE FRNMOD (IRET)
C-----------------------------------------------------------------------
C   FRNMOD divides the CLEAN model visibilities into the data.
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
      INTEGER   MODEL, METHOD, ISTOKE, DISKO, ISCR, CHAN, NCHAN, I, IIF,
     *   IROUND
      LOGICAL   DOMSG, F, NONAM, NOCLAS, WASOME
      INCLUDE 'FRING.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INTEGER   BITER(MAXFLD)
      REAL      RBUF(MAXIF)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSCD.INC'
      DATA DOMSG, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if multiple sources
      IF (SINGLE) NSOUWD = 1
      IF (NSOUWD.NE.1) GO TO 300
C                                       If neither a point (SMODEL) nor
C                                       clean model use source table.
      NONAM = NAME2.EQ.'            '
      NOCLAS = CLAS2.EQ.'      '
C                                       If single force model
      IF (SINGLE .AND. NONAM .AND. NOCLAS .AND. (SMODEL(1).LE.1.0E-20))
     *   SMODEL(1) = 1.0
C                                       Use source table?
      IF (NONAM .AND. NOCLAS .AND. (SMODEL(1).LE.1.0E-20)) GO TO 300
C                                       Set model and method
C                                       (Options limited by sort order.)
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
      MFIELD = IROUND (XNMAP)
      IF (MFIELD.LE.0) MFIELD = 1
      LIMFLX = XFLUX
      NONEG = F
      WASOME = F
      DO 10 I = 1,MFIELD
         BITER(I) = 1
         IF (I.LE.MAXAFL) THEN
            NCOMP(I) = ABS (XNCOMP(I)) + 0.1
            IF (XNCOMP(I).LE.-0.5) NONEG = .TRUE.
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
      XVER = CCTVER
      CNOIN2 = CCCNO(1)
C                                       Divide data by model
      DISKO = VISDSK
      ISCR = VISCNO
      COMPDT = .FALSE.
      DATDIV = .TRUE.
C                                       Consider whether to process
C                                       1 IF at a time
      IF ((NUMIF.GT.1) .AND. (MODEL.EQ.1) .AND.
     *   (ABS(FACGRD(1)-1.0).GT.1.E-10)) THEN
C                                       number of channels
         NCHAN = CATBLK(KINAX+JLOCF)
C                                       For each IF
         DO 15 IIF = 1,NUMIF
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
         NCHAN = CATBLK(KINAX+JLOCF) * NUMIF
C                                       Div all vis by model
         CALL UVMDIV (VISDSK, VISCNO, DISKO, ISCR, MODEL, METHOD, DOMSG,
     *      CHAN, NCHAN, CATBLK, JBUFSZ, FRQSEL, BUFF1, BUFF2, UBUFF,
     *      RBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (.NOT.DOPTMD) CALL UNSETG (BUFF2)
         END IF
C                                       Get true values of NCOMP
      DO 20 I = 1,MFIELD
         NCOMP(I) = MAX (NSUBG(I), 1) - 1
C                                       Tell user about CCs
         IF (LIMFLX.GT.0.0) THEN
            IF (NONEG) THEN
               WRITE (MSGTXT,1000) I, NCOMP(I), LIMFLX
            ELSE
               WRITE (MSGTXT,1010) I, NCOMP(I), LIMFLX
               END IF
         ELSE
            IF (NONEG) THEN
               WRITE (MSGTXT,1100) I, NCOMP(I)
            ELSE
               WRITE (MSGTXT,1110) I, NCOMP(I)
               END IF
            END IF
         CALL MSGWRT(3)
 20      CONTINUE
C                                       Model divided by data now
C                                       in scratch file.
      VISDSK = 0
      VISCNO = ISCR
      GO TO 999
C                                       Multiple sources, use point
C                                       source at phase center only.
 300  IRET = 0
      MSGTXT = 'No need to divide by flux densities in this task'
      CALL MSGWRT (2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Field',I5,' used',I8,' CCs before 1st neg with flux>',
     *   F9.6)
 1010 FORMAT ('Field',I5,' used',I8,' CCs with flux>',F9.6)
 1100 FORMAT ('Field',I5,' used',I8,' CCs before 1st negative')
 1110 FORMAT ('Field',I5,' used',I8,' CCs')
      END
      SUBROUTINE FRNHIS
C-----------------------------------------------------------------------
C   FRNHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(7)*2, CTIME(2)*12, HILINE*72
      INTEGER   LUN1, LUN2, LIMIT, IERR, ITEMP, I, TIME(3), DATE(3),
     *   NONOT, SOLSUB, SOLMIN
      LOGICAL   T
      INCLUDE 'FRING.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /7,'SN','NX','CL','CH','BP','FQ','BL'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       sub interval parameters
      SOLMIN = XSOLM + 0.1
      SOLSUB = XSOLS + 0.1
      IF (SOLSUB.LE.0) SOLSUB = 1
      IF (SOLSUB.GT.10) SOLSUB = 10
      IF ((SOLMIN.LE.0) .OR. (SOLMIN.GT.SOLSUB)) SOLMIN = SOLSUB
      IF ((.NOT.SINGLE) .OR. (.NOT.DOMODL) .OR. (DOAPLY.LT.0.0)) THEN
C                                       Multisource - open old history
         CALL HIOPEN (LUN2, DISKIN, CNOIN, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Task message
         CALL ZDATE (DATE)
         CALL ZTIME (TIME)
         CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
         WRITE (HILINE,1000) TSKNAM, RLSNAM, CTIME
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Copy/open history file.
      ELSE
         CALL HISCOP (LUN1, LUN2, DISKIN, DISOUT, CNOIN, CNOOUT, CATBLK,
     *      BUFF1, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            WRITE (MSGTXT,1100) IERR
            CALL MSGWRT (6)
            GO TO 190
            END IF
C                                       New history
         CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2,
     *      BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISOUT, LUN2,
     *      BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       calibration HI
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Write control info.
C                                       CC tables
      IF ((DOMODL) .AND. (SMODEL(1).EQ.0.0)) THEN
C                                       CC File Name etc.
         CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUN2, BUFF2,
     *      IERR)
         IF (IERR.NE.0) GO TO 190
C                                        CCfile version no.
         WRITE (HILINE,2001) TSKNAM, CCTVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                        Number of images
         WRITE (HILINE,2002) TSKNAM, MFIELD
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Number of CLEAN components.
         DO 140 I = 1,MFIELD
            WRITE (HILINE,2003) TSKNAM, I, NCOMP(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
 140     CONTINUE
         END IF
C                                       General information
C                                       Soln. interval.
      XSOLIN = SOLINT * 24.0 * 60.0
      IF (XSOLIN.GT.9999.99) XSOLIN = 9999.99
      WRITE (HILINE,2010) TSKNAM, XSOLIN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       subintervals
      IF (SOLSUB.GT.1) THEN
         WRITE (HILINE,2005) TSKNAM, SOLSUB
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,2006) TSKNAM, SOLMIN
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                        Reference ant
      WRITE (HILINE,2012) TSKNAM, REFANT
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Bchan/Echan
      WRITE (HILINE,2017) TSKNAM, BCHAN, ECHAN, CHINC
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Min. no. antennas
      WRITE (HILINE,2011) TSKNAM, MINNO
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Average RR,LL
      ITEMP = -1
      IF (AVGPOL) ITEMP = 1
      WRITE (HILINE,2013) TSKNAM, ITEMP
      IF (NCOR.GT.1) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Average freq in IF
      ITEMP = -1
      IF (AVGIF) ITEMP = 1
      WRITE (HILINE,2014) TSKNAM, ITEMP
      IF (EIF.GT.BIF) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Soln in each IF?
      ITEMP = 1
      IF (DOIF) ITEMP = -1
      IF (DOMS) ITEMP = 2
      WRITE (HILINE,2015) TSKNAM, ITEMP
      IF (EIF.GT.BIF) THEN
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         IF (APARM(10).GT.0.0) THEN
            HILINE = TSKNAM //
     *         '/ then did multi-band and dispersion fit'
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
            END IF
         END IF
C                                        Multi- and single-band fit
      IF (DOMS) THEN
         WRITE (HILINE,3015) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      IF (DOEVLA.EQ.1) THEN
         HILINE = TSKNAM // '/ IFs combined for 1 solution only'
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
      ELSE IF (DOEVLA.EQ.2) THEN
         HILINE = TSKNAM // '/ IFs divided in half for 2 solutions'
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
      ELSE IF (DOEVLA.EQ.4) THEN
         HILINE = TSKNAM // '/ IFs divided in quarters for 4 solutions'
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      IF (DOEVLA.GT.0) THEN
         IF (DOEVLA.EQ.1) THEN
            HILINE = TSKNAM // '/ IFs combined for 1 solution only'
         ELSE IF (DOEVLA.EQ.2) THEN
            HILINE = TSKNAM // '/ IFs divided in half for 2 solutions'
            IF (IERR.NE.0) GO TO 190
         ELSE IF (DOEVLA.EQ.4) THEN
            HILINE = TSKNAM //
     *         '/ IFs divided in quarters for 4 solutions'
            END IF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,2018) TSKNAM, LBIF+1
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,2019) TSKNAM, LEIF+1
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                        SNR cutoff.
      WRITE (HILINE,2016) TSKNAM, SNRMIN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Point source model
      IF (ABS (SMODEL(1)).GT.0.0) THEN
         WRITE (HILINE,2020) TSKNAM, SMODEL(1), SMODEL(2), SMODEL(3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Other parameters
         WRITE (HILINE,2021) TSKNAM, SMODEL(4), SMODEL(5),
     *      SMODEL(6), SMODEL(7)
         IF (SMODEL(4).GT.0.01) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         END IF
      IF (IERR.NE.0) GO TO 190
C                                        Already divided by model
      WRITE (HILINE,2022) TSKNAM
      IF (APARM(2).GT.0.0) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Output SN table
      WRITE (HILINE,2062) TSKNAM, SNVER
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Full weight annulus
      WRITE (HILINE,2035) TSKNAM, MNPABL
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
      WRITE (HILINE,2036) TSKNAM, MXPABL
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
      WRITE (HILINE,2037) TSKNAM, WTPABL
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       No. baselines to search
      WRITE (HILINE,2040) TSKNAM, BLDO
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Delay window
      IF (DELWIN.LT.0.0) THEN
         WRITE (HILINE,3023) TSKNAM
      ELSE
         WRITE (HILINE,2041) TSKNAM, DELWIN
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Rate window.
      WRITE (HILINE,2042) TSKNAM, RATWIN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Integration time.
      WRITE (HILINE,2043) TSKNAM, DPARM(4)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        LS solution?
      ITEMP = 1
      IF (DODRLS) ITEMP = -1
      WRITE (HILINE,2044) TSKNAM, ITEMP
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Rereferenced?
      IF (DPARM(7).GT.1.0E-10) THEN
         ITEMP = 1
         WRITE (HILINE,2045) TSKNAM, ITEMP
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                        Antenna weights.
      ITEMP = 1
      LIMIT = MIN (ITEMP+8,NANT)
      WRITE (HILINE,2050) TSKNAM, (ANTWT(I), I = ITEMP,LIMIT)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
      ITEMP = ITEMP + 9
 160  IF (ITEMP.LE.NANT) THEN
         LIMIT = MIN (ITEMP+8,NANT)
         WRITE (HILINE,2051) TSKNAM, (ANTWT(I), I = ITEMP,LIMIT)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         ITEMP = ITEMP + 9
         GO TO 160
         END IF
C                                       Close HI file
 190   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables.
      IF ((SINGLE) .AND. (DOMODL) .AND. (DOAPLY.GE.0.0)) THEN
         CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISOUT,
     *      CNOIN, CNOOUT, CATBLK, BUFF1, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            WRITE (MSGTXT,1190)
            CALL MSGWRT (6)
            END IF
C                                        Update CATBLK.
          CALL CATIO ('UPDT', DISOUT, CNOOUT, CATBLK, 'REST', BUFF1,
     *       IERR)
          END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 1100 FORMAT ('FRNHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1190 FORMAT ('FRNHIS: ERROR COPYING TABLES')
 2001 FORMAT (A6,'INVER = ',I5,' /CC file version no.')
 2002 FORMAT (A6,'NMAPS =',I4,' /Number of clean images used')
 2003 FORMAT (A6,'NCOMP(',I3,') = ',I8,' /Number of clean comps.')
 2005 FORMAT (A6,'SOLSUB = ',I3,' /Number of sub-intervals')
 2006 FORMAT (A6,'SOLMIN = ',I3,' /Min number of sub-intervals')
 2010 FORMAT (A6,'SOLINT = ',F7.2,' /Soln. inter. (min)')
 2011 FORMAT (A6,'APARM(1) = ',I4,' /Min. no antennas')
 2012 FORMAT (A6,'REFANT = ',I4,' /Reference antenna')
 2013 FORMAT (A6,'APARM(3) = ',I4,' />0 => avg. RR,LL')
 2014 FORMAT (A6,'APARM(4) = ',I4,' />0 => avg. freq. in an IF')
 2015 FORMAT (A6,'APARM(5) = ',I4,' /<0 => soln. for each IF')
 3015 FORMAT (A6,'/ Did multi- and single band delay fits')
 2016 FORMAT (A6,'APARM(7)=',F5.1,' /SNR cutoff')
 2017 FORMAT (A6,'BCHAN, ECHAN, CHINC =',2(I5,','),I4)
 2018 FORMAT (A6,'BIF=',I3,6X,' / First IF when DOIFS=1')
 2019 FORMAT (A6,'EIF=',I3,6X,' / Last IF when DOIFS=1')
 2020 FORMAT (A6,'SMODEL = ',2(F10.5,','),F10.5,
     *   ' /Pt. model parameters')
 2021 FORMAT (A6,'        ',4F10.5,' / Other parms.')
 2022 FORMAT (A6,'APARM(2) = 1 /Data already divided by model')
 2035 FORMAT (A6,'UVRANGE(1)=',1PE12.5,' /Min. bl. full weight')
 2036 FORMAT (A6,'UVRANGE(2)=',1PE12.5,' /Max. bl. full weight')
 2037 FORMAT (A6,'WTUV =',1PE12.5,' /Weight outside annulus')
 2040 FORMAT (A6,'DPARM(1)=',I2,' /No. baseline combinations')
 2041 FORMAT (A6,'DPARM(2)=',F10.0,' /Delay win. (nsec)')
 2042 FORMAT (A6,'DPARM(3)=',F8.2,' /Rate win. (MHz)')
 2043 FORMAT (A6,'DPARM(4)=',F5.2,' /Input integ. time (sec)')
 2044 FORMAT (A6,'DPARM(5)=',I2,' /.gt.0 => no ls. soln.')
 2045 FORMAT (A6,'DPARM(7)=',I2,' /.gt.0 => do not rereference')
 2050 FORMAT (A6,'ANTWT=',9F5.1,' /Ant. wt')
 2051 FORMAT (A6,'      ',9F5.1)
 2062 FORMAT (A6,'SNVER =',I4,' / Output SN table version')
 3023 FORMAT (A6,'/ No delay search performed')
      END
      SUBROUTINE FRNADJ (IRET)
C-----------------------------------------------------------------------
C   FRNADJ massages the solutions so that interpolation between points
C   is reasonable.
C   Output:
C   IRET   I     Return error code. 0 => OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER KEYS(22)*24
      HOLLERITH CATUVH(256)
      INTEGER   ANT, REF, KOLS(22), LUN, LOOP, NKEY, LKEY, IFLOOP,
     *   KEY(2,2), KEYSUB(2,2), IPNT, IIVER, NIF, IERR, MXINDX, REFTMP,
     *   NUMSUB, LIMS1, LIMS2, LOOPSA, MBKOLS(14)
      LOGICAL   T, ISAPPL, DOIT, DOREF
      INTEGER   ISNRNO, MXCNT, NUMROW, NWORDS
      LONGINT   OFFSET
      REAL      WORK(1), SMOTIM(3), FKEY(2,2)
      LOGICAL   NOTREF
      INCLUDE 'FRING.INC'
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
C
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
     *   'MBDELAY2                ', 'DISP 2'/
C-----------------------------------------------------------------------
C                                       Initialize OFFSET; if OFFSET = 0
C                                       then workspace has not been
C                                       allocated
      OFFSET = 0
C                                       NOTREF indicates that some
C                                       antenna solutions were not
C                                       adjusted.
      NOTREF = .FALSE.
C                                       See if any work to be done.
      DOIT = ZRAT.OR.ZPHS.OR.ZDEL
      DOREF = DPARM(7).LE.0.0
      MXCNT = 0
      DO 5 LOOP = 1,NUMANT
         DOIT = DOIT .OR. ((REFUSE(LOOP).GT.0) .AND. (LOOP.NE.REFANT)
     *      .AND. (DOREF))
         IF (REFUSE(LOOP).GT.MXCNT) MXINDX = LOOP
         IF (REFUSE(LOOP).GT.MXCNT) MXCNT = REFUSE(LOOP)
 5       CONTINUE
      IF (.NOT.DOIT) GO TO 999
C                                       Message
      MSGTXT = 'Adjusting solutions to a common reference antenna'
      IF (DOREF) CALL MSGWRT (3)
C                                       If no REFANT specified pick the
C                                       one with the most solutions.
      REFTMP = REFANT
      IF (REFTMP.LE.0) REFTMP = MXINDX
      LUN = 29
C                                       Get IF frequencies
      FOFF(1) = 0.0
      IIVER = 1
      IF (EIF.GT.1)
     *   CALL CHNDAT ('READ', CLBUFF, DISKIN, CNOIN, IIVER, CATIN, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
C                                       Open solution table
      CALL SNINI ('READ', CLBUFF, DISKIN, CNOIN, SNVER, CATIN, LUN,
     *   ISNRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.GT.0) GO TO 990
C
      NUMROW = CLBUFF(5)
C                                       Set column pointers for sort
      CALL FNDCOL (NKEY, KEYS, LKEY, T, CLBUFF, KOLS, IRET)
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
      NWORDS = (5*NUMROW - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'FRNADJ', NWORDS, WORK, OFFSET, IRET)
      IF (IRET .NE. 0) THEN
         MSGTXT = 'Could not allocate enough workspace to reference'
         CALL MSGWRT (6)
         MSGTXT = 'solutions to a common antenna. Solutions will not'
         CALL MSGWRT (6)
         MSGTXT = 'be adjusted. If this is not acceptable then either'
         CALL MSGWRT (6)
         MSGTXT = 'increase SOLINT or shutdown some applications and'
         CALL MSGWRT (6)
         MSGTXT = 'run FRING again.'
         IRET = 0
         GO TO 999
         END IF
C                                       Sort to time-ant order.
      KEY(1,1) = KOLS(5)
      KEY(2,1) = KOLS(5)
      KEY(1,2) = KOLS(1)
      KEY(2,2) = KOLS(1)
      CALL TABSRT (DISKIN, CNOIN, 'SN', SNVER, SNVER, KEY, KEYSUB, FKEY,
     *   CLBUFF, CATIN, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 980
         END IF
C                                       Find number of subarrays
      CALL FNDEXT ('AN', CATUVH, NUMSUB)
      LIMS1 = 1
      LIMS2 = NUMSUB
      IF (SUBARR.GT.0) LIMS1 = SUBARR
      IF (SUBARR.GT.0) LIMS2 = SUBARR
C                                       Open for write
      CALL SNINI ('WRIT', CLBUFF, DISKIN, CNOIN, SNVER, CATIN, LUN,
     *   ISNRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.GT.0) GO TO 990
C                                       Set column pointers
      DO 40 LOOP = 1,11
         IPNT = KOLS(LOOP)
         KOLS(LOOP) = CLKOLS(IPNT)
 40      CONTINUE
C                                       Set MBD kol pointers
      CALL COPY (5, KOLS, MBKOLS)
      MBKOLS(6) = KOLS(10)
      MBKOLS(7) = KOLS(11)
C
      IF (NUMPOL.GT.1) THEN
C                                       Second Stokes'
         CALL FNDCOL (NKEY, KEYS(12), LKEY, T, CLBUFF, KOLS(12), IRET)
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
      IF (DOREF) THEN
         DO 110 LOOPSA = LIMS1,LIMS2
C                                       Loop over reference antennas
C                                       used.
            REF = REFTMP
            DO 100 LOOP = 1,MAXANT
               IF ((REFUSS(LOOP,LOOPSA).GT.0) .AND. (LOOP.NE.REF)) THEN
                  ANT = LOOP
C                                       First Stokes'
                  CALL MBDREF (ANT, REF, LOOPSA, MBKOLS(1), CLBUFF,
     *               SMOTIM, NUMROW, WORK(OFFSET+1),
     *               WORK(OFFSET+NUMROW+1), WORK(OFFSET+2*NUMROW+1),
     *               WORK(OFFSET+3*NUMROW+1), IRET)
                  IF (IRET.GT.0) GO TO 990
C                                       Second Stokes'
                  IF (NUMPOL.GT.1) THEN
                     CALL MBDREF (ANT, REF, LOOPSA, MBKOLS(8), CLBUFF,
     *                  SMOTIM, NUMROW, WORK(OFFSET+1),
     *                  WORK(OFFSET+NUMROW+1), WORK(OFFSET+2*NUMROW+1),
     *                  WORK(OFFSET+3*NUMROW+1), IRET)
                     IF (IRET.GT.0) GO TO 990
                     END IF
                  END IF
 100           CONTINUE
 110        CONTINUE
         END IF
C                                       Now re-reference IF dependent
C                                       values
C                                       Loop over IFs
      DO 200 IFLOOP = 1,NUMIF
         FREQIF = FREQ + FOFF(IFLOOP-BIF+1)
C                                       Loop over subarrays
         IF (DOREF) THEN
            DO 160 LOOPSA = LIMS1,LIMS2
C                                       Loop over reference antennas
C                                       used.
               REF = REFTMP
               DO 150 LOOP = 1,NUMANT
                  IF ((REFUSS(LOOP,LOOPSA).GT.0) .AND. (LOOP.NE.REF))
     *               THEN
                     ANT = LOOP
C                                       First Stokes'
                     CALL CALREF (ANT, REF, LOOPSA, KOLS(1), FREQIF,
     *                  SMOTIM, NUMROW, CLBUFF, WORK(OFFSET+1),
     *                  WORK(OFFSET+NUMROW+1), WORK(OFFSET+2*NUMROW+1),
     *                  WORK(OFFSET+3*NUMROW+1),
     *                  WORK(OFFSET+4*NUMROW+1), IRET)
C                                       Inability to connect ANT to
C                                       REF is not necessarily an
C                                       error
                     IF (IRET.EQ.1) THEN
                        IRET = 0
                        NOTREF = .TRUE.
                        END IF
                     IF (IRET.GT.0) GO TO 990
C                                       Second Stokes'
                     IF (NUMPOL.GT.1) THEN
                        CALL CALREF (ANT, REF, LOOPSA, KOLS(12), FREQIF,
     *                     SMOTIM, NUMROW, CLBUFF, WORK(OFFSET+1),
     *                     WORK(OFFSET+NUMROW+1),
     *                     WORK(OFFSET+2*NUMROW+1),
     *                     WORK(OFFSET+3*NUMROW+1),
     *                     WORK(OFFSET+4*NUMROW+1), IRET)
C                                       Inability to connect ANT to
C                                       REF is not necessarily an
C                                       error
                        IF (IRET.EQ.1) THEN
                           IRET = 0
                           NOTREF = .TRUE.
                           END IF
                        IF (IRET.GT.0) GO TO 990
                        END IF
                     END IF
 150              CONTINUE
 160           CONTINUE
            END IF
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
 980  CALL MSGWRT (8)
 990  WRITE (MSGTXT,1990)
      CALL MSGWRT (8)
C
 999  CONTINUE
C                                       De-allocate any dynamic
C                                       workspace. Ignore errors.
      IF (OFFSET.NE.0) CALL ZMEMRY ('FREE', 'FRNADJ', 5 * NUMROW, WORK,
     *   OFFSET, IERR)
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR ',I5,' FINDING SN TABLE COLUMNS')
 1010 FORMAT ('TABIO ERROR ',I5,' CLOSING SN TABLE')
 1020 FORMAT ('TABSRT ERROR ',I5,' SORTING SN TABLE')
 1990 FORMAT ('ERROR OCCURED IN FRNADJ')
      END
      SUBROUTINE FRNAPL (IRET)
C-----------------------------------------------------------------------
C   FRNAPL corrects single source data files with optional averaging
C   over the frequency axis.
C   Input from common:
C      DISOUT I     Output disk number.
C      CNOOUT I     Output catalog slot number.
C      DAVOUT L     If true, average frequencies in IF
C      INCF   I     Increment in freq. of data from UVGET
C      INCIF  I     Increment in IF of data from UVGET
C      INCS   I     Increment in Stokes' of data from UVGET
C      JLOCF  I     Offset of freq. of data from UVGET
C      JLOCIF I     Offset of IF of data from UVGET
C      JLOCS  I     Offset of Stokes' of data from UVGET
C   Output:
C      IRET   I     Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER NAME*48
      HOLLERITH CATH(256), CATUVH(256)
      INTEGER   LUN, FIND, BIND, LENBU, NIO, LOOPS, LOOPIF, LOOPF,
     *   JNCIF, JNCS, LRECO, INP, NUMFRQ, NUMSUB, SUB, LIMS1, LIMS2,
     *   SUBTMP, LUN1, LUN2, NUMPRM
      LOGICAL   T, F, DOCMP
      INTEGER   BO, I, XCOUNT, INDEX, OUTDEX, IIVER, OOVER, NUMVIS,
     *   OLDVIS, TOTREC(2,3), JERR, MMVIS, MMCOR, WTOFF, NUMSTK, NUMBIF,
     *   RNXRET
      REAL      DUM, SUMWT, SUMRE, SUMIM, XNORM, WT, CATR(256), MLTINC
      INCLUDE 'FRING.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      REAL    VIS(XBFSZ), VIS2(XBFSZ)
      DOUBLE PRECISION FOFF(MAXIF)
      EQUIVALENCE (CATBLK, CATH, CATR),  (CATUV, CATUVH)
      EQUIVALENCE (BUFF2, VIS)
      EQUIVALENCE (BUFFS, VIS2)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN, LUN1, LUN2, BO /27,27,28,1/
C-----------------------------------------------------------------------
C                                       Message
      WRITE (MSGTXT,2000)
      CALL MSGWRT (6)
      DOCAL = T
      CLUSE = SNVER
      STOKES = '    '
C                                       Zero flag counts
      TOTREC(1,1) = 0
      TOTREC(2,1) = 0
      TOTREC(1,2) = 0
      TOTREC(2,2) = 0
      TOTREC(1,3) = 0
      TOTREC(2,3) = 0
C                                       Find number of subarrays
      CALL FNDEXT ('AN', CATUVH, NUMSUB)
      LIMS1 = 1
      LIMS2 = NUMSUB
      IF (SUBARR.GT.0) LIMS1 = SUBARR
      IF (SUBARR.GT.0) LIMS2 = SUBARR
      SUBTMP = SUBARR
      NUMVIS = 0
C                                       Get output CATBLK from DISK
      MSGSUP = 32000
      CALL CATIO ('READ', DISOUT, CNOOUT, CATBLK, 'REST', BUFF1, IRET)
      MSGSUP = 0
      IF ((IRET.GT.0) .AND. (IRET.LT.5)) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Get info about visibility
C                                       structure.
      CALL UVPGET (JERR)
      NUMFRQ = CATBLK(KINAX+JLOCF)
      DAVOUT = DAVOUT .AND. (NUMFRQ.GT.1)
C                                       If averaging, reset freq. axis
C                                       length (the file was originally
C                                       created correctly and then the
C                                       no. freq. was reset to the
C                                       original value.)
      IF (DAVOUT) THEN
         MLTINC = NUMFRQ
         CATR(KRCIC+JLOCF) = CATR(KRCIC+JLOCF) * MLTINC
         CATR(KRCRP+JLOCF) = (CATR(KRCRP+JLOCF) - ((NUMFRQ+1.0)/2.0)) /
     *      NUMFRQ + 1.0
         CATBLK(KINAX+JLOCF) = 1
         CALL UVPGET (JERR)
      ELSE
         MLTINC = 1.0
         END IF
C                                       Set lengths of input axes.
      OLDVIS = CATBLK(KIGCN)
      NUMBIF = 1
      IF (JLOCIF.GT.0) NUMBIF = CATBLK(KINAX+JLOCIF)
      NUMSTK = CATBLK(KINAX+JLOCS)
C                                       Set output increments
C                                       (averaging)
      JNCIF = INCIF
      JNCS = INCS
C                                       Compressed data?
      DOCMP = CATBLK(KINAX) .EQ. 1
      NUMPRM = NRPARM
      IF (DOCMP) THEN
         MMVIS = (LREC - NUMPRM) * 3
         MMCOR = LREC - NUMPRM
         JNCS = JNCS * 3
         JNCIF = JNCIF * 3
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), WTOFF,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            IRET = 9
            GO TO 990
            END IF
      ELSE
C                                       Un compressed
         MMVIS = LREC - NUMPRM
         MMCOR = (LREC - NUMPRM) / 3
         WTOFF = 0
         END IF
C                                       Set output file name
      CALL ZPHFIL ('UV', DISOUT, CNOOUT, 1, NAME, IRET)
C                                       Open output file.
      CALL ZOPEN (LUN, FIND, DISOUT, NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      LENBU = 1
      LRECO = LREC
      CALL UVINIT ('WRIT', LUN, FIND, CATBLK(KIGCN), NUMVIS, LRECO,
     *   LENBU, JBUFSZ, BUFF1, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
C                                       Loop over subarray
      XCOUNT = 0
      CALL RNXGET (DISKIN, CNOIN, CATIN)
      CALL RNXINI (DISOUT, CNOOUT, CATBLK, RNXRET)
      DO 200 SUB = LIMS1,LIMS2
         SUBARR = SUB
C                                       Setup
C                                       Save CATBLK
         CALL COPY (256, CATBLK, BUFF2)
         CALL UVGET ('INIT', DUM, DUM, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Restore CATBLK
         CALL COPY (256, BUFF2, CATBLK)
         IF (IRET.LT.0) GO TO 120
         IF (NVIS.LE.0) GO TO 120
C                                       Check buffer size for use of
C                                       VIS, VIS2
         IF ((LREC.GT.XBFSZ) .AND. (DAVOUT.OR.DOCMP)) THEN
            MSGTXT = 'FRNAPL: BUFFER SIZE TOO SMALL FOR VISIBILITY ' //
     *         ' RECORD'
            IRET = 1
            GO TO 990
            END IF
C                                       If multiple subarrays mark
C                                       unsorted
         IF (NUMVIS.GT.0) CALL CHR2H (2, '**', 1, CATH(KITYP))
C                                       Copy file
         DO 100 I = 1,NVIS
            IF (DAVOUT) THEN
C                                       Average in freq in each IF
               CALL UVGET ('READ', BUFF1(BIND), VIS, IRET)
               IF (IRET.LT.0) GO TO 120
               IF (IRET.NE.0) GO TO 999
C                                       Average.
               CALL RFILL (MMVIS, 0.0, VIS2)
               DO 80 LOOPS = 1,NUMSTK
                  DO 70 LOOPIF = 1,NUMBIF
                     SUMWT = 0.0
                     SUMRE = 0.0
                     SUMIM = 0.0
                     INDEX = 1 + (LOOPS-1) * INCS + (LOOPIF-1) * INCIF
                     OUTDEX = 1 + (LOOPS-1) * JNCS + (LOOPIF-1) * JNCIF
                     DO 60 LOOPF = 1,NUMFRQ
                        INP = INDEX + (LOOPF-1) * INCF
                        WT = VIS(INP+2)
                        IF (WT.LE.0.0) WT = 0.0
                        SUMRE = SUMRE + VIS(INP) * WT
                        SUMIM = SUMIM + VIS(INP+1) * WT
                        SUMWT = SUMWT + WT
 60                     CONTINUE
                     XNORM = 1.0
                     IF (SUMWT.GT.1.0E-10) XNORM = 1.0 / SUMWT
                     VIS2(OUTDEX) = SUMRE * XNORM
                     VIS2(OUTDEX+1) = SUMIM * XNORM
                     VIS2(OUTDEX+2) = SUMWT
 70                  CONTINUE
 80               CONTINUE
C                                       Copy/compress to output buffer
               IF (DOCMP) THEN
                  CALL ZUVPAK (MMCOR, VIS2, BUFF1(BIND+WTOFF),
     *               BUFF1(BIND+NUMPRM))
               ELSE
                  CALL RCOPY (MMVIS, VIS2, BUFF1(BIND+NUMPRM))
                  END IF
C                                       No averaging:
            ELSE
C                                       Write/compress to output buffer
               IF (DOCMP) THEN
                  CALL UVGET ('READ', BUFF1(BIND), VIS2, IRET)
                  CALL ZUVPAK (MMCOR, VIS2, BUFF1(BIND+WTOFF),
     *               BUFF1(BIND+NUMPRM))
               ELSE
                  CALL UVGET ('READ', BUFF1(BIND), BUFF1(BIND+NUMPRM),
     *               IRET)
                  END IF
               IF (IRET.LT.0) GO TO 120
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       Write new
C                                       update NX table
            CALL RNXUPD (BUFF1(BIND), RNXRET)
            NIO = 1
            XCOUNT = XCOUNT + 1
            CALL UVDISK ('WRIT', LUN, FIND, BUFF1, NIO, BIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1090) IRET
               GO TO 990
               END IF
 100           CONTINUE
C                                       Close Input file
 120     CALL UVGET ('CLOS', DUM, DUM, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Sum flag counts
         TOTREC(1,1) = TOTREC(1,1) + CNTREC(1,1)
         TOTREC(2,1) = TOTREC(2,1) + CNTREC(2,1)
         TOTREC(1,2) = TOTREC(1,2) + CNTREC(1,2)
         TOTREC(2,2) = TOTREC(2,2) + CNTREC(2,2)
         TOTREC(1,3) = TOTREC(1,3) + CNTREC(1,3)
         TOTREC(2,3) = TOTREC(2,3) + CNTREC(2,3)
 200     CONTINUE
      SUBARR = SUBTMP
C                                       Flush output
      NIO = 0
      CALL UVDISK ('FLSH', LUN, FIND, BUFF1, NIO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1090) IRET
         GO TO 990
         END IF
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISOUT, CNOOUT, LUN, CATBLK, IRET)
C                                      Put vis. count in CATBLK
      CATBLK(KIGCN) = NVIS
C                                       Copy relevant portion of IF
C                                       table.
      IIVER = 1
      OOVER = 1
      CALL CHNCPY (IIVER, OOVER, LUN1, LUN2, DISKIN, DISOUT, CNOIN,
     *   CNOOUT, CATIN, CATBLK, BIF, EIF, FRQSEL, SFREQS, MLTINC,
     *   IBUFF1, FOFF, ISBAND, FINC, IRET)
      IRET = 0
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISOUT, CNOOUT, CATBLK, 'REST', BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1230) IRET
         CALL MSGWRT (6)
         END IF
C                                       Close output
      CALL ZCLOSE (LUN, FIND, IRET)
C                                       Give data summary
      WRITE (MSGTXT,2800)
      CALL MSGWRT (6)
      WRITE (MSGTXT,2801) TOTREC(1,1), TOTREC(1,2), TOTREC(1,3)
      CALL MSGWRT (6)
      WRITE (MSGTXT,2802) TOTREC(2,1), TOTREC(2,2), TOTREC(2,3)
      CALL MSGWRT (6)
      IF (NVIS.GT.0) GO TO 999
C                                       No data found.
         IRET = 9
         WRITE (MSGTXT,1800)
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FRNAPL: ERROR',I5,' READING OUTPUT CATBLK')
 1020 FORMAT ('FRNAPL: ERROR',I5,' OPENING OUTPUT FILE')
 1030 FORMAT ('FRNAPL: ERROR',I5,' INIT. OUTPUT FILE')
 1090 FORMAT ('FRNAPL: ERROR',I5,' WRITING OUTPUT FILE')
 1230 FORMAT ('FRNAPL: ERROR',I3,' UPDATING CATALOG HEADER')
 1800 FORMAT ('FRNAPL: ERROR - NO DATA WRITTEN')
 2000 FORMAT ('Applying solutions to data')
 2800 FORMAT (10X,' Previously flagged ','  Flagged by gain   ',
     *   '      Kept')
 2801 FORMAT ('Partially ',2I20,I10)
 2802 FORMAT ('Fully     ',2I20,I10)
      END
      SUBROUTINE CHNCPY (INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD,
     *   VOLNEW, CNOOLD, CNONEW, CATOLD, CATNEW, BIF, EIF, FREQID,
     *   SFOFF, MLTINC, BUFF1, FOFF, ISBAND, FINC, IRET)
C-----------------------------------------------------------------------
C   CHNCPY copies selected portions of a Channel (IF) table extension
C   file.   Local version with channel averaging
C   Inputs:
C      LUNOLD   I        LUN for old file
C      LUNNEW   I        LUN for new file
C      VOLOLD   I        Disk number for old file.
C      VOLNEW   I        Disk number for new file.
C      CNOOLD   I        Catalog slot number for old file
C      CNONEW   I        Catalog slot number for new file
C      BIF      I        First IF to copy to output.
C      EIF      I        Last IF to copy.
C      FREQID   I        FREQ ID to copy.
C                        If an FQ table exists and FREQID .le. 0 then
C                        if there is only one row in the FQ table, that
C                        row is copied; if there are multiple rows an
C                        error message is returned from CHNDAT.
C      CATOLD   I(256)   Catalog header for old file.
C      SFOFF    D(*)     Frequency offsets to be added to FQ table
C                        offsets in the output FQ table.  These are
C                        usually those of the 1 source in the output
C                        file taken from the SU table in the input.
C      MLTINC   R        Scale the FINC values
C   In/out:
C      INVER    I        Version number to copy, 0 => copy all.
C      OUTVER   I        Version number on output file, if more than one
C                        copied (INVER=0) this will be the number of the
C                        first file.  If OUTVER = 0, it will be taken as
C                        1 higher than the previous highest version.
C      CATNEW   I(256)   Catalog header for new file.
C   Output:
C      BUFF1    I(512)   Work buffer
C      FOFF     D(*)     Frequency table.  Must be >= MAXIF
C      ISBAND   I(*)     Sideband table.  Must be >= MAXIF
C      FINC     R(*)     Channel increment table  Must be >= MAXIF
C      IRET     I        Return error code  0 => ok
C                           6 => asked for too many IFs.
C                           other = CHNDAT error.
C-----------------------------------------------------------------------
      INTEGER   INVER, OUTVER, LUNOLD, LUNNEW, VOLOLD, VOLNEW,
     *   CNOOLD, CNONEW, BIF, EIF, BUFF1(*), ISBAND(*), CATNEW(256),
     *   CATOLD(256), FREQID, IRET
      DOUBLE PRECISION SFOFF(*), FOFF(*)
      REAL      FINC(*), MLTINC
      INTEGER   NIF, IEIF, IBIF
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Read all IFs from old
C                                       CH/FQ table
      CALL CHNDAT ('READ', BUFF1, VOLOLD, CNOOLD, INVER, CATOLD, LUNOLD,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FREQID, IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                       Check number
 30   IBIF = MAX (BIF, 1)
      IEIF = MAX (EIF, IBIF)
      IF (NIF.LT.IEIF) THEN
         IRET = 6
         WRITE (MSGTXT,1030) IEIF, NIF
         GO TO 990
         END IF
C                                       Offset
      IF (MLTINC.EQ.0.0) MLTINC = 1.0
      DO 40 NIF = IBIF,IEIF
         FOFF(NIF) = FOFF(NIF) + SFOFF(NIF)
         FINC(NIF) = FINC(NIF) * MLTINC
 40      CONTINUE
C                                       Number of output IFs.
      NIF = IEIF - IBIF + 1
C                                       Write new FQ table for
C                                       selected IFs
      CALL CHNDAT ('WRIT', BUFF1, VOLNEW, CNONEW, OUTVER, CATNEW,
     *   LUNNEW, NIF, FOFF(IBIF), ISBAND(IBIF), FINC(IBIF),
     *   BNDCOD(IBIF), FREQID, IRET)
      IF (IRET.EQ.0) GO TO 50
         WRITE (MSGTXT,1040) IRET
         GO TO 990
 50   IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CHNCPY: ERROR',I5,' READING OLD IF TABLE')
 1030 FORMAT ('CHNCPY: REQUESTED TOO MANY IFS : ',I4,'>',I4)
 1040 FORMAT ('CHNCPY: ERROR',I5,' WRITING NEW IF TABLE')
      END
      SUBROUTINE FRNSOL (MAXBL, MAXTIM, MAXFRQ, IRET)
C-----------------------------------------------------------------------
C   FRNSOL calls FRNFIT.  Its only purpose is to declare various arrays
C   outside of the main routine, for the benefit of
C   machines that require overlaying.  See FRNFIT for details.
C-----------------------------------------------------------------------
      INTEGER   MAXBL, MAXFRQ, MAXTIM, IRET
C
      INTEGER   MAXIFS, SOLSUB, SOLMIN
      INCLUDE 'FRING.INC'
      INCLUDE 'INCS:DGDS.INC'
C                                       dynamic array declarations
      REAL      TIMB(2), VREAL(2), VIMAG(2), CMBDEL(2*MAXANT), CREAL(2),
     *   CIMAG(2), CDELY(2), CRATE(2), CWT(2), WTT(2), TIME(2),
     *   CDISP(2*MAXANT)
      INTEGER   NWD, JERR, NPLAC(2), NPL(2), MCOR, LAXTIM
      LONGINT   OVREAL, OVIMAG, ONPLAC, OTIMB, OWTT, OTIME, ONPL,
     *   OCREAL, OCIMAG, OCDELY, OCRATE, OCWT
      LOGICAL   ISIQUV, DOAVER
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (CATBLK, CATD)
C-----------------------------------------------------------------------
      MAXIFS = 1
      IF (JLOCIF.GT.0) MAXIFS = CATBLK(KINAX+JLOCIF)
C                                       sub interval parameters
      SOLMIN = XSOLM + 0.1
      SOLSUB = XSOLS + 0.1
      IF (SOLSUB.LE.0) SOLSUB = 1
      IF (SOLSUB.GT.10) SOLSUB = 10
      IF ((SOLMIN.LE.0) .OR. (SOLMIN.GT.SOLSUB)) SOLMIN = SOLSUB
      DOAVER = (SOLSUB.EQ.1) .AND. (NORAT)
      LAXTIM = MAXTIM
      IF (DOAVER) LAXTIM = 2
C                                       Number correlators
      MCOR = 1
      IF (NCOR.GE.2) MCOR = 2
      IF (AVGPOL) MCOR = 1
      ISIQUV = CATD(KDCRV+JLOCS).GT.0.0
      IF (ISIQUV) MCOR = 1
C                                       allocate memory
      NWD = (LAXTIM + 2) * MAXFRQ * MAXBL * MCOR
      NWD = ((NWD - 1) / 256 + 1) * 256
      NWD = (NWD - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'FRNSOL', NWD, VREAL, OVREAL, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FRNSOL', NWD, VIMAG, OVIMAG,
     *   IRET)
      NWD = (LAXTIM + 2) * MAXFRQ * MAXBL
      NWD = (NWD - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FRNSOL', NWD, NPLAC, ONPLAC,
     *   IRET)
      NWD = (LAXTIM + 2) * MAXBL
      NWD = ((NWD - 1) / 256 + 1) * 256
      NWD = (NWD - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FRNSOL', NWD, TIMB, OTIMB,
     *   IRET)
      NWD = LAXTIM + 2
      NWD = ((NWD - 1) / 256 + 1) * 256
      NWD = (NWD - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FRNSOL', NWD, WTT, OWTT,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FRNSOL', NWD, TIME, OTIME,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FRNSOL', NWD, NPL, ONPL,
     *   IRET)
      NWD = 2 * MAXIFS * NANT
      NWD = ((NWD - 1) / 256 + 1) * 256
      NWD = (NWD - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FRNSOL', NWD, CREAL, OCREAL,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FRNSOL', NWD, CIMAG, OCIMAG,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FRNSOL', NWD, CDELY, OCDELY,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FRNSOL', NWD, CRATE, OCRATE,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'FRNSOL', NWD, CWT, OCWT,
     *   IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'FRNSOL: UNABLE TO GET NEEDED MEMORY, reduce SOLINT'
         CALL MSGWRT (8)
         MSGTXT = '        OR USE A BIGGER OR LESS BUSY COMPUTER'
         CALL MSGWRT (8)
      ELSE
         CALL FRNFIT (VREAL(1+OVREAL), VIMAG(1+OVIMAG), TIMB(1+OTIMB),
     *      CMBDEL, CDISP, CREAL(1+OCREAL), CIMAG(1+OCIMAG),
     *      CDELY(1+OCDELY), CRATE(1+OCRATE), CWT(1+OCWT), WTT(1+OWTT),
     *      TIME(1+OTIME), NPL(1+ONPL), NPLAC(1+ONPLAC), MAXBL, LAXTIM,
     *      MAXFRQ, MAXIFS, MCOR, SOLSUB, SOLMIN, IRET)
         END IF
C                                       clear allocations
      CALL ZMEMRY ('FRAL', 'FRNSOL', NWD, VREAL, OVREAL, JERR)
C
 999  RETURN
      END
      SUBROUTINE FRNFIT (VREAL, VIMAG, TIMB, CMBDEL, CDISP, CREAL,
     *   CIMAG, CDELY, CRATE, CWT, WTT, TIME, NPLACE, NNPLAC, MAXBL,
     *   MAXTIM, MAXFRQ, MAXIFS, MCOR, SOLSUB, SOLMIN, IERR)
C-----------------------------------------------------------------------
C   FRNFIT reads thru a data file which has been divided by the model
C   and makes the requested solutions which are written into a solution
C   (SN) table.
C   Input:
C    VREAL(MAXTIM,MAXFRQ,MAXBL)     R    Work array.
C    VIMAG(MAXTIM,MAXFRQ,MAXBL)     R    Work array.
C    CMBDEL(2,NUMANT)               R    Work array
C    CDISP(2,NUMANT)               R    Work array
C    CREAL(2,MAXIFS,NUMANT)         R    Work array.
C    CIMAG(2,MAXIFS,NUMANT)         R    Work array.
C    CDELY(2,MAXIFS,NUMANT)         R    Work array.
C    CRATE(2,MAXIFS,NUMANT)         R    Work array.
C    CWT(2,MAXIFS,NUMANT)           R    Work array.
C    TIMB(MAXTIM,MAXBL)             R    Big array to hold times.
C    NPLACE(MAXTIM)                 I    Work array.
C    NNPLAC(MAXTIM,MAXFRQ,MAXBL)    I    Work array.
C      MAXBL    I      Max. number of baselines in data.
C      MAXTIM   I      Maximum number of time integrations.
C      MAXFRQ   I      Maximum number of frequency channels.
C      SOLSUB   I      Number of sub-intervals to use
C      SOLMIN   I      Minimum number subintervals for solution
C   From common:
C    SOLINT        R    Solution interval (days).
C    TINTG         R    Integration time (sec)
C    DELWIN        R    Delay window (nsec)
C    RATWIN        R    Rate window (mHz)
C    REFANT        I    Ref ant to use.
C    DODRLS        L    True if least squares solution wanted.
C    AVGPOL        L    True if RR and LL to be averaged
C    AVGIF         L    True if all frequencies in each IF to be
C                       averaged
C    DOIF          L    True then do "fringe fit" independently in each
C                       IF.
C    DOMS          L    If TRUE then fit for both multi- and single band
C                       delays.
C    NUMBL         I    Number of baselines
C    NUMTIM        I    Number of time intervals
C    NUMIF         I    Number of IFs
C    SNRMIN        R    Minimum acceptable SNR
C    PRTLV         I    Print level
C    BLDO          I    the number of baseline combinations to try for
C                       coarse fringe search.
C    MINNO         I    Min. no. antennas.
C    ANTWT(20)     R    Antenna weights.
C    CATBLK(256)   I    Output catalog header.
C    CATIN(256)    I    Input catalog header.
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
      INTEGER   MAXBL, MAXFRQ, MAXTIM, MAXIFS, MCOR, SOLSUB, SOLMIN
      INTEGER   NNPLAC(MAXTIM,MAXFRQ,MAXBL), NPLACE(MAXTIM), IERR,
     *   IRET
      REAL      CDISP(2,*), VREAL(MAXTIM,MAXFRQ,MAXBL,MCOR),
     *   VIMAG(MAXTIM,MAXFRQ,MAXBL,MCOR), TIMB(MAXTIM,MAXBL),
     *   CMBDEL(2,*), CREAL(2,MAXIFS,*), CIMAG(2,MAXIFS,*),
     *   CDELY(2,MAXIFS,*), CRATE(2,MAXIFS,*), CWT(2,MAXIFS,*),
     *   WTT(MAXTIM), TIME(MAXTIM)
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER KEYWRD*8
      INTEGER   LUNI, LUNSS, FINDI, BINDI, J, IJK, NIN, NBL, NTIM, IC,
     *   IITEMP, NBLANK, I, IBL, ICOR, DISK, INDEX, JBL, KK1, KK3,
     *   INCIII, II, NANTM1, I1P1, I1, I2, IDAY, KDAY, KHR, KMN, KSEC,
     *   IROUND, SCNSOU, SCNSUB, SUBA, INTNO, NUMINT, NFPIF, NUMFRQ,
     *   INCJJJ, NOIF, MFRQ, MIF, SNKOLS(MAXSNC), ISUB, MSUB, TNTIM,
     *   SNNUMV(MAXSNC), NODENO, IM1, JFRQ, JIF, NXVER, FREQID, TVER,
     *   NNSOU, BO, VO, ISNRNO, IDUM1, IDUM2, VCNO, CNTOK, KFRQ, KF,
     *   CNTBAD, TMPNIF, TMPNFQ, NDXSOU, ONESOU, KEYLOC, KEYTYP,
     *   ORIGIN, NUMKEY, PRIINI(1+MAXANT), NMSG, BLCODE(MXBASE), LWT,
     *   IS(MXBASE), JS(MXBASE), REFAN(2,MAXIF), ISU, ITICK, NTICK,
     *   NLEFT, NRIGHT, BTCODE(MXBASE), PRMAX, NTIMES(10), MTIM, TOFF,
     *   DROUND, IBASE
      REAL      DELT, CATR(256), WTF(MAXCIF), WT, AMP, TIMEX, XINC,
     *   CATIR(256), SCNTIM, SCNDT, BASEL, MX2BAS, MN2BAS, BLFACT, IFRM,
     *   TAU(MXBASE,10), WTB(MXBASE,2,10), WGTMOD(MXBASE,10), WTFACT
      CHARACTER IFILE*48, BNDCOD(MAXIF)*8
      LOGICAL   T, F, JUSRED, WARN, DONDX, ISAPPL, ISIQUV, WANSRC,
     *   ADDANT, GOTANT(MAXANT,10), EXPAND, DOAVER
      DOUBLE PRECISION X8, TIMEC, FREQIF(MAXIF), CATD(128), TIMRA(2),
     *   FREQS(MAXCIF), SUM, LASTIM, TIMNOM, STTIME, CURTIM, CUREND,
     *   ENDTIM, DEPS, SIUSE, TINTGH, CURSUB, SCNEND, SCNBEG, SCNINT
      INCLUDE 'FRING.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATIN, CATIR), (CATBLK, CATR, CATD)
      DATA ISAPPL /.FALSE./
      DATA KEYWRD /'SNORIGIN'/
      DATA T,F /.TRUE.,.FALSE./
      DATA LUNI, LUNSS /16, 27/
      DATA BO, VO /1,0/
C-----------------------------------------------------------------------
C                                       0.001 sec
      DEPS = 1.157407D-8
      LWT = XWTIT + 0.1
      DOAVER = (SOLSUB.LE.1) .AND. (NORAT)
C                                       Message(s)
      MSGTXT = 'Determining solutions'
      CALL MSGWRT (6)
      IF (DOMS) THEN
         MSGTXT = 'Doing Least Squares fits for multi- and single' //
     *      '-band delays'
         CALL MSGWRT (6)
         END IF
      CNTOK = 0
      CNTBAD = 0
      NMSG = 0
C                                       Square baseline limits
      MX2BAS = MXPABL * MXPABL * 1.0E6
      MN2BAS = MNPABL * MNPABL * 1.0E6
      NOIF = EIF - BIF + 1
      TINTGH = TINTG * 0.5
C                                       Increment if averaging in
C                                       frequency
      INCIII = 1
      INCJJJ = (ECHAN - BCHAN) / CHINC + 1
      IF (AVGIF) THEN
         INCIII = 0
         INCJJJ = 1
         END IF
      WARN = F
      NUMBL = (NUMANT * (NUMANT-1)) / 2
C                                       If only one source selected get
C                                       it's number
      IF (NSOUWD.EQ.1) THEN
         ONESOU = SOUWAN(1)
      ELSE
         ONESOU = 0
         END IF
C                                       Purge old SN table?
      IF (SNVER.GT.0) THEN
         NNSOU = NSOUWD
         IF (SINGLE) NNSOU = 0
         TIMRA(1) = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0)
     *      + (XTIME(4) / (24.0*60.0*60.0))
         TIMRA(2) = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0)
     *      + (XTIME(8) / (24.0*60.0*60.0))
         IF ((TIMRA(2).LT.TIMRA(1)) .OR. (TIMRA(2).LT.1.0E-5))
     *      TIMRA(2) = 1.0E20
         CALL CALSEL (DISKIN, CNOIN, 'SN', SNVER, CATIN, CLBUFF, NNSOU,
     *      SOUWAN, DOSWNT, NANTSL, ANTENS, DOAWNT, TIMRA, SUBARR,
     *      FRQSEL, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                        Init. Gain. file.
      NUMNOD = 0
      NODENO = 0
      NUMPOL = 1
      GMMOD = 1.0
      IF (NCOR.GT.1) NUMPOL = 2
C                                        IQUV?
      ISIQUV = CATD(KDCRV+JLOCS).GT.0.0
      IF (ISIQUV) NUMPOL = 1
      CALL CATFIX (DISKIN, CNOIN, '    ')
      CALL SNINI ('WRIT', CLBUFF, DISKIN, CNOIN, SNVER, CATIN, LUNSS,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       try it here! - ketan
      KEYLOC = 1
      KEYTYP = 4
      ORIGIN = 0
      NUMKEY = 1
      IF (SINGLE) ORIGIN = 1
      CALL TABKEY ('WRIT', KEYWRD, NUMKEY, CLBUFF, KEYLOC, ORIGIN,
     *   KEYTYP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1150) IERR
         GO TO 990
         END IF
C                                       Close - will be opened when
C                                       written
      CALL TABIO ('CLOS', 0, ISNRNO, TIMEC, CLBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGTXT,1140) SNVER
      CALL MSGWRT (4)
      IFRM = 0.0
C                                        Open vis. file
      DISK = VISDSK
      VCNO = VISCNO
      IF (VISDSK.EQ.0) THEN
         DISK = SCRVOL(VISCNO)
         VCNO = SCRCNO(VISCNO)
         CALL ZPHFIL ('SC', DISK, VCNO, 1, IFILE, IRET)
      ELSE
         CALL ZPHFIL ('UV', DISK, VCNO, 1, IFILE, IRET)
         END IF
      CALL ZOPEN (LUNI, FINDI, DISK, IFILE, T, F, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1005) IERR
         GO TO 990
         END IF
C                                        First initialize.
      NIN = 1
      VO = 0
      CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LREC, NIN, JBUFSZ,
     *   BUFF1, BO, BINDI, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Setup.
C                                       Stokes' parameter
      ICOR0 = ABS (ICOR0)
      INCS = ABS (INCS)
C                                        Clear "got data" flag
      JUSRED = F
      CALL COPY (1+MAXANT, PRIRTY, PRIINI)
      IF (PRIRTY(1).GE.0) THEN
         PRMAX = PRIRTY(1)
         IF (PRMAX.EQ.0) THEN
            DO 24 I1 = 1,NUMANT
               PRIRTY(1+I1) = I1
 24            CONTINUE
            PRMAX = NUMANT
         ELSE
C                                       cycle through all antennas
            DO 28 I1 = 1,NUMANT
C                                       antenna appears already on PRIRTY list?
               ADDANT = T
               DO 27 I2 = 1,PRMAX
                  IF (PRIRTY(1+I2).EQ.I1) ADDANT = F
 27               CONTINUE
C                                       If not, add it to the end
               IF (ADDANT) THEN
                  PRMAX = PRMAX + 1
                  PRIRTY(1+PRMAX) = I1
                  END IF
 28            CONTINUE
            END IF
C                                       each entry of BTCODE points to the
C                                       relevant entry of BLCODE
         NBL = 0
         DO 23 I1 = 1,NUMANT-1
            DO 29 I2 = I1+1, NUMANT
               NBL = NBL + 1
               IF (PRIRTY(1+I1).LT.PRIRTY(1+I2)) THEN
                  BTCODE(NBL) = NUMANT *(NUMANT-1)/2 -
     *               (NUMANT-PRIRTY(1+I1)+1)*(NUMANT-PRIRTY(1+I1))/2 +
     *               PRIRTY(1+I2)-PRIRTY(1+I1)
               ELSE
                  BTCODE(NBL) = NUMANT * (NUMANT-1)/2 -
     *               (NUMANT+1-PRIRTY(1+I2))*(NUMANT-PRIRTY(1+I2))/2 +
     *               PRIRTY(1+I1)-PRIRTY(1+I2)
                  END IF
 29            CONTINUE
 23         CONTINUE
C                                       baselines in desired order in BTCODE
C                                       PRIRTY lists by decreasing priority
         DO 31 I1 = 1, NUMANT
            BLCODE(I1) = PRIRTY(1+I1)
            PRIRTY(1+I1) = 0
 31         CONTINUE
         DO 32 I1 = 1, NUMANT
            I2 = BLCODE(I1)
            PRIRTY(1+I2) = 1 + NUMANT - I1
 32         CONTINUE
C                                       PRIRTY(1+I) indicates priority of ant I
         END IF
C
C                                       If this definition of BLCODE is
C                                       changed, please adjust BTCODE above
C                                       accordingly!!!
C                                        Set baseline arrays.
      NANTM1 = NUMANT - 1
      NBL = 0
      IBASE = 256
      IF (ILOCB.LT.0) IBASE = 32768
      DO 30 I1 = 1, NANTM1
         I1P1 = I1 + 1
         DO 25 I2 = I1P1, NUMANT
            NBL = NBL + 1
            BLCODE(NBL) = I1 * IBASE + I2
            IS(NBL) = I1
            JS(NBL) = I2
 25         CONTINUE
 30      CONTINUE
      IF (PRIRTY(1).LT.0) THEN
         DO 33 I1=1,NBL
            BTCODE(I1) = I1
 33         CONTINUE
         END IF
C                                       Get IF frequency offsets.
      IXLUN = 29
      TVER = 1
      CALL CHNDAT ('READ', NXBUFF, DISK, VCNO, TVER, CATBLK, IXLUN, MIF,
     *   FREQIF, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Fill frequency array for each
C                                       channel
      CALL FRQTAB (DISK, VCNO, IXLUN, CATBLK, FRQSEL, NXBUFF,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      NUMFRQ = 0
      FREQS(1) = 0.0D0
C
      TMPNIF = EIF - BIF + 1
      TMPNFQ = ECHAN - BCHAN + 1
C                                       Check number of frequencies.
      IF ((TMPNFQ.GT.MAXCHA) .OR. (TMPNIF.GT.MAXIF) .OR.
     *   (TMPNIF*TMPNFQ.GT.MAXCIF)) THEN
         IERR = 1
         MSGTXT = 'FRNFIT: VISIBILITIES TOO BIG FOR BUFFERS'
         GO TO 990
         END IF
C                                       Refer frequencies to ref. freq.
C                                       in GHz.
      KFRQ = 0
      DO 40 JIF = 1,TMPNIF
         DO 35 JFRQ = 1,TMPNFQ
            KFRQ = KFRQ + 1
            IF (MOD(JFRQ-1,CHINC).EQ.0) THEN
               NUMFRQ = NUMFRQ + 1
               FREQS(NUMFRQ) = 0.0D0
               DO 34 I = 1,CHINC
                  FREQS(NUMFRQ) = FREQS(NUMFRQ) + FREQG(KFRQ+I-1)
 34               CONTINUE
               FREQS(NUMFRQ) = (FREQS(NUMFRQ)/CHINC - FREQ) * 1.0D-9
               WTF(NUMFRQ) = 1.0
               END IF
 35         CONTINUE
 40      CONTINUE
      IF (NUMFRQ.LE.0) NUMFRQ = 1
C                                       Find IF averages
      NFPIF = NUMFRQ / NUMIF
      IF (AVGIF) THEN
         DO 60 I = 1,NUMIF
            SUM = 0.0D0
            I1 = (I-1) * NFPIF + 1
            I2 = I1 + NFPIF - 1
            DO 50 J = I1,I2
               SUM = SUM + FREQS(J)
 50            CONTINUE
            FREQS(I) = SUM / (I2-I1+1)
 60         CONTINUE
            END IF
C                                       Check AP size
      IF (AVGIF) THEN
         CALL CHKAP (SOLINT * 86400.0, TINTG * 86400, NUMIF, NUMIF,
     *      FREQS, DOIF, DOEVLA, RATWIN, DELWIN, IERR)
      ELSE
         CALL CHKAP (SOLINT * 86400.0, TINTG * 86400, NUMFRQ, NUMIF,
     *      FREQS, DOIF, DOEVLA, RATWIN, DELWIN, IERR)
         END IF
      IF (IERR.NE.0) GO TO 999
C                                       Initialize I/O to INDEX file
      MSGSUP = 32000
      NXVER = 1
      CALL NDXINI ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATIN,
     *   IXLUN, INXRNO, NXKOLS, NXNUMV, IRET)
      MSGSUP = 0
      DONDX = IRET.EQ.0
C                                       Read first scan info
      IF (DONDX) THEN
 65      IF (INXRNO.LE.NXBUFF(5)) THEN
            CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV, SCNTIM,
     *         SCNDT, NDXSOU, SCNSUB, IDUM1, IDUM2, FREQID, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Scan match current data?
            IF ((SCNSUB.GT.0) .AND. (SUBARR.GT.0) .AND.
     *         (SUBARR.NE.SCNSUB)) GO TO 65
            IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *         (FRQSEL.NE.FREQID)) GO TO 65
C                                       Accept?
            IF (.NOT.(WANSRC(NDXSOU, DOCWNT, NCALWD, CALWAN))) GO TO 65
            SCNINT = SCNDT
            SCNBEG = SCNTIM - 0.5D0 * SCNINT
            SCNEND = SCNBEG + SCNINT + 10.0D0*DEPS
            IF (SCNBEG.LT.TSTART) SCNBEG = TSTART
            IF (SCNEND.GT.TEND) SCNEND = TEND + 10.0D0*DEPS
            SCNINT = SCNEND - SCNBEG
            SCNSOU = ONESOU
            IF (TSTART.GT.SCNEND) GO TO 65
         ELSE
            DONDX = .FALSE.
            END IF
         END IF
C                                       Dummy if no NX table
      IF (.NOT.DONDX) THEN
         SCNBEG = -1.0E10
         SCNEND =  1.0E10
         SCNINT = SOLINT
         SCNSOU = ONESOU
         SCNSUB = 0
         FREQID = FRQSEL
         END IF
C                                       Begin Loop in time.
 80   NIN = 1
C                                       Clear "Got data" flags
         DO 86 ISUB = 1,SOLSUB
            DO 85 KK1 = 1,NUMANT
               GOTANT(KK1,ISUB) = F
 85            CONTINUE
 86         CONTINUE
         MSUB = 0
C                                       Clear source id
         SCNSOU = ONESOU
C                                       Blank/zero solution values
         NBLANK = 2 * NUMIF * NUMANT
         CALL RFILL (2*NUMANT, 0.0, CMBDEL)
         CALL RFILL (2*NUMANT, 0.0, CDISP)
         CALL RFILL (NBLANK, FBLANK, CREAL)
         CALL RFILL (NBLANK, FBLANK, CIMAG)
         CALL RFILL (NBLANK, 0.0, CDELY)
         CALL RFILL (NBLANK, 0.0, CRATE)
         CALL RFILL (NBLANK, 0.0, CWT)
         CALL FILL (10, 0, NTIMES)
C                                        Init. for sol. interval.
C                                        Zero weights. etc
         CALL RFILL (MAXTIM, 0.0, WTT)
         KK1 = MXBASE * 10 * 2
         CALL RFILL (KK1, 0.0, WTB)
         KK1 = NUMBL * 10
         CALL RFILL (KK1, 1.0, WGTMOD)
         CALL RFILL (KK1, 0.0, TAU)
         CALL RFILL (MAXTIM, 0.0, TIME)
         KK3 = MAXTIM * MAXBL
         CALL RFILL (KK3, 0.0, TIMB)
         KK3 = KK3 * MAXFRQ * MCOR
         CALL RFILL (KK3, 0.0, VREAL)
         CALL RFILL (KK3, 0.0, VIMAG)
C                                       Read first record (if nec)
C                                       and setup
 115     IF (.NOT.JUSRED) THEN
            NIN = 0
            CALL UVDISK ('READ', LUNI, FINDI, BUFF1, NIN, BINDI, IERR)
            IF ((NIN.LE.0) .OR. (IERR.EQ.4)) THEN
               IERR = 0
               GO TO 300
            ELSE IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) IERR, 'READ'
               GO TO 990
               END IF
            JUSRED = T
            END IF
         CURTIM = BUFF1(BINDI+ILOCT)
         IF (CURTIM.LT.SCNBEG-5.0D0*DEPS) THEN
            JUSRED = .FALSE.
            GO TO 115
            END IF
         IF (ILOCB.GE.0) THEN
            IITEMP = BUFF1(BINDI+ILOCB) + 0.1
            SUBA = (BUFF1(BINDI+ILOCB) - IITEMP) * 100.0 + 1.5
         ELSE
            SUBA = BUFF1(BINDI+ILOCSA) + 0.1
            END IF
         IF (ILOCSU.GE.0) SCNSOU = BUFF1(BINDI+ILOCSU) + 0.5
C                                       Find index record
         GO TO 130
 120     IF (DONDX) THEN
 125        IF (INXRNO.LE.NXBUFF(5)) THEN
               CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV,
     *            SCNTIM, SCNDT, NDXSOU, SCNSUB, IDUM1, IDUM2, FREQID,
     *            IERR)
               IF (IERR.NE.0) GO TO 999
C                                       Accept?
               IF (.NOT.(WANSRC(NDXSOU, DOCWNT, NCALWD, CALWAN)))
     *            GO TO 125
               IF ((SCNSUB.GT.0) .AND. (SUBARR.GT.0) .AND.
     *            (SUBARR.NE.SCNSUB)) GO TO 125
               IF ((FREQID.GT.0) .AND. (FRQSEL.GT.0) .AND.
     *            (FRQSEL.NE.FREQID)) GO TO 125
               SCNINT = SCNDT
               SCNBEG = SCNTIM - 0.5D0 * SCNINT
               SCNEND = SCNBEG + SCNINT + 10.0D0*DEPS
               IF (SCNBEG.LT.TSTART) SCNBEG = TSTART
               IF (SCNEND.GT.TEND) SCNEND = TEND + 10.0D0*DEPS
               SCNINT = SCNEND - SCNBEG
               IF (TSTART.GT.SCNEND) GO TO 125
               IF (CURTIM.LT.SCNBEG-5.0D0*DEPS) THEN
                  MSGTXT = 'DATA SKIPPED: APPARENTLY BETWEEN SCANS'
                  CALL MSGWRT (6)
                  JUSRED = .FALSE.
                  GO TO 115
                  END IF
            ELSE
               SCNBEG = CURTIM
               SCNEND = SCNBEG + SOLINT
               END IF
C                                       Dummy if no NX table
         ELSE
            SCNBEG = -1.0E10
            SCNEND =  1.0E10
            SCNINT = SOLINT
            SCNSUB = 0
            FREQID = FRQSEL
            END IF
C                                       This scan?
 130     IF (CURTIM.GT.SCNEND) GO TO 120
         IDAY = CURTIM
         X8 = (CURTIM - IDAY) / TINTG
         TIMNOM = INT (X8) * TINTG + TINTGH + IDAY
         LASTIM = TIMNOM + SOLINT - TINTGH
         NTIM = 1
         WTT(1) = 1.0
         TIME(1) = TIMNOM
         STTIME = CURTIM
         CURSUB = SOLINT / SOLSUB
         CUREND = CURTIM
C                                       if INDEXed divide up scan
C                                       into even sections
         IF (DONDX) THEN
            NUMINT = DROUND (SCNINT / SOLINT)
            NUMINT = MAX (NUMINT, 1)
            SIUSE = SCNINT / NUMINT
            INTNO = (CURTIM-SCNBEG+DEPS) / SIUSE
            IF (INTNO.LT.0) INTNO = 0
            INTNO = INTNO + 1
            LASTIM = SCNBEG + INTNO * SIUSE
            IF (LASTIM.GT.SCNEND) LASTIM = SCNEND
            CURSUB = SIUSE / SOLSUB
            END IF
C                                       Load data into array.
C                                       Begin Loop.
C                                       If next point already read,
C                                       skip read.
         IF (JUSRED) GO TO 210
 200        NIN = 0
            CALL UVDISK ('READ', LUNI, FINDI, BUFF1, NIN, BINDI, IERR)
            IF ((NIN.LE.0) .OR. (IERR.EQ.4)) THEN
               IERR = 0
               GO TO 300
            ELSE IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) IERR, 'READ'
               GO TO 990
               END IF
 210     CURTIM = BUFF1(BINDI+ILOCT)
C                                       Check for last time.
         IF (CURTIM.GT.LASTIM) GO TO 300
         ISUB = (CURTIM - STTIME) / CURSUB + 1.0
         ISUB = MAX (1, MIN (SOLSUB, ISUB))
C                                       Check source change unless
C                                       we have preselected data
C                                       for a single source
         IF (ILOCSU.GE.0) THEN
            ISU = BUFF1(BINDI+ILOCSU) + 0.5
            IF (ISU.NE.SCNSOU) GO TO 300
            END IF
C                                       Check if finished.
         IF (NIN.LE.0) GO TO 300
C                                        Determine baseline code.
         IF (ILOCB.GE.0) THEN
            JBL = BUFF1(BINDI+ILOCB) + 0.1
            I1 = JBL / 256
            I2 = JBL - I1 * 256
         ELSE
            I1 = BUFF1(BINDI+ILOCA1) + 0.1
            I2 = BUFF1(BINDI+ILOCA2) + 0.1
            END IF
         JBL = I1 * IBASE + I2
C                                        Look for match.
         DO 220 I = 1, NBL
            IBL = I
            IF (JBL.EQ.BLCODE(I)) GO TO 230
 220        CONTINUE
C                                        Bad baseline code.
C                                        No message on AC data
         IF (I2.NE.I1) THEN
            WRITE (MSGTXT,1130) I1, I2, NUMANT
            CALL MSGWRT (6)
            END IF
C                                        Go to next data point
         GO TO 200
C                                       Store preaverage time for each
C                                       baseline. Assume it is constant
C                                       within each solution interval.
 230     IF ((TAU(IBL,ISUB).EQ.0.0).AND.(ILOCIT.GE.0)) THEN
            TAU(IBL,ISUB) = BUFF1(BINDI+ILOCIT)
            WGTMOD(IBL,ISUB) = TAU(IBL,ISUB) / (TINTG * 86400.0)
            END IF
C                                        Check if new time.
         IF ((CURTIM-TIMNOM).GT.(TINTGH-DEPS)) THEN
            IDAY = CURTIM
            X8 = (CURTIM - IDAY) / TINTG + 0.5D0
            TIMNOM = INT(X8) * TINTG + IDAY
C                                         Compute time increment.
            XINC = (TIMNOM - TIME(NTIM)) / TINTG
            NTIM = NTIM + IROUND (XINC)
            IF (DOAVER) NTIM = 1
C                                         Check # of time s.
            IF (NTIM.GT.MAXTIM) THEN
               NTIM = NTIM - IROUND (XINC)
               IF (.NOT.WARN) THEN
C                                         Warn that soln. int. too long
                  WARN = T
                  DELT = (TIME(NTIM)-TIME(1)) * 24. * 60.
                  WRITE (MSGTXT,1050)
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,1060)
                  IERR = 5
                  GO TO 990
                  END IF
               END IF
            END IF
         TIME(NTIM) = TIMNOM
         WTT(NTIM) = 1.0
         NTIMES(ISUB) = NTIM
C                                       Baseline factors
         I1 = IS(IBL)
         I2 = JS(IBL)
         GOTANT(I1,ISUB) = T
         GOTANT(I2,ISUB) = T
         CUREND = CURTIM
         MSUB = MAX (MSUB, ISUB)
         BLFACT = ANTWT(I1) * ANTWT(I2)
         BASEL = BUFF1(BINDI+ILOCU) * BUFF1(BINDI+ILOCU) +
     *      BUFF1(BINDI+ILOCV) * BUFF1(BINDI+ILOCV)
         IF ((BASEL.LT.MN2BAS) .OR. (BASEL.GT.MX2BAS))
     *      BLFACT = BLFACT * WTPABL
C                                       Accumulate
         ENDTIM = CURTIM
         DO 260 ICOR = 1,MCOR
            DO 255 I = 1,NUMFRQ
               IM1 = I - 1
               JFRQ = MOD (IM1, NFPIF)
               JIF = IM1 / NFPIF
               II = JIF * INCJJJ + JFRQ * INCIII + 1
               INDEX = BINDI + NRPARM + JFRQ * CHINC * INCF +
     *            JIF * INCIF + (ICOR-1) * INCS
C                                       JIF, LBIF, LEIF 0 relative
               IF ((JIF.LT.LBIF) .OR. (JIF.GT.LEIF)) THEN
                  WTFACT = 0.0
               ELSE
                  WTFACT = 1.0
                  END IF
               DO 254 KF = 1,CHINC
                  WT = WTFACT * BLFACT * BUFF1(INDEX+2)
                  AMP = BUFF1(INDEX)*BUFF1(INDEX) +
     *               BUFF1(INDEX+1)*BUFF1(INDEX+1)
                  IF ((WT.GT.0.0) .AND. (AMP.GT.0.0)) THEN
                     WT = WT * AMP
                     CALL REWAIT (LWT, WT)
                     VREAL(NTIM,II,IBL,ICOR) = VREAL(NTIM,II,IBL,ICOR)
     *                  + WT * BUFF1(INDEX)
                     VIMAG(NTIM,II,IBL,ICOR) = VIMAG(NTIM,II,IBL,ICOR)
     *                  + WT * BUFF1(INDEX+1)
                     WTB(IBL,ICOR,ISUB) = WTB(IBL,ICOR,ISUB) + WT
                     END IF
                  INDEX = INDEX + INCF
 254              CONTINUE
 255           CONTINUE
 260        CONTINUE
         TIMB(NTIM,IBL) = CURTIM
C                                       Sum 2nd Stokes'
         IF (AVGPOL) THEN
            DO 270 I = 1,NUMFRQ
               IM1 = I - 1
               JFRQ = MOD (IM1, NFPIF)
               JIF = IM1 / NFPIF
               II = JIF * INCJJJ + JFRQ * INCIII + 1
               INDEX = BINDI + NRPARM + JFRQ * CHINC * INCF +
     *             JIF * INCIF + INCS
               DO 269 KF = 1,CHINC
                  WT = BLFACT * BUFF1(INDEX+2)
                  AMP = BUFF1(INDEX)*BUFF1(INDEX) +
     *               BUFF1(INDEX+1)*BUFF1(INDEX+1)
                  IF ((WT.GT.0.0) .AND. (AMP.GT.0.0)) THEN
                     WT = WT * AMP
                     CALL REWAIT (LWT, WT)
                     VREAL(NTIM,II,IBL,1) = VREAL(NTIM,II,IBL,1) +
     *                  WT * BUFF1(INDEX)
                     VIMAG(NTIM,II,IBL,1) = VIMAG(NTIM,II,IBL,1) +
     *                  WT * BUFF1(INDEX+1)
                     WTB(IBL,1,ISUB) = WTB(IBL,1,ISUB) + WT
                     END IF
                  INDEX = INDEX + INCF
 269              CONTINUE
 270           CONTINUE
            END IF
C                                       Loop back for next visibility
         GO TO 200
C                                       End of solution interval.
 300  JUSRED = T
C                                       Do solution.
C                                       Adjust time to center.
C                                       Center time defined by first
C                                       poln.
      TIMEC = (STTIME + ENDTIM) * 0.5
      DO 310 I = 1,NTIM
         TIME(I) = TIME(I) - TIMEC
         DO 305 J = 1,NBL
            TIMB(I,J) = TIMB(I,J) - TIMEC
 305        CONTINUE
 310     CONTINUE
C                                       Write time if requested
      IF ((PRTLV.GE.0) .OR. ((PRTLV.EQ.0) .AND. (NMSG.EQ.0))) THEN
         KDAY = TIMEC
         TIMEX = (TIMEC - KDAY) * 24.
         KHR = TIMEX
         TIMEX = (TIMEX - KHR) * 60.
         KMN = TIMEX
         TIMEX = (TIMEX - KMN) * 60.
         KSEC = IROUND (TIMEX)
         END IF
C                                       double up
      TNTIM = NTIM
      IF (NTIM.EQ.1) THEN
         NUMTIM = 2
         EXPAND = .TRUE.
         TIME(2) = 10.D0 * DEPS
         TIME(1) = -10.D0 * DEPS
         DO 510 IBL = 1,NBL
            TIMB(2,IBL) = 10.0D0*DEPS
            TIMB(1,IBL) = -10.0D0*DEPS
            DO 509 ICOR = 1,MCOR
               DO 508 I = 1,NUMFRQ
                  IM1 = I - 1
                  JFRQ = MOD (IM1, NFPIF)
                  JIF = IM1 / NFPIF
                  II = JIF * INCJJJ + JFRQ * INCIII + 1
                  VREAL(2,II,IBL,ICOR) = VREAL(1,II,IBL,ICOR)
                  VIMAG(2,II,IBL,ICOR) = VIMAG(1,II,IBL,ICOR)
 508              CONTINUE
 509           CONTINUE
 510        CONTINUE
         WTT(2) = WTT(1)
      ELSE IF (NORAT) THEN
C                                       must make space
         MTIM = 2 - NTIMES(1)
         IF ((MTIM.GT.0) .AND. (MSUB.GT.1)) THEN
            DO 515 ISUB = 2,MSUB
               NTIMES(ISUB) = NTIMES(ISUB) + MTIM
 515           CONTINUE
            DO 540 I = TNTIM,1,-1
               DO 530 ICOR = 1,MCOR
                  DO 525 J = 1,NBL
                     DO 520 II = 1,MAXFRQ
                        VREAL(I+MTIM,II,J,ICOR) = VREAL(I,II,J,ICOR)
                        VIMAG(I+MTIM,II,J,ICOR) = VIMAG(I,II,J,ICOR)
 520                    CONTINUE
                     CALL RFILL (MTIM, 0.0, VREAL(TOFF,II,J,ICOR))
                     CALL RFILL (MTIM, 0.0, VIMAG(TOFF,II,J,ICOR))
 525                 CONTINUE
 530              CONTINUE
               WTT(I+MTIM) = WTT(I)
               TIME(I+MTIM) = TIME(I)
               DO 535 IBL = 1,NBL
                  TIMB(I+MTIM,IBL) = TIMB(I,IBL)
 535              CONTINUE
 540           CONTINUE
            DO 550 I = 1,MTIM
               WTT(I) = 0.0
               TIME(I) = 0.0
               DO 545 IBL = 1,NBL
                  TIMB(I,IBL) = 0.0
 545              CONTINUE
 550           CONTINUE
            TNTIM = TNTIM + MTIM
            END IF
C                                       now sum up
         DO 580 I = 2,TNTIM
            DO 570 ICOR = 1,MCOR
               DO 565 J = 1,NBL
                  DO 560 II = 1,MAXFRQ
                     VREAL(1,II,J,ICOR) = VREAL(1,II,J,ICOR) +
     *                  VREAL(I,II,J,ICOR)
                     VIMAG(1,II,J,ICOR) = VIMAG(1,II,J,ICOR) +
     *                  VIMAG(I,II,J,ICOR)
 560                 CONTINUE
 565              CONTINUE
 570           CONTINUE
            WTT(1) = MAX (WTT(I), WTT(1))
            TIME(1) = TIME(1) + TIME(I)
            DO 575 IBL = 1,NBL
               TIMB(1,IBL) = TIMB(1,IBL) + TIMB(I,IBL)
 575           CONTINUE
 580        CONTINUE
C                                       average and copy to 2
         NUMTIM = 2
         EXPAND = .TRUE.
         TIME(2) = 10.D0 * DEPS
         TIME(1) = -10.D0 * DEPS
         DO 590 IBL = 1,NBL
            TIMB(2,IBL) = 10.0D0*DEPS
            TIMB(1,IBL) = -10.0D0*DEPS
            DO 585 ICOR = 1,MCOR
               DO 584 I = 1,NUMFRQ
                  IM1 = I - 1
                  JFRQ = MOD (IM1, NFPIF)
                  JIF = IM1 / NFPIF
                  II = JIF * INCJJJ + JFRQ * INCIII + 1
                  VREAL(2,II,IBL,ICOR) = VREAL(1,II,IBL,ICOR)
                  VIMAG(2,II,IBL,ICOR) = VIMAG(1,II,IBL,ICOR)
 584              CONTINUE
 585           CONTINUE
 590        CONTINUE
         WTT(2) = WTT(1)
      ELSE
         NUMTIM = NTIM
         EXPAND = .FALSE.
         END IF
C                                       sum up weights into interval 1
      DO 315 ISUB = 2,MSUB
         DO 314 IBL = 1,MAXBL
            IF ((TAU(IBL,1).EQ.0.0) .AND. (ILOCIT.GE.0)) THEN
               TAU(IBL,1) = TAU(IBL,ISUB)
               WGTMOD(IBL,1) = WGTMOD(IBL,ISUB)
               END IF
            DO 313 ICOR = 1,MCOR
               WTB(IBL,ICOR,1) = WTB(IBL,ICOR,1) + WTB(IBL,ICOR,ISUB)
 313           CONTINUE
            I1 = IS(IBL)
            I2 = JS(IBL)
            IF (.NOT.GOTANT(I1,1)) GOTANT(I1,1) = GOTANT(I1,ISUB)
            IF (.NOT.GOTANT(I2,1)) GOTANT(I2,1) = GOTANT(I2,ISUB)
 314        CONTINUE
 315     CONTINUE
C                                       Loop over Stokes'
      DO 400 ICOR = 1,MCOR
         IF ((PRTLV.GE.0) .OR. ((PRTLV.EQ.0) .AND. (NMSG.EQ.0))) THEN
            WRITE (MSGTXT,2001) KDAY, KHR, KMN, KSEC, ICOR
            CALL MSGWRT (2)
            END IF
         IC = ICOR
C                                       IC=0 means averaged Stokes
         IF (AVGPOL) IC = 0
C                                       IC=3 means Ipol
         IF (ISIQUV) IC = 3
C                                       Replicate entries if there
C                                       are unequal integration times
C                                       in the data, so that baseline
C                                       stacking will work.
         DO 340 IBL = 1,NBL
C                                       Skip if not applicable
            IF (WGTMOD(IBL,1).GE.2) THEN
               DO 330 I = 1,NUMFRQ
                  IM1 = I - 1
                  JFRQ = MOD (IM1, NFPIF)
                  JIF = IM1 / NFPIF
                  II = JIF * INCJJJ + JFRQ * INCIII + 1
C                                       Find positions of non zero data
C                                       and store in array NNPLAC.
                  NTICK = 0
                  DO 320 NTIM = 1,NUMTIM
                     IF ((VREAL(NTIM,II,IBL,ICOR).NE.0) .OR.
     *                  (VIMAG(NTIM,II,IBL,ICOR).NE.0)) THEN
                        NTICK = NTICK + 1
                        NPLACE(NTICK) = NTIM
                        NNPLAC(NTICK,II,IBL) = NTIM
                        END IF
 320                 CONTINUE
C                                       Fill the zero entries using
C                                       nearby non-zero values.
                  NLEFT = 1
                  ITICK =1
                  DO 325 NTIM = 1,NUMTIM
                     IF (NTICK.EQ.0) THEN
                        VREAL(NTIM,II,IBL,ICOR) = 0.0
                        VIMAG(NTIM,II,IBL,ICOR) = 0.0
                     ELSE
                        IF ((ITICK + 1).LE.NTICK) THEN
                           NRIGHT = (NPLACE(ITICK+1)+NPLACE(ITICK)) / 2
                        ELSE
                           NRIGHT = NUMTIM
                           END IF
                        IF ((NTIM.GE.NLEFT).AND.(NTIM.LE.NRIGHT)) THEN
                           VREAL(NTIM,II,IBL,ICOR) =
     *                        VREAL(NPLACE(ITICK),II,IBL,ICOR)
                           VIMAG(NTIM,II,IBL,ICOR) =
     *                        VIMAG(NPLACE(ITICK),II,IBL,ICOR)
                           WTT(NTIM) = 1.0
                           END IF
                        IF (NTIM.EQ.NRIGHT) THEN
                           NLEFT = NRIGHT
                           ITICK = ITICK + 1
                           END IF
                        END IF
 325                 CONTINUE
 330              CONTINUE
               END IF
 340        CONTINUE
C                                       If averaged in IF
         IF (AVGIF) THEN
            MFRQ = NUMIF
            MIF = 1
         ELSE
            MFRQ = NUMFRQ
            MIF = NOIF
            END IF
C                                       Find initial delay-rate solution
C                                       Use original method
         IF ((PRIRTY(1).LT.0) .AND. (.NOT.GDSOLV(1))) THEN
            CALL FRNSRC  (IS, JS, VREAL(1,1,1,ICOR), VIMAG(1,1,1,ICOR),
     *         TIME, FREQS, CMBDEL, CREAL, CIMAG, CDELY, CRATE, CWT,
     *         REFAN, MAXFRQ, MAXTIM, MAXIFS, NUMANT, NUMBL, MFRQ,
     *         NUMTIM, NOIF, WGTMOD, WTB(1,ICOR,1), WTT, WTF, DELWIN,
     *         RATWIN, REFANT, IC, SNRMIN, PRTLV, BLDO, DOIF, DOEVLA,
     *         FREQIF, IERR)
C                                       Search on all baselines if requested
         ELSE
            CALL FRNALL (IS, JS, VREAL(1,1,1,ICOR), VIMAG(1,1,1,ICOR),
     *         TIME, FREQS, CMBDEL, CREAL, CIMAG, CDELY, CRATE, CWT,
     *         REFAN, MAXFRQ, MAXTIM, MAXIFS, NUMANT, NUMBL, MFRQ,
     *         NUMTIM, NOIF, WGTMOD, WTB(1,ICOR,1), WTT, WTF, DELWIN,
     *         RATWIN, REFANT, IC, SNRMIN, PRTLV, BLDO, DOIF, DOEVLA,
     *         FREQIF, BTCODE, PRIRTY(2), GDSOLV, IERR)
            END IF
         IF (IERR.EQ.2) GO TO 999
         IF (IERR.NE.0) GO TO 400
C                                       Release AP if time
         IJK = 0
         CALL QROLL (0, NXBUFF, IJK, IERR)
C                                       Remove replicated points if
C                                       they were added above.
         DO 380 IBL = 1,NBL
            IF (WGTMOD(IBL,1).GE.2) THEN
               DO 370 I = 1,NUMFRQ
                  IM1 = I - 1
                  JFRQ = MOD (IM1, NFPIF)
                  JIF = IM1 / NFPIF
                  II = JIF * INCJJJ + JFRQ * INCIII + 1
C                                       Find positions of non zero data
                  NTICK = 1
                  DO 360 NTIM = 1,NUMTIM
                     IF (NTIM.NE.NNPLAC(NTICK,II,IBL)) THEN
                        VREAL(NTIM,II,IBL,ICOR) = 0.0
                        VIMAG(NTIM,II,IBL,ICOR) = 0.0
                     ELSE
                        NTICK = NTICK + 1
                        END IF
 360                 CONTINUE
 370              CONTINUE
               END IF
 380        CONTINUE
C                                       Delay, rate, phase soln.
         IF (DODRLS)
     *      CALL FRNDRP (IS, JS, VREAL(1,1,1,ICOR), VIMAG(1,1,1,ICOR),
     *      TIMB, FREQS, FREQIF, CMBDEL, CREAL, CIMAG, CDELY, CRATE,
     *      CWT, REFAN, MAXFRQ, MAXTIM, MAXIFS, MAXBL, NUMANT, NUMBL,
     *      MFRQ, NUMTIM, NOIF, DOIF, DOEVLA, WTB(1,ICOR,1), WTT,
     *      WTF, DELWIN,RATWIN, IC, SNRMIN, PRTLV, GDSOLV, RINWIN)
C                                       Multi and single band delay,
C                                       rate, phase soln.
         IF (DODRLS .AND. DOMS)
     *      CALL FRNMSP (IS, JS, VREAL(1,1,1,ICOR), VIMAG(1,1,1,ICOR),
     *      TIMB, FREQS, FREQIF, CMBDEL, CREAL, CIMAG, CDELY, CRATE,
     *      CWT, REFAN, MAXFRQ, MAXTIM, MAXIFS, MAXBL, NUMANT, NUMBL,
     *      MFRQ, NUMTIM, NOIF, WTB(1,ICOR,1), WTT, WTF, DELWIN, RATWIN,
     *      IC, SNRMIN, GDSOLV, PRTLV)
C                                       dispersion and multi-band delay
         IF (APARM(10).GT.0.0)
     *      CALL FRDISP (IS, JS, VREAL(1,1,1,ICOR), VIMAG(1,1,1,ICOR),
     *      FREQ, FREQS, FREQIF, CMBDEL, CDISP, CREAL, CIMAG, CDELY,
     *      CWT, REFAN, MAXFRQ, MAXTIM, MAXIFS, MAXBL, NUMANT, NUMBL,
     *      MFRQ, NUMTIM, NOIF, WTB(1,ICOR,1), WTT, WTF, IC, GDSOLV,
     *      PRTLV, TIMEC, DODISP)
 400     CONTINUE
      NMSG = MOD (NMSG+1, 10)
C                                       Write solution record.
      DELT = ENDTIM - STTIME
      IF (DELT.LT.0.0) DELT = 0.0
      CALL FRSNOU (TIMEC, DELT, SCNSOU, IFRM, NODENO, SUBA, MAXIFS,
     *   FREQIF, GOTANT, ISNRNO, SNKOLS, SNNUMV, CMBDEL, CDISP,
     *   CREAL, CIMAG, CDELY, CRATE, CWT, REFAN, CNTOK,CNTBAD, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Shift subintervals
      IF (MSUB.GT.1) THEN
         MTIM = NTIMES(1)
         DO 425 ISUB = 2,MSUB
            NTIMES(ISUB-1) = NTIMES(ISUB) - MTIM
            DO 420 IBL = 1,MAXBL
               TAU(IBL,ISUB-1) = TAU(IBL,ISUB)
               WGTMOD(IBL,ISUB-1) = WGTMOD(IBL,ISUB)
               DO 415 ICOR = 1,MCOR
                  WTB(IBL,ICOR,ISUB-1) = WTB(IBL,ICOR,ISUB)
 415              CONTINUE
               IF (ISUB.EQ.MSUB) THEN
                  TAU(IBL,ISUB) = 0.0
                  WGTMOD(IBL,ISUB) = 0.0
                  WTB(IBL,1,ISUB) = 0.0
                  WTB(IBL,2,ISUB) = 0.0
                  END IF
 420           CONTINUE
 425        CONTINUE
         NTIMES(MSUB) = 0
         TOFF = TNTIM - MTIM + 1
C                                       fix and shift times
         DO 435 I = MTIM+1,TNTIM
            TIME(I-MTIM) = TIME(I) + TIMEC
            WTT(I-MTIM) = WTT(I)
            DO 430 J = 1,NBL
               TIMB(I-MTIM,J) = TIMB(I,J) + TIMEC
 430           CONTINUE
 435        CONTINUE
         DO 440 J = 1,NBL
            CALL RFILL (MTIM, 0.0, TIMB(TOFF,J))
 440        CONTINUE
         CALL RFILL (MTIM, 0.0, TIME(TOFF))
C                                       main time pointers
         STTIME = STTIME + CURSUB
         NTIM = TNTIM - MTIM
         IDAY = STTIME
         X8 = (STTIME - IDAY) / TINTG
         TIMNOM = INT (X8) * TINTG + TINTGH + IDAY
         TIME(1) = TIMNOM
C                                       shift and zero data arrays
         DO 460 ICOR = 1,MCOR
            DO 455 J = 1,NBL
               DO 450 II = 1,MAXFRQ
                  DO 445 I = MTIM+1,TNTIM
                     VREAL(I-MTIM,II,J,ICOR) = VREAL(I,II,J,ICOR)
                     VIMAG(I-MTIM,II,J,ICOR) = VIMAG(I,II,J,ICOR)
 445                 CONTINUE
                  CALL RFILL (MTIM, 0.0, VREAL(TOFF,II,J,ICOR))
                  CALL RFILL (MTIM, 0.0, VIMAG(TOFF,II,J,ICOR))
 450              CONTINUE
 455           CONTINUE
 460        CONTINUE
         END IF
      MSUB = MSUB - 1
      CUREND = CUREND + CURSUB
      CUREND = MIN (CUREND, SCNEND)
C                                       If not finished, loop back.
      IF (NIN.GT.0) THEN
         IF (MSUB.LE.0) GO TO 80
         IF (NTIM.LE.0) GO TO 80
         IF ((CURTIM.GE.CUREND) .AND. (MSUB.LT.SOLMIN)) GO TO 80
         IF (CURTIM.GE.CUREND) GO TO 300
         LASTIM = CUREND
         GO TO 210
      ELSE
         IF (MSUB.GE.SOLMIN) GO TO 300
         END IF
C                                       Release AP if have it.
      CALL QRLSE
C                                       Close solution files and
C                                       uv file.
      CALL ZCLOSE (LUNI, FINDI, IERR)
C                                       Close index file
      IF (DONDX) CALL TABIO ('CLOS', 0, INXRNO, TIMEC, NXBUFF, IERR)
      IERR = 0
C                                       Give body count
      IF (CNTOK.LE.0) THEN
C                                       Nothing worked
         MSGTXT = 'ERROR: NO VALID SOLUTIONS FOUND'
         IERR = 8
         GO TO 990
      ELSE
         WRITE (MSGTXT,1440) CNTOK
         CALL MSGWRT (6)
         IF (CNTBAD.GT.0) THEN
            WRITE (MSGTXT,1441) CNTBAD
            CALL MSGWRT (6)
            END IF
         END IF
      CALL COPY (1+MAXANT, PRIINI, PRIRTY)
      GO TO 999
C                                        Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('FRNFIT: ERROR',I3,' OPENING INPUT UV FILE')
 1010 FORMAT ('FRNFIT: ERROR',I3,' INITING UV FILE')
 1050 FORMAT ('SOLUTION INTERVAL IS TOO LONG. TRY LESS SOLINT.')
 1060 FORMAT ('OR INCREASE PREAV TIME APPLYING UVAVG.')
 1100 FORMAT ('FRNFIT: ERROR',I3,1X,A4,'ING UV FILE')
 1130 FORMAT ('BAD BASELINE CODE=',I4,'-',I4,' NO. ANT.=',I4)
 1140 FORMAT ('Writing SN table ',I4)
 1150 FORMAT ('FRNFIT: ERROR ',I3,' WRITING ORIGIN KEYWORD TO SN TABLE')
 1440 FORMAT ('Found ', I8, ' good solutions')
 1441 FORMAT ('Failed on ', I8, ' solutions')
 2001 FORMAT ('Time=',I4,'/',3I3.2,', Polarization =',I2)
      END
      SUBROUTINE FRSNOU (TIMEC, DELT, SCNSOU, IFRM, NODENO, SUBA,
     *   MAXIFS, FREQIF, GOTANT, ISNRNO, SNKOLS, SNNUMV, CMBDEL, CDISP,
     *   CREAL, CIMAG, CDELY, CRATE, CWT, REFAN, CNTOK, CNTBAD, IERR)
C-----------------------------------------------------------------------
C   FRSNOU prepares a set of SN table entries and writes them to an SN
C   table.
C   Input:
C      TIMEC    D     Time in days
C      DELT     R     Solution interval in days
C      SCNSOU   I     Source number
C      IFRM     R     Ionospheric Faraday Rotation
C      NODENO   I     Node numbe.
C      SUBA     I     Subarray number
C      MAXIFS   I     Maximum number of IFs
C      FREQIF   D(*)  IF frequency offsets (Hz)
C      GOTANT   L(*)  Flags indicating if there was data for each ant.
C      ISNRNO   I     TABSN counter.
C      SNKOLS   I(*)  SN table column pointers
C      SNNUMV   I(*)  SN table element counts.
C      CMBDEL   R(2,NUMANT)  Multiband delays in seconds.
C      CDISP    R(2,NUMANT)  Dispersion in seconds/m/m
C      CREAL    R(2,NUMIF,NUMANT) Real part of solution
C      CIMAG    R(2,NUMIF,NUMANT) Imag part of solution
C      CDELY    R(2,NUMIF,NUMANT) delays in seconds.
C      CRATE    R(2,NUMIF,NUMANT) Rates in Hz.
C      CWT      R(2,NUMIF,NUMANT) Weights = SNR
C      REFAN    I(2,NUMIF) Reference antennas used
C   Input on common:
C      NUMANT   I     The number of antennas.
C      NUMIF    I     The number of IFs.
C      NUMPOL   I     The number of polarizations.
C      SNRMIN   R     Minimum allowed SNR for a solution.
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
      DOUBLE PRECISION TIMEC, FREQIF(*)
      INTEGER   SCNSOU, NODENO, SUBA, MAXIFS, ISNRNO, SNKOLS(*),
     *   SNNUMV(*), REFAN(2,*), CNTOK, CNTBAD, IERR
      REAL      DELT, IFRM, CMBDEL(2,*), CDISP(2,*), CREAL(2,MAXIFS,*),
     *   CIMAG(2,MAXIFS,*), CDELY(2,MAXIFS,*), CRATE(2,MAXIFS,*),
     *   CWT(2,MAXIFS,*), DDISP(2)
      LOGICAL   GOTANT(MAXANT,10)
C
      INTEGER   IANT, IIF, IREF, LUNSS
      LOGICAL   ISAPPL
      INCLUDE 'FRING.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA DDISP /2*0.0/
C-----------------------------------------------------------------------
C                                       Open table
      LUNSS = 27
      ISAPPL = .FALSE.
      CALL SNINI ('WRIT', CLBUFF, DISKIN, CNOIN, SNVER, CATIN, LUNSS,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Loop over antennae
      DO 420 IANT = 1,NUMANT
C                                       Convert rates Hz to sec/sec
         DO 410 IIF = 1,NUMIF
            IF (CRATE(1,IIF,IANT).NE.FBLANK)
     *         CRATE(1,IIF,IANT) = CRATE(1,IIF,IANT) /
     *         (FREQ + FREQIF(IIF))
            IF (CRATE(2,IIF,IANT).NE.FBLANK)
     *         CRATE(2,IIF,IANT) = CRATE(2,IIF,IANT) /
     *         (FREQ + FREQIF(IIF))
C                                       Keep track of reference ants
            IREF = REFAN(1,IIF)
            IF ((IREF.GT.0) .AND. (IREF.LE.NUMANT)) THEN
               REFUSE(IREF) = REFUSE(IREF) + 1
               REFUSS(IREF,SUBA) = REFUSS(IREF,SUBA) + 1
C                                       Deal with ref SNR
               IF (CWT(1,IIF,IREF).LT.(SNRMIN+1.0))
     *            CWT(1,IIF,IREF) = SNRMIN + 1.0
               END IF
            IREF = REFAN(2,IIF)
            IF ((IREF.GT.0) .AND. (IREF.LE.NUMANT)) THEN
               REFUSE(IREF) = REFUSE(IREF) + 1
               REFUSS(IREF,SUBA) = REFUSS(IREF,SUBA) + 1
               IF (CWT(2,IIF,IREF).LT.(SNRMIN+1.0))
     *            CWT(2,IIF,IREF) = SNRMIN + 1.0
               END IF
C                                       Count good and bad solns
            IF (GOTANT(IANT, 1)) THEN
               IF (CWT(1,IIF,IANT).GE.SNRMIN) THEN
                  CNTOK = CNTOK + 1
               ELSE
                  CNTBAD = CNTBAD + 1
                  END IF
C                                       Second poln.
               IF (NUMPOL.GT.1) THEN
                  IF (CWT(2,IIF,IANT).GE.SNRMIN) THEN
                     CNTOK = CNTOK + 1
                  ELSE
                     CNTBAD = CNTBAD + 1
                     END IF
                  END IF
               END IF
 410        CONTINUE
         IF (GOTANT(IANT, 1))
     *      CALL TABSN ('WRIT', CLBUFF, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *      TIMEC, DELT, SCNSOU, IANT, SUBA, FRQSEL, IFRM, NODENO,
     *      CMBDEL(1,IANT), CDISP(1,IANT), DDISP, CREAL(1,1,IANT),
     *      CIMAG(1,1,IANT), CDELY(1,1,IANT), CRATE(1,1,IANT),
     *      CWT(1,1,IANT), REFAN, IERR)
         IF (IERR.NE.0) GO TO 999
 420     CONTINUE
      CALL TABIO ('CLOS', 0, ISNRNO, TIMEC, CLBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE FRNSRC (IS, JS, VREAL, VIMAG, TIME, FREQS, CMBDEL,
     *   CREAL, CIMAG, CDELY, CRATE, CWT, REFAN, MAXFRQ, MAXTIM, MAXIFS,
     *   NUMANT, NUMBL, NUMFRQ, NUMTIM, NUMIF, WGTMOD, WTB, WTT, WTF,
     *   DELWIN, RATWIN, REFANT, IC, SNRMIN, PRTLV, BLDO, DOIF, DOEVLA,
     *   FREQIF, IERR)
C-----------------------------------------------------------------------
C   FRNSRC makes an initial estimate of the delay, rate and phase
C   of a visibility array by the method of F. Schwab.
C   One set of values is determined from all IFs together and then
C   filled into all.  If the Stokes' parameters were averaged (IC=0)
C   then the common solutions are copied into both.
C      Currently assumes that all frequencies are spaced by multiples
C   of the minimum spacing.
C   Input:
C    IS(*)                       I    First ant. of baseline numbers
C    JS(*)                       I    2nd ant. of baseline numbers
C    VREAL(MAXTIM,MAXFRQ,MAXBL)  R    Real part of visibility array
C    VIMAG(MAXTIM,MAXFRQ,MAXBL)  R    Imag part of visibility array
C    TIME(*)                     R    Time wrt center
C    FREQS(*)                    D    Frequency array
C    MAXTIM              I    Maximum number of time integrations.
C    MAXFRQ              I    Maximum number of frequency channels.
C    MAXIFS              I    Maximum number of IFs
C    NUMANT              I    Number of antennas
C    NUMBL               I    Number of baselines
C    NUMFRQ              I    Number of frequencies
C    NUMTIM              I    Number of times
C    NUMIF               I    Number of IFs
C    WGTMOD(NUMBL)       R    Weight modification array; used if
C                             unequal integration times in the data.
C    WTT(NUMTIM)         R    Time weight array
C    WTF(NUMFRQ)         R    Frequency weight array
C    DELWIN              R    delay window, <0 => no search in delay
C    RATWIN              R    rate window, <0 => no search in rate
C    REFANT              I    Reference antenna to use if possible.
C    IC                  I    Stokes' number passed, 0 => averaged.
C                             1=R, 2=L, 3=I
C    SNRMIN              R    Minimum SNR allowed
C    PRTLV               I    Print level
C    BLDO                I    The number of baseline combinations=1,2,3
C    DOIF                L    If true then solve each IF independently
C    DOEVLA              I    Solve for 2 delays, NIF/2 together
C    FREQIF(*)           D    Reference frequency offset per IF (Hz)
C   Output:
C    WTB(NUMBL)                  R    Baseline weight array, returned
C                                     normalized.
C    CMBDEL(2,NUMANT)            I    Multiband delays in seconds.
C    CREAL(2,NUMIF,NUMANT)       R    Real part of solution
C    CIMAG(2,NUMIF,NUMANT)       R    Imag part of solution
C    CDELY(2,NUMIF,NUMANT)       R    delays in seconds.
C    CRATE(2,NUMIF,NUMANT)       R    Rates in Hz.
C    CWT(2,NUMIF,NUMANT)         R    Weights = SNR
C    REFAN(2,NUMIF)      I    Reference antennas used
C    IERR                I    Return code, 0=>OK, 1 => all data bad,
C                             2=>insufficient memory
C-----------------------------------------------------------------------
      INTEGER   MAXFRQ, MAXTIM, MAXIFS, IERR
      INTEGER   IS(*), JS(*), REFAN(2,*), NUMBL, NUMFRQ, NUMTIM, NUMIF,
     *   NUMANT, REFANT, IC, PRTLV, BLDO, DOEVLA
      LOGICAL   DOIF
      DOUBLE PRECISION FREQIF(*), FREQS(*)
      REAL      VREAL(MAXTIM,MAXFRQ,*), VIMAG(MAXTIM,MAXFRQ,*),
     *   CMBDEL(2,*), CREAL(2,MAXIFS,*), CIMAG(2,MAXIFS,*),
     *   CDELY(2,MAXIFS,*), CRATE(2,MAXIFS,*), CWT(2,MAXIFS,*), TIME(*),
     *   WGTMOD(*), WTB(*), WTT(*), WTF(*), DELWIN, RATWIN,
     *   SNRMIN
      INTEGER   IIF, IB, IST, REFA, ANT, NFRQ, NFPIF, IFP, NOIF, REF2,
     *   IREF, IT, NF, I1, IA, I, IS1, IS2, N2M1, IFQ, GOODCT, ISI, JSI,
     *   TREFAN, NUMPAS, ITEMP, LIMF1, LIMF2, I4TEMP, NNT, NNF, MF, MT,
     *   ND, NR, FCOUNT, APIAD, APINTR, APFIN, NEED, KAP, NIF1, NIF2, NJ
      LOGICAL   TRUE, FALSE, REDO, ALLBAD, FRSMSG
      REAL      AMP, CTR, CTI, CPR, CPI, DF, DT, SNRAT,
     *   XMAX, XMAX2, WT, SUMWT, TWOPI, FRATE, FDELAY,
     *   CWTTST, AVGWTI, SPCNS, SPCMH
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   BADANT(MAXANT)
      INTEGER   REFLST(MAXANT), AIF
      REAL      SWT(MAXANT), WTBT(MXBASE), WT1(MXBASE)
      INCLUDE 'INCS:GAIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:PMAD.INC'
      EQUIVALENCE (WT1, HESS)
      SAVE FRSMSG
      DATA TRUE, FALSE /.TRUE.,.FALSE./
      DATA FRSMSG /.TRUE./
C-----------------------------------------------------------------------
      TREFAN = REFANT
      TWOPI = 8. * ATAN (1.)
      NNT = NUMTIM
      IST = MAX (IC, 1)
C                                       IQUV?
      IF (IC.EQ.3) IST = 1
C                                       Find minimum Freq step,
C                                       and frequency spread
C                                       Only need the spread for 1 IF if
C                                       IFs are separate.
      IF (DOIF) THEN
         FCOUNT = NUMFRQ / NUMIF
         NOIF = NUMIF
      ELSE IF (DOEVLA.GT.0) THEN
         FCOUNT = NUMFRQ / DOEVLA
         NOIF = DOEVLA
      ELSE
         FCOUNT = NUMFRQ
         NOIF = 1
         END IF
      NFPIF = FCOUNT
      CALL GRDFRQ (FREQS, FCOUNT, 1.0E-3, NF, DF)
      NFRQ = NUMFRQ / NOIF
      NNF = MAX (NF, NFRQ, MAXFRQ)
C                                       Time step
      IF (NUMTIM.GT.1) THEN
         DT = (TIME(NUMTIM) - TIME(1)) / (NUMTIM - 1.0)
      ELSE
         DT = 0.01 / 86400.0
         END IF
C                                       Normalize data
      DO 20 IB = 1,NUMBL
         DO 20 IIF = 1,NUMFRQ
            DO 20 IT = 1,NUMTIM
               AMP = SQRT (VREAL(IT,IIF,IB)*VREAL(IT,IIF,IB) +
     *            VIMAG(IT,IIF,IB)*VIMAG(IT,IIF,IB))
               IF (AMP.GT.1.0E-15) THEN
                  VREAL(IT,IIF,IB) = VREAL(IT,IIF,IB) / AMP
                  VIMAG(IT,IIF,IB) = VIMAG(IT,IIF,IB) / AMP
                  END IF
 20            CONTINUE
C                                       FFT sizes
C                                       Time/rate dimension
      MT = LOG (16.0 * NUMTIM) / LOG (2.0) + 0.999
C                                       was 2048 until 3/12/96
C                                       2*MAXIMG is max FFT available
      IS1 = 2*MAXIMG
      IS2 = 2 ** MT
      MT = MIN (IS1, IS2)
      IF ((RATWIN.LE.1.0E-20) .OR. (NUMTIM.LE.1)) MT = NUMTIM
C                                       Freq./delay dimension
      MF = 1
      IF (FCOUNT.GT.1) MF = LOG (16.0 * NF) / LOG (2.0) + 0.999
      IS2 = 2**MF
      IF (FCOUNT.GT.1) MF = MIN (IS1, IS2)
      IF (DELWIN.LE.1.0E-20) MF = NF
C                                        search array size.
      ND = 1
      NR = 1
      IF (NUMTIM.GT.1) NR = MT * (RATWIN / (1000.0 / (DT * 86400.0)))
      NR = MAX (1, NR)
      IF (FCOUNT.GT.1) ND = MF * (DELWIN / (1.0 / ABS(DF)))
      ND = MAX (1, ND)
C                                       Make sure odd
      NR = (NR/2) * 2 + 1
      ND = (ND/2) * 2 + 1
C                                       Make sure that it will fit in
C                                       AP
      I4TEMP = 2 * NUMTIM * NNF
      APIAD = 10
      APINTR = APIAD + I4TEMP
      APFIN = APINTR + 2 * NNF * NR
      NEED = APFIN + 2 * ((NR+1) * (ND+1) + MAX (MT, MF))
      NEED = NEED / 1024 + 4
      CALL QINIT (NEED, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) GO TO 900
      IF (PSAPNW.LT.NEED) THEN
         MSGTXT = 'FRNSRC: DID NOT GET WHAT I NEED FOR MEMORY'
         CALL MSGWRT (8)
         MSGTXT = 'FRNSRC: USE SETMAXAP TO RAISE THE LIMIT, OR'
         CALL MSGWRT (8)
         GO TO 910
         END IF
C                                       Write message if under sampling
C                                       Make sure odd again
      NR = (NR/2) * 2 + 1
      ND = (ND/2) * 2 + 1
      IF ((MF.LT.(4*NF)) .AND. (MF.NE.NF)) THEN
         WRITE (MSGTXT,1020) 4*NF, MF
         CALL MSGWRT (8)
         IF (MT.LT.4*NUMTIM) THEN
            WRITE (MSGTXT,1021) 4*NUMTIM, MT
            CALL MSGWRT (8)
            END IF
         IERR = 2
         GO TO 999
         END IF
      IF ((MT.LT.4*NUMTIM) .AND. (MF.NE.NF)) THEN
         WRITE (MSGTXT,1021) 4*NUMTIM, MT
         CALL MSGWRT (8)
         IERR = 2
         GO TO 999
         END IF
C                                       Make sure WORK declaration in
C                                       FRNSR2 is not exceeded.
      IF (4*NUMTIM.GT.2*MAXIMG) THEN
         MSGTXT = 'FRNSRC: WORK BUFFER TOO SMALL IN FRNSR2'
         CALL MSGWRT (8)
         MSGTXT = 'FRNSRC: USE A SHORTER SOLINT OR AVG. IN TIME'
         CALL MSGWRT (8)
         IERR = 2
         GO TO 999
         END IF
C                                       Notify user regarding coarse
C                                       search spacing
      IF (PRTLV.GE.2) THEN
         IF (ABS(DF)*MF.GT.0.0) THEN
            SPCNS = 1.0 / (ABS(DF) * MF)
         ELSE
            SPCNS = 0.0
            END IF
         IF (DT*MT.GT.0.0) THEN
            SPCMH = 1000.0 / (DT * MT * 86400.0)
         ELSE
            SPCMH = 0.0
            END IF
C
         WRITE (MSGTXT,1041) SPCNS, SPCMH
         CALL MSGWRT (2)
         END IF
C
      N2M1 = 2 * NUMANT - 1
C                                       Normalize baseline weights.
      SUMWT = 0.0
      DO 50 IB = 1,NUMBL
         IF (WTB(IB).GT.0.0) SUMWT = SUMWT + WTB(IB)
 50      CONTINUE
      AVGWTI = 0.0
      IF (SUMWT.GT.1.0E-10) AVGWTI = NUMBL * 1.0 / SUMWT
      DO 60 IB = 1,NUMBL
         IF (WTB(IB).GT.0.0) WTB(IB) = WTB(IB) * AVGWTI
 60      CONTINUE
C                                       Loop over independent IFs
      DO 400 IIF = 1,NOIF
         IFP = (IIF-1) * NFPIF + 1
         AIF = IIF
         IF (DOEVLA.GT.0) AIF = (IIF - 1) * (NUMIF / DOEVLA) + 1
C                                       Blank solution
         REFAN(IST,IIF) = 0
         DO 80 IA = 1,NUMANT
            CREAL(IST,IIF,IA) = FBLANK
            CIMAG(IST,IIF,IA) = FBLANK
            CDELY(IST,IIF,IA) = FBLANK
            CRATE(IST,IIF,IA) = FBLANK
            CWT(IST,IIF,IA) = 0.0
 80         CONTINUE
         DO 90 ANT = 1,NUMANT
            BADANT(ANT) = FALSE
            REFLST(ANT) = 0
 90         CONTINUE
         NUMPAS = 0
C                                       Copy baseline weights.
         CALL RCOPY (NUMBL, WTB, WTBT)
         CALL RCOPY (NUMBL, WTB, WT1)
C                                       Restart here if bad ant. found
 100        REDO = FALSE
            GOODCT = 0
            NUMPAS = NUMPAS + 1
C                                       Determine which antennas have
C                                       data
         DO 110 IB = 1,NUMANT
            SWT(IB) = 0.0
 110        CONTINUE
         SUMWT = 0.0
         LIMF1 = IFP
         LIMF2 = IFP + NFRQ - 1
         DO 140 IB = 1,NUMBL
            IF (WT1(IB).EQ.0.) GO TO 140
            ISI = IS(IB)
            JSI = JS(IB)
            DO 130 IFQ = LIMF1,LIMF2
               DO 120 IT = 1,NUMTIM
                  WT = WT1(IB) * WTT(IT) * WTF(IFQ)
                  IF ((ABS (VREAL(IT,IFQ,IB)) +
     *               ABS (VIMAG(IT,IFQ,IB))).LT.1.0E-20) WT = 0.0
                  SWT(ISI) = SWT(ISI) + WT
                  SWT(JSI) = SWT(JSI) + WT
                  SUMWT = SUMWT + WT
 120              CONTINUE
 130           CONTINUE
 140        CONTINUE
C                                      Find reference antenna.
         REFA = TREFAN
         IF ((REFA.LE.0) .OR. (SWT(REFA).LE.1.0E-20)) THEN
            XMAX = 0.0
            DO 150 IA = 1,NUMANT
               IF ((SWT(IA).GT.XMAX) .AND. (.NOT.BADANT(IA))) THEN
                  XMAX  =  SWT(IA)
                  REFA = IA
                  END IF
 150           CONTINUE
            END IF
C                                       If no good ants. give up on IF
         IF (REFA.EQ.0) GO TO 400
         XMAX2 = 0.0
C                                       Find secondary ref.
         REF2 = REFA
         DO 170 IA = 1,NUMANT
            IF ((SWT(IA).GT.XMAX2) .AND. (IA.NE.REFA) .AND.
     *         (.NOT.BADANT(IA))) THEN
               XMAX2 = SWT(IA)
               REF2 = IA
               END IF
 170        CONTINUE
C                                       Initialize ref. ant values
         CREAL(IST,IIF,REFA) = 1.0
         CIMAG(IST,IIF,REFA) = 0.0
         CDELY(IST,IIF,REFA) = 0.0
         CRATE(IST,IIF,REFA) = 0.0
         CWT(IST,IIF,REFA) = SNRMIN + 1.0
C                                         Loop over antennae
         DO 300 ANT = 1,NUMANT
            IREF = REFA
C                                       If not found try another ref.
            IF (BADANT(ANT)) IREF = REF2
            IF (ANT.EQ.IREF) GO TO 300
            IF (SWT(ANT).LE.0.0) GO TO 300
C                                       See if already have soln.
            IF (REFLST(ANT).EQ.IREF) GO TO 300
C                                       Do solution
            CALL FRNSR2 (IFP, VREAL, VIMAG, TIME, FREQS, DT, DF, MAXFRQ,
     *         MAXTIM, NUMANT, NFRQ, NUMTIM, WGTMOD, WTBT, WTT, WTF,
     *         IREF, ANT, FREQIF(AIF), BLDO, MF, MT, NR, ND, APIAD,
     *         APINTR, APFIN, CREAL(IST,IIF,ANT), CIMAG(IST,IIF,ANT),
     *         CDELY(IST,IIF,ANT), CRATE(IST,IIF,ANT), CWT(IST,IIF,ANT))
C                                       If multiple baseline and SNR
C                                       too low, try 1 baseline.
            CWTTST = CWT(IST,IIF,ANT)
            ITEMP = 1
            IF ((BLDO.GT.1) .AND. (CWTTST.LT.SNRMIN))
     *         CALL FRNSR2 (IFP, VREAL, VIMAG, TIME, FREQS, DT, DF,
     *         MAXFRQ, MAXTIM, NUMANT, NFRQ, NUMTIM, WGTMOD, WTBT, WTT,
     *         WTF, IREF, ANT, FREQIF(AIF), ITEMP, MF, MT, NR, ND,
     *         APIAD, APINTR, APFIN, CREAL(IST,IIF,ANT),
     *         CIMAG(IST,IIF,ANT), CDELY(IST,IIF,ANT),
     *         CRATE(IST,IIF,ANT), CWT(IST,IIF,ANT))
            IF (CWTTST.GT.CWT(IST,IIF,ANT)) CWT(IST,IIF,ANT) = CWTTST
C                                       Save refence ant. used.
            REFAN(IST,IIF) = IREF
            IF (CWT(IST,IIF,ANT).GE.SNRMIN) REFLST(ANT) = IREF
C                                       Tell results if requested
            IF ((PRTLV.GE.2) .AND. (CDELY(IST,IIF,ANT).NE.FBLANK) .AND.
     *         (CRATE(IST,IIF,ANT).NE.FBLANK)) THEN
               IF (FRSMSG) THEN
                  MSGTXT = 'B = Baseline  R = Rate (mHz)  ' //
     *               'D = Delay (nsec)'
                  CALL MSGWRT (2)
                  FRSMSG = .FALSE.
                  END IF
               FDELAY = CDELY(IST,IIF,ANT) * 1.0E9
               FRATE = CRATE(IST,IIF,ANT) * 1000.0
               SNRAT = MIN (999.0, CWT(IST,IIF,ANT))
               WRITE (MSGTXT,2001,ERR=275) ANT, IREF, AIF, FRATE,
     *            FDELAY, SNRAT
 275           CALL MSGWRT (2)
               END IF
C                                        Check SNR.
            IF (CWT(IST,IIF,ANT).GE.SNRMIN) THEN
               GOODCT = GOODCT + 1
               GO TO 300
               END IF
C                                        Bad antenna.
            REDO = NUMPAS.EQ.1
            IF (BADANT(ANT)) THEN
               DO 280 I1 = 1,NUMBL
                  IF ((IS(I1).EQ.ANT) .OR. (JS(I1).EQ.ANT))
     *               WT1(I1) = 0.0
 280              CONTINUE
               END IF
            BADANT(ANT) = TRUE
C                                       Blank solution
            CREAL(IST,IIF,ANT) = FBLANK
            CIMAG(IST,IIF,ANT) = FBLANK
            CRATE(IST,IIF,ANT) = FBLANK
            CDELY(IST,IIF,ANT) = FBLANK
            CWT(IST,IIF,ANT) = 0.0
 300        CONTINUE
         IF ((CWT(IST,IIF,REF2).GT.SNRMIN)  .AND. (REFA.NE.REF2))
     *      THEN
C                                       Refer secondary to primary
C                                       reference.
            CPR = CREAL(IST,IIF,REF2)
            CPI = CIMAG(IST,IIF,REF2)
            DO 310 IA = 1,NUMANT
               IF (BADANT(IA) .AND. (CWT(IST,IIF,IA).GE.SNRMIN)) THEN
                  CTR = CREAL(IST,IIF,IA)
                  CTI = CIMAG(IST,IIF,IA)
                  CREAL(IST,IIF,IA) = CTR*CPR - CTI*CPI
                  CIMAG(IST,IIF,IA) = CTR*CPI + CTI*CPR
                  CDELY(IST,IIF,IA) = CDELY(IST,IIF,IA) +
     *               CDELY(IST,IIF,REF2)
                  CRATE(IST,IIF,IA) = CRATE(IST,IIF,IA) +
     *               CRATE(IST,IIF,REF2)
                  END IF
 310           CONTINUE
            REFAN(IST,IIF) = REFA
            END IF
C                                       Check if need to redo.
         IF (REDO) THEN
            IF (GOODCT.LT.1) THEN
C                                       Bad ref. antenna (?).
               ALLBAD = TRUE
               DO 350 I = 1,NUMBL
                  IF ((JS(I).EQ.REFA).OR.(IS(I).EQ.REFA)) WTBT(I) = 0.0
                  ALLBAD = ALLBAD .AND. (WTBT(I).LE.0.0)
 350              CONTINUE
C                                       See if any data LEFT
               IF (ALLBAD) GO TO 400
               DO 360 I = 1,NUMANT
                  BADANT(I) = FALSE
 360              CONTINUE
C                                       Blank bad reference antenna
               CREAL(IST,IIF,REFA) = FBLANK
               CIMAG(IST,IIF,REFA) = FBLANK
               CDELY(IST,IIF,REFA) = FBLANK
               CRATE(IST,IIF,REFA) = FBLANK
               CWT(IST,IIF,REFA) = 0.0
               TREFAN = 0
               CALL RCOPY (NUMBL, WTBT, WT1)
               END IF
C                                       Try again.
            GO TO 100
            END IF
C                                       End independent IF loop
 400     CONTINUE
C                                       Copy solns. to all IFs
      IF ((NUMIF.GT.1) .AND. (.NOT.DOIF)) THEN
         IF (DOEVLA.GT.0) THEN
            DO 420 NJ = DOEVLA,1,-1
               NIF1 = NUMIF/DOEVLA
               NIF2 = NJ * NIF1
               NIF1 = NIF2 - NIF1 + 1
               NIF1 = MAX (2, NIF1)
               DO 410 IIF = NIF1,NIF2
                  DO 405 IA = 1,NUMANT
                     CREAL(IST,IIF,IA) = CREAL(IST,NJ,IA)
                     CIMAG(IST,IIF,IA) = CIMAG(IST,NJ,IA)
                     CDELY(IST,IIF,IA) = CDELY(IST,NJ,IA)
                     CRATE(IST,IIF,IA) = CRATE(IST,NJ,IA)
                     CWT(IST,IIF,IA) = CWT(IST,NJ,IA)
 405                 CONTINUE
                  REFAN(IST,IIF) = REFAN(IST,NJ)
 410              CONTINUE
 420           CONTINUE
            END IF
         END IF
C                                       If combined Stokes, copy
      IF (IC.LE.0) THEN
         DO 485 IIF = 1,NUMIF
            DO 480 IA = 1,NUMANT
               CMBDEL(2,IA) = CMBDEL(1,IA)
               CREAL(2,IIF,IA) = CREAL(1,IIF,IA)
               CIMAG(2,IIF,IA) = CIMAG(1,IIF,IA)
               CDELY(2,IIF,IA) = CDELY(1,IIF,IA)
               CRATE(2,IIF,IA) = CRATE(1,IIF,IA)
               CWT(2,IIF,IA) = CWT(1,IIF,IA)
 480           CONTINUE
            REFAN(2,IIF) = REFAN(1,IIF)
 485        CONTINUE
         END IF
      GO TO 999
C                                       error
 900  MSGTXT = 'FRNSRC: DYNAMIC MEMORY AP FAILS'
      CALL MSGWRT (8)
      MSGTXT = 'FRNSRC: MEMORY TOO SMALL FOR SPECIFIED FFT SEARCH'
      CALL MSGWRT (8)
 910  MSGTXT = 'REDUCE DELAY AND/OR RATE WINDOW OR AVERAGE IN'
      CALL MSGWRT (8)
      MSGTXT = 'FREQUENCY OR USE A SHORTER SOLINT'
      CALL MSGWRT (8)
      IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('FRNSRC: MIN FFT IN FREQ REQUESTED',I8,' MAX ALLOWED',I7)
 1021 FORMAT ('FRNSRC: MIN FFT IN TIME REQUESTED',I8,' MAX ALLOWED',I7)
 1041 FORMAT ('Coarse search spacing:',E12.3,' ns;',E12.3,' mHz')
 2001 FORMAT ('Ant=',I3,' ref=',I3,' IF=',I3,' R=',F10.2,' D=',F11.1,
     *   ' SNR=',F6.1)
      END
      SUBROUTINE FRNSR2 (IFP, VREAL, VIMAG, TIME, FREQS, DT, DF, MAXFRQ,
     *   MAXTIM, NUMANT, NUMFRQ, NUMTIM, WGTMOD, WTB, WTT, WTF, REFAN,
     *   ANT, FREQIF, BLDO, MF, MT, NR, ND, APIAD, APINTR, APFIN, CREAL,
     *   CIMAG, CDELY, CRATE, CWT)
C-----------------------------------------------------------------------
C   Subroutine to solve for the delay, rate and phase of a given antenna
C   wrt a given reference antenna.
C    A coarse fringe search is done by FFTing freq-time data to the
C   delay-rate domain and searching for a maximum amplitude.
C   Interpolation of the solution is done by padding the data arrays
C   with zeroes (MT,MF) before the FFT.
C      Sensitivity is increased by (optionally) stacking data from
C   several baseline combinations before the search.  The returned
C   weight (CWT) is an approximation of the signal to noise ratio.
C   This approximation breaks down for large values of SNR (>50).
C     The data in VREAL and VIMAG are assumed to be evenly spaced in
C   time with increment DT.  The spacing in frequency is assumed to
C   be multiples of DF with FREQS(IFP) being the lowest frequency; the
C   spacing in frequency need otherwise not be uniform.
C   Input:
C      IFP      I                  First frequency number
C      VREAL    R(MAXTIM,MAXFRQ,*) Real part of visibility array
C      VIMAG    R(MAXTIM,MAXFRQ,*) Imag part of visibility array
C      TIME     R(*)               Time wrt center (days)
C      FREQS    D(*)               Frequency array, freq. increasing.
C                                  (GHz)
C      DT       R                  Time increment for search
C      DF       R                  Frequency increment for search.
C      MAXFRQ   I                  Maximum number of frequency channels.
C      MAXTIM   I                  Maximum number of time integrations.
C      NUMANT   I                  Number of antennas
C      NUMFRQ   I                  Number of frequencies
C      NUMTIM   I                  Number of times
C      WGTMOD   R(*)               Weight modification array; used if
C                                  unequal integration times in the data.
C      WTB      R(*)               Baseline weight array
C      WTT      R(*)               Time weight array
C      WTF      R(*)               Frequency weight array
C      REFAN    I                  Reference antenna to use.
C      ANT      I                  Antenna for solutions.
C      FREQIF   D                  IF Reference frequency offset (Hz)
C      BLDO     I                  No. baseline combinations to do 1,2,
C                                  or 3
C      MF       I                  No. freq. in search FFT
C      MT       I                  No. times in search FFT
C      NR       I                  No. delay channels to search
C      ND       I                  No. rate channels to search
C   Output:
C      CREAL    R                  Real part of solution
C      CIMAG    R                  Imag part of solution
C      CDELY    R                  delay in seconds
C      CRATE    R                  Rate in Hz
C      CWT      R                  Weight = SNR
C-----------------------------------------------------------------------
      INTEGER   IFP, MAXFRQ, MAXTIM, NUMANT, NUMFRQ, NUMTIM, REFAN, ANT,
     *   BLDO, MF, MT, NR, ND, APIAD, APINTR, APFIN
      REAL      VREAL(MAXTIM,MAXFRQ,*), VIMAG(MAXTIM,MAXFRQ,*),
     *   CREAL, CIMAG, CDELY, CRATE, CWT, TIME(*), DT, DF,
     *   WGTMOD(*), WTB(*), WTT(*), WTF(*)
      DOUBLE PRECISION FREQIF, FREQS(*)
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   JJ, II, KB1, KB2, KB3, J, J1, J2, J3, JLIM, IR, ID,
     *   IIAD, I1, I2, I3, KSTART, K, N2M1, JNF, JNT, I4TEMP, IAD, NT2,
     *   LIMF1, LIMF2, KBSAV
      LOGICAL   USEAP, GOOD
      REAL      VR1, VR2, VR3, VI1, VI2, VI3, W, SUMW, SUMWW, FAZ,
     *   TWOPI, XCOUNT, WORK(MAXIMG), S1, S2, S3, TFACT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:GAIN.INC'
C      EQUIVALENCE (SHESS, WORK)
C-----------------------------------------------------------------------
C                                       Make sure ANT .ne. REFAN
      IF (ANT.EQ.REFAN) GO TO 999
      TWOPI = 8.0 * ATAN (1.0)
      N2M1 = 2 * NUMANT - 1
      NT2 = 2 * NUMTIM
C                                       See if will use AP
      USEAP = (ND.GT.1) .OR. (NR.GT.1)
      XCOUNT = 0.0
      SUMW = 0.0
      SUMWW = 0.0
C                                        Zero AP memory
      IF (USEAP) THEN
         I4TEMP = ABS ((FREQS(IFP+NUMFRQ-1) - FREQS(IFP)) / DF) + 1.5
         I1 = (NUMTIM * I4TEMP * 2)
         CALL QVCLR (10, 1, I1)
         END IF
C                                       Begin loop in frequency
      LIMF1 = IFP
      LIMF2 = IFP + NUMFRQ - 1
      DO 400 JJ = LIMF1,LIMF2
         IF (WTF(JJ).LE.0.0) GO TO 400
C                                       Zero WORK.
         VR1 = 0.0
         CALL RFILL (NT2, VR1, WORK)
C                                       Single baseline.
         I1 = MIN (REFAN, ANT)
         J1 = MAX (REFAN, ANT)
         S1 = 1.0
         IF (REFAN.GT.ANT) S1 = -1.0
         KB1 = I1 * (N2M1 - I1) / 2 + J1 - NUMANT
         KBSAV = KB1
         IF (WTB(KB1).LE.0.0) GO TO 110
         DO 100 II = 1,NUMTIM
            GOOD = (ABS (VREAL(II,JJ,KB1)) + ABS (VIMAG(II,JJ,KB1)))
     *        .GT.1.0E-20
            IF (GOOD) THEN
               W = WTB(KB1) * WTT(II) * WTF(JJ)
               SUMWW = SUMWW + W * W
               SUMW = SUMW + W
               XCOUNT = XCOUNT + 1.0
               WORK(II*2-1) = WORK(II*2-1) + W * VREAL(II,JJ,KB1)
               WORK(II*2) = WORK(II*2) + W * VIMAG(II,JJ,KB1) * S1
               END IF
 100        CONTINUE
 110     CONTINUE
C                                       Double baseline combination.
C                                       Check if double baseline wanted.
         IF (BLDO.LE.1) GO TO 310
         DO 200 J = 1,NUMANT
            IF ((J.NE.ANT) .AND. (J.NE.REFAN)) THEN
            I1 = MIN (REFAN, J)
            J1 = MAX (REFAN, J)
            I2 = MIN (J, ANT)
            J2 = MAX (J, ANT)
            KB1 = I1 * (N2M1 - I1) / 2 + J1 - NUMANT
            KB2 = I2 * (N2M1 - I2) / 2 + J2 - NUMANT
            IF (WTB(KB1) * WTB(KB2).LE.0.0) GO TO 200
            S1 = 1.0
            S2 = 1.0
            IF (REFAN.GT.J) S1 = -1.0
            IF (J.GT.ANT) S2 = -1.0
            DO 190 II = 1,NUMTIM
               IF (WTT(II).GT.0.0) THEN
                  VR1 = VREAL(II,JJ,KB1)
                  VR2 = VREAL(II,JJ,KB2)
                  VI1 = VIMAG(II,JJ,KB1) * S1
                  VI2 = VIMAG(II,JJ,KB2) * S2
                  GOOD = ((ABS (VR1) + ABS (VI1)) * (ABS (VR2) +
     *               ABS (VI2))).GT.1.0E-20
                  IF (GOOD) THEN
                     W = (1.0 / (WTB(KB1) * WTT(II) * WTF(JJ))) +
     *                  (1.0 / (WTB(KB2) * WTT(II) * WTF(JJ)))
                     W = 1.0 / W
                     SUMWW = SUMWW + W * W
                     SUMW = SUMW + W
                     XCOUNT = XCOUNT + 1.0
                     WORK(II*2-1) = WORK(II*2-1) + W * (VR1*VR2 - VI1
     *                  *VI2)
                     WORK(II*2) = WORK(II*2) + W * (VR1*VI2 + VR2*VI1)
                     END IF
                  END IF
 190           CONTINUE
               END IF
 200        CONTINUE
C                                        Triple baseline combination.
         JLIM = NUMANT - 1
C                                        Check if triple bl wanted.
         IF (BLDO.LE.2) GO TO 310
         DO 300 J = 1,JLIM
            IF ((J.EQ.ANT) .OR. (J.EQ.REFAN)) GO TO 300
            KSTART = J + 1
            DO 290 K = KSTART,NUMANT
               IF ((K.EQ.ANT) .OR. (K.EQ.REFAN)) GO TO 290
               I1 = MIN (REFAN, J)
               J1 = MAX (REFAN, J)
               I2 = MIN (J, K)
               J2 = MAX (J, K)
               I3 = MIN (K, ANT)
               J3 = MAX (K, ANT)
               KB1 = I1 * (N2M1 - I1) / 2 + J1 - NUMANT
               KB2 = I2 * (N2M1 - I2) / 2 + J2 - NUMANT
               KB3 = I3 * (N2M1 - I3) / 2 + J3 - NUMANT
               IF (WTB(KB1) * WTB(KB2) * WTB(KB3).LE.0.0) GO TO 290
               S1 = 1.0
               S2 = 1.0
               S3 = 1.0
               IF (REFAN.GT.J) S1 = -1.0
               IF (J.GT.K) S2 = -1.0
               IF (K.GT.ANT) S3 = -1.0
               DO 280 II = 1,NUMTIM
                  VR1 = VREAL(II,JJ,KB1)
                  VR2 = VREAL(II,JJ,KB2)
                  VR3 = VREAL(II,JJ,KB3)
                  VI1 = VIMAG(II,JJ,KB1) * S1
                  VI2 = VIMAG(II,JJ,KB2) * S2
                  VI3 = VIMAG(II,JJ,KB3) * S3
                  GOOD = ((ABS (VR1) + ABS (VI1)) * (ABS (VR2) +
     *               ABS (VI2)) * (ABS(VR3) + ABS (VI3))).GT.1.0E-20
                  IF (GOOD) THEN
                     W = (1.0 / (WTB(KB1) * WTT(II) * WTF(JJ))) +
     *                   (1.0 / (WTB(KB2) * WTT(II) * WTF(JJ))) +
     *                   (1.0 / (WTB(KB3) * WTT(II) * WTF(JJ)))
                     W = 1.0 / W
                     SUMWW = SUMWW + W * W
                     SUMW = SUMW + W
                     XCOUNT = XCOUNT + 1.0
                     WORK(II * 2-1) = WORK(II * 2-1) + W *
     *                  (VR3 * (VR1*VR2 - VI1*VI2) -
     *                  VI3 * (VR1*VI2 + VR2*VI1))
                     WORK(II * 2) = WORK(II * 2) + W *
     *                  (VI3 * (VR1*VR2 - VI1*VI2) +
     *                  VR3 * (VR1*VI2 + VR2*VI1))
                     END IF
 280              CONTINUE
 290           CONTINUE
 300        CONTINUE
C                                       End of frequency loop
 310     NT2 = 2 * NUMTIM
C                                       Get correct frequency bin.
         I4TEMP = ABS ((FREQS(JJ) - FREQS(IFP)) / DF) + 0.5
         IAD = (NUMTIM * I4TEMP * 2) + APIAD
C                                        Shove freq. slice into AP
         IF (USEAP) CALL QPUT (WORK, IAD, NT2, 2)
 400     CONTINUE
C                                       Make sure that there was data
      CWT = 0.0
      IF (XCOUNT.LE.0.0) GO TO 999
C                                       Finished sums - do search
      IF (USEAP) THEN
         CALL QWD
C                                       Fringe search.
         JNF = NUMFRQ
         JNT = NUMTIM
         CALL QSEARC (JNF, JNT, MF, MT, ND, NR, APIAD, APINTR, APFIN)
         CALL QWR
C                                       Get results.
         CALL QGET (WORK, 1, 2, 2)
         CALL QGET (IIAD, 0, 1, 1)
         CALL QWD
         IAD = IIAD
         IF (IIAD.LT.0) IAD = 65536 - IIAD
C                                       1 position search, don't bother
C                                       with FFTs etc.
      ELSE
         FAZ = ATAN2 (WORK(2), WORK(1)+1.0E-20)
         WORK(1) = SQRT (WORK(1)*WORK(1) + WORK(2)*WORK(2))
         WORK(2) = FAZ
         IAD = 1
         END IF
C                                          Compute antenna delay
C                                          rate and phase.
      IR = ((IAD-1) / ND)
      IF (IR.GT.(NR/2)) IR = IR - NR
      IF (NR.EQ.1) IR = 0
      ID = MOD (IAD, ND) - 1
      IF (ID.GT.(ND/2)) ID = ID - ND
      IF (ND.EQ.1) ID = 0
      FAZ = -WORK(2)
      CRATE = (IR / (DT * MT))
      CDELY = ID / (DF * MF)
C                                       Have used TIME(1),FREQS(IFP)
C                                       as the reference.
      FAZ = FAZ - (TIME(1) * CRATE +
     *   (FREQS(IFP) - FREQIF*1.0D-9) * CDELY) * TWOPI
      CREAL = COS (FAZ)
      CIMAG = SIN (FAZ)
      CRATE = CRATE / 86400.0
C                                       Convert delay to sec.
      CDELY = CDELY * 1.0E-9
C                                       Compute SNR.
      CWT = 0.0
      IF (WORK(1).GT.(0.999*SUMW)) WORK(1) = SUMW * 0.999
      IF ((WORK(1).GT.0.0) .AND. (SUMW.GT.0.0))
     *   CWT = (TAN (1.570796 * WORK(1) / SUMW) ** 1.163) *
     *      SQRT (SUMW / SQRT (SUMWW / XCOUNT))
C                                       Adjust for unequal integration
C                                       times in the data
      TFACT = MAX (INT (WGTMOD(KBSAV)), 1)
      CWT = CWT / SQRT (TFACT)
C
 999  RETURN
      END
      SUBROUTINE FRNDRP (IS, JS, VREAL, VIMAG, TIMB, FREQS, FREQIF,
     *   CMBDEL,  CREAL, CIMAG, CDELY, CRATE, CWT, REFAN, MAXFRQ,
     *   MAXTIM, MAXIFS, MAXBL, NUMANT, NUMBL, NUMFRQ, NUMTIM, NUMIF,
     *   DOIF, DOEVLA, WTB, WTT, WTF, DELWIN, RATWIN, IC, SNRMIN, PRTLV,
     *   GDSOLV, RINWIN)
C-----------------------------------------------------------------------
C   FRNDRP does least squares solutions for delay and rate
C   The input values of CREAL,CIMAG,CDELY, and CRATE are the initial
C   guess.
C   Input:
C    IS(*)                       I    First ant. of baseline numbers
C    JS(*)                       I    2nd ant. of baseline numbers
C    VREAL(MAXTIM,MAXFRQ,MAXBL)  R    Real part of visibility array
C    VIMAG(MAXTIM,MAXFRQ,MAXBL)  R    Imag part of visibility array
C    TIMB(MAXTIM,MAXBL)  R    Time wrt center
C    FREQS(*)            D    Frequency array
C    FREQIF(*)           D    Reference frequency offset per IF (Hz)
C    MAXTIM              I    Maximum number of time integrations.
C    MAXFRQ              I    Maximum number of frequency channels.
C    MAXIFS              I    Maximum number of IFs
C    MAXBL               I    Maximum number of baselines
C    NUMANT              I    Number of antennas
C    NUMBL               I    Number of baselines
C    NUMFRQ              I    Number of frequencies
C    NUMTIM              I    Number of times
C    NUMIF               I    Number of IFS
C    DOIF                L    If true do each IF independently, else
C                             data averaged.
C    WTT(NUMTIM)         R    Time weight array
C    WTF(NUMFRQ)         R    Frequency weight array
C    WTB(NUMBL)          R    Baseline weight array
C    DELWIN              R    delay window, <0 => no search in delay
C    RATWIN              R    rate window, <0 => no search in rate
C    IC                  I    Stokes' number passed, 0 => averaged.
C                             1 = R, 2 = L, 3 = I
C    SNRMIN              R    Minimum SNR allowed
C    PRTLV               I    Print level, prints results at .ge. 2
C    RINWIN              R    Real input delay window, will be negative
C                             if only one channel selected. Needed to
C                             avoid non-convergence problems.
C   Input/Output:
C    CREAL(2,NUMIF,NUMANT)       R    Real part of solution
C    CIMAG(2,NUMIF,NUMANT)       R    Imag part of solution
C    CDELY(2,NUMIF,NUMANT)       R    delays in seconds
C    CRATE(2,NUMIF,NUMANT)       R    Rates in Hz.
C    CWT(2,NUMIF,NUMANT)         R    Weights = SNR
C    REFAN(2,NUMIF)              I    Reference antennas used
C   Output:
C    CMBDEL(2,NUMANT)            I    Multiband delays in seconds.
C-----------------------------------------------------------------------
      INTEGER   MAXFRQ, MAXTIM, MAXIFS, MAXBL, IS(*), JS(*), NUMBL,
     *   NUMFRQ, NUMTIM, NUMANT, NUMIF, DOEVLA, IC, PRTLV, REFAN(2,*)
      REAL      VREAL(MAXTIM,MAXFRQ,*), VIMAG(MAXTIM,MAXFRQ,*),
     *   CMBDEL(2,*), CREAL(2,MAXIFS,*), CIMAG(2,MAXIFS,*),
     *   CDELY(2,MAXIFS,*), CRATE(2,MAXIFS,*), CWT(2,MAXIFS,*),
     *   TIMB(MAXTIM,MAXBL), WTB(*), WTT(*), WTF(*), DELWIN,
     *   RATWIN, SNRMIN, RINWIN
      DOUBLE PRECISION FREQIF(*), FREQS(*)
      LOGICAL   DOIF, GDSOLV(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LDH, NA3, ITMAX, IANT, IST, IIF, NOIF, IFP, REFANT, I,
     *   I1, I2, I3, II, II1, II2, III, J, JJ, JJ1, JJ2, JJJ, K, KK,
     *   NFPIF, NFRQ, NANT, NANT2, NANT3, NIT, NA2, IT, INDX, ISI, JSI,
     *   ID2, IER, ILIM, LIMF1, LIMF2, JX, IFP1, NJ, NIF1, NIF2, AIF,
     *   INERT(3)
      LOGICAL   FLGRAT, FLGDEL, KFATI, KFATJ
      REAL      TWOPI,
     *   XTR, XTI, PSTD(2), FREQX(MAXCIF), RATE, DELAY, SNR, PHASE,
     *   X1R, X1I, X2R, X2I, X12R, X12I, X3R, X3I, PHAZ
      DOUBLE PRECISION SL, S, WT, X1, X2, RNOBS, SUMWT, X3, CX, SX,
     *   G1, G2, G3, D11, D12, D13, D22, D23, D33, GN, W, TOL,
     *   SIGMA2, RMS, RCOND, DET(2)
      INCLUDE 'INCS:GAIN.INC'
      INTEGER   KANT(MAXANT), IKANT(MAXANT), KPVT(MAXPRM)
      REAL      SWT(MAXANT)
      DOUBLE PRECISION YPRM(MAXPRM,MAXIF), FTEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA TOL/1.0D-4/
C-----------------------------------------------------------------------
      LDH = MAXPRM
C                                       Set solution flags
      FLGRAT = RATWIN .LT. 0.0
      FLGDEL = DELWIN .LT. 0.0
      FLGDEL = RINWIN .LT. 0.0
      ITMAX = 50
      SL = 1.0D30
      TWOPI = 8.0 * ATAN (1.0)
      NANT2 = 2 * NUMANT
      NANT3 = 3 * NUMANT
      IST = MAX (IC, 1)
C                                       IQUV?
      IF (IC.EQ.3) IST = 1
C                                       Set prior dist. widths.
CCC      PSTD(1) = 0.5 * RATWIN * 5.4286721E2
CCC      PSTD(2) = 0.5 * DELWIN * 6.283185308
C                                       Non-zero penalty terms caused
C                                       the solution to be biased
C                                       towards zero when PSTD dominated
C                                       over the Chi-squared
      PSTD(1) = 0.0
      PSTD(2) = 0.0
C                                       Set up frequency stuff
      IF (DOIF) THEN
         NOIF = NUMIF
         NFRQ = NUMFRQ / NUMIF
      ELSE IF (DOEVLA.GT.0) THEN
         NOIF = DOEVLA
         NFRQ = NUMFRQ / DOEVLA
      ELSE
         NOIF = 1
         NFRQ = NUMFRQ
         END IF
      NFPIF = NFRQ
C                                       Loop over independent IFs
      DO 800 IIF = 1,NOIF
         AIF = IIF
         IF (DOEVLA.GT.1) AIF = (IIF - 1) * (NUMIF / DOEVLA) + 1
C                                       correction for DOEVLA
C                                       move data back to low IF
         IF (AIF.NE.IIF) THEN
            DO 5 IANT = 1,NUMANT
               CREAL(IST,IIF,IANT) = CREAL(IST,AIF,IANT)
               CIMAG(IST,IIF,IANT) = CIMAG(IST,AIF,IANT)
               CDELY(IST,IIF,IANT) = CDELY(IST,AIF,IANT)
               CRATE(IST,IIF,IANT) = CRATE(IST,AIF,IANT)
               CWT(IST,IIF,IANT) = CWT(IST,AIF,IANT)
 5             CONTINUE
            REFAN(IST,IIF) = REFAN(IST,AIF)
            END IF
         REFANT = REFAN(IST,IIF)
C                                       IF pointer.
         IFP = (IIF-1) * NFPIF
         LIMF1 = IFP + 1
         LIMF2 = IFP + NFRQ
C                                       Determine which antennas have
C                                       data:
         DO 10 I = 1,NUMANT
            SWT(I) = 0.0
 10         CONTINUE
         RNOBS = 0.0
         SUMWT = 0.0
         DO 40 I = 1,NUMBL
            IF (WTB(I).EQ.0.) GO TO 40
            ISI = IS(I)
            JSI = JS(I)
            DO 30 J = LIMF1,LIMF2
C                                       Use freq offset in IF.
               JX = J - LIMF1 + 1
               FREQX(JX) = FREQS(J) - FREQS(LIMF1)
               IF (WTF(J).LE.0.0) GO TO 30
               DO 20 K = 1,NUMTIM
                  WT = WTB(I) * WTF(J) * WTT(K)
                  IF ((ABS (VREAL(K,J,I)) + ABS (VIMAG(K,J,I))) .LE.
     *               1.0E-20) WT = 0.0
                  SWT(ISI) = SWT(ISI) + WT
                  SWT(JSI) = SWT(JSI) + WT
                  IF (WT.GT.1.0E-20) RNOBS = RNOBS + 1.0D0
                  SUMWT = SUMWT + WT
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
         NANT = 0
C                                       Get lists of good antennas other
C                                       than the reference antenna.
         CALL FILL (MAXANT, -1, KANT)
         CALL FILL (MAXANT, -1, IKANT)
         DO 50 IANT = 1,NUMANT
            IF ((SWT(IANT).GT.1.0E-20) .AND.
     *         (CWT(IST,IIF,IANT).GT.0.0001)) THEN
C                                       if subset solve and nosolve for I
               IF ((GDSOLV(1).AND.GDSOLV(1+IANT)).OR.(IANT.EQ.REFANT))
     *            THEN
C                                       then IANT is neutral
                  IKANT(IANT) = 0
               ELSE
C                                       otherwise IANT is good
                  NANT = NANT + 1
                  KANT(NANT) = IANT
                  IKANT(IANT) = NANT
                  END IF
               END IF
 50         CONTINUE
         NA2 = 2 * NANT
         NA3 = 3 * NANT
         IF (FLGDEL) NA3 = NA2
C                                       Be sure the problem is
C                                       constrained and arrays are large
C                                       enough.
         IF (((RNOBS.LE.(NA3+2)) .OR. (NANT.EQ.0)) .OR. (NA3.GT.MAXPRM))
     *      THEN
C                                       Message if arrays aren't large
C                                       enough
            IF (NA3.GT.MAXPRM) THEN
               WRITE (MSGTXT,1050) NA3
               CALL MSGWRT (7)
               MSGTXT = 'SOLUTION WAS SKIPPED - FLAGGED BAD'
               CALL MSGWRT (7)
               END IF
C                                       Flag solution and skip
            DO 55 IANT = 1,NUMANT
               CREAL(IST,IIF,IANT) = FBLANK
               CIMAG(IST,IIF,IANT) = FBLANK
               CDELY(IST,IIF,IANT) = FBLANK
               CRATE(IST,IIF,IANT) = FBLANK
               CWT(IST,IIF,IANT) = 0.0
 55            CONTINUE
            GO TO 790
            END IF
C                                       Transfer initial guesses to
C                                       internal array:
         DO 60 IANT = 1,NUMANT
            STEP(IANT) = 0.0
            STEP(IANT+NUMANT) = 0.0
            STEP(IANT+NANT2) = 0.0
            XPRM(IANT) = 0.0
            XPRM(IANT+NUMANT) = 0.0
            XPRM(IANT+NANT2) = 0.0
            IF (SWT(IANT).GT.0.0) THEN
C         IF ((SWT(IANT).GT.0.0).AND.(CWT(IST,IIF,IANT).GT.0.0)) THEN
C                                       This additional test is required
C                                       to prevent CDELY=FBLANK from
C                                       being used.  All tests for FBLANK
C                                       below should be redundant now.
               IF ((CIMAG(IST,IIF,IANT).NE.FBLANK) .AND.
     *            (CREAL(IST,IIF,IANT).NE.FBLANK)) XPRM(IANT) =
     *            ATAN2 (CIMAG(IST,IIF,IANT), CREAL(IST,IIF,IANT)) +
C                                       Delay correction from ref. chan.
     *            (FREQS(IFP+1) * 1.0D9 - FREQIF(AIF)) *
     *            CDELY(IST,IIF,IANT) * TWOPI
               IF (CRATE(IST,IIF,IANT).NE.FBLANK) XPRM(NUMANT+IANT) =
     *            CRATE(IST,IIF,IANT) * TWOPI * 86400.0
               IF (CDELY(IST,IIF,IANT).NE.FBLANK) XPRM(NANT2+IANT) =
     *            CDELY(IST,IIF,IANT) * TWOPI * 1.0E9
               END IF
C                                       In case it doesn't converge.
            CREAL(IST,IIF,IANT) = FBLANK
            CIMAG(IST,IIF,IANT) = FBLANK
            CDELY(IST,IIF,IANT) = FBLANK
            CRATE(IST,IIF,IANT) = FBLANK
            CWT(IST,IIF,IANT) = 0.0
 60         CONTINUE
         XPRM(       REFANT) = 0.0
         XPRM(NUMANT+REFANT) = 0.0
         XPRM(NANT2 +REFANT) = 0.0
C                                       Iterate:
         DO 500 IT = 1,ITMAX
            NIT = IT
C                                       Zero the gradient and Hessian
C                                       arrays:
            DO 71 I = 1,NA3
               GRAD(I) = 0.0D0
               DO 70 J = 1,NA3
                  HESS(I,J) = 0.0D0
 70               CONTINUE
 71            CONTINUE
C                                       Initial value of function
C                                       minimized.
            W = 0.0D0
            IFP1 = IFP + 1
            IF (IT.EQ.1) CALL SEVAL (IFP1, XPRM, W, STEP, IS, JS,
     *         VREAL, VIMAG, TIMB, FREQX, MAXTIM, MAXFRQ, MAXBL, NUMANT,
     *         NUMBL, NFRQ, NUMTIM, WTB, WTT, WTF, PSTD, REFANT, PRTLV,
     *         SL, GWORK)
C                                       Accumulate (half) the gradient
C                                       of S and (half) the Hessian of
C                                       S:
            DO 200 I = 1,NUMBL
C                                       Data valid?
               IF (WTB(I).EQ.0.) GO TO 200
               III = IS(I)
               JJJ = JS(I)
               II = IKANT(III)
               JJ = IKANT(JJJ)
C                                       if both ends are bad
C                                       then skip this baseline
               IF ((II.LT.0).OR.(JJ.LT.0)) GO TO 200
               II1 = NANT + II
               II2 = NANT + II1
               JJ1 = NANT + JJ
               JJ2 = NANT + JJ1
               X1    = XPRM(III) - XPRM(JJJ)
C                                       only calculate derivatives for
C                                       those ends that are explicitly good
               KFATI = II.GT.0
               KFATJ = JJ.GT.0
               X1R = COS (X1)
               X1I = SIN (X1)
               DO 190 J = LIMF1,LIMF2
                  JX = J - LIMF1 + 1
                  IF (WTF(J).EQ.0.) GO TO 190
                  X2 = (XPRM(III+NANT2) - XPRM(JJJ+NANT2)) * FREQX(JX)
                  X2R = COS (X2)
                  X2I = SIN (X2)
                  X12R = X1R*X2R - X1I*X2I
                  X12I = X1R*X2I + X1I*X2R
                  DO 140 K = 1,NUMTIM
                     WT = WTB(I) * WTF(J) * WTT(K)
C                                        Check for blanking.
                     IF ((ABS (VREAL(K,J,I)) + ABS (VIMAG(K,J,I)))
     *                  .LE.1.0E-20) WT = 0.0
                     X3 = (XPRM(III+NUMANT) - XPRM(JJJ+NUMANT)) *
     *                  TIMB(K,I)
                     X3R = COS (X3)
                     X3I = SIN (X3)
                     XTR = X3R*X12R - X3I*X12I
                     XTI = X3I*X12R + X3R*X12I
                     CX = WT * (VREAL(K,J,I) * XTR + VIMAG(K,J,I) * XTI)
                     SX = WT * (VIMAG(K,J,I) * XTR - VREAL(K,J,I) * XTI)
                     G1 = -SX
                     G2 = -TIMB(K,I) * SX
                     G3 = -FREQX(JX) * SX
                     D11 = CX
                     D12 = TIMB(K,I) * CX
                     D13 = FREQX(JX) * CX
                     D22 = TIMB(K,I) * D12
                     D23 = FREQX(JX) * D12
                     D33 = FREQX(JX) * D13
C                                       This code is still being tested
C                                       it is commented out of the check'ed
C                                       in version.
C
C                                       This takes care of zero'ing
C                                       terms that would cause the rate to
C                                       be solved for
C                  IF (FLGRAT) THEN
C                     G2  = 0.0
C                     D12 = 0.0
C                     END IF
C                                       This takes care of zero'ing
C                                       terms that would cause the delay to
C                                       be solved for
C                  IF (FLGDEL) THEN
C                     G3  = 0.0
C                     D13 = 0.0
C                     D23 = 0.0
C                     D33 = 0.0
C                     END IF
                     IF (KFATI) THEN
                        GRAD(II ) = GRAD(II ) +  G1
                        GRAD(II1) = GRAD(II1) +  G2
                        GRAD(II2) = GRAD(II2) +  G3
                        HESS(II , II ) = HESS(II , II ) + D11
                        HESS(II , II1) = HESS(II , II1) + D12
                        HESS(II , II2) = HESS(II , II2) + D13
                        HESS(II1, II1) = HESS(II1, II1) + D22
                        HESS(II1, II2) = HESS(II1, II2) + D23
                        HESS(II2, II2) = HESS(II2, II2) + D33
                        END IF
                     IF (KFATJ) THEN
                        GRAD(JJ ) = GRAD(JJ ) -  G1
                        GRAD(JJ1) = GRAD(JJ1) -  G2
                        GRAD(JJ2) = GRAD(JJ2) -  G3
                        HESS(JJ , JJ ) = HESS(JJ , JJ ) + D11
                        HESS(JJ , JJ1) = HESS(JJ , JJ1) + D12
                        HESS(JJ , JJ2) = HESS(JJ , JJ2) + D13
                        HESS(JJ1, JJ1) = HESS(JJ1, JJ1) + D22
                        HESS(JJ1, JJ2) = HESS(JJ1, JJ2) + D23
                        HESS(JJ2, JJ2) = HESS(JJ2, JJ2) + D33
                        END IF
                     IF (KFATI.AND.KFATJ) THEN
                        HESS(II , JJ ) = HESS(II , JJ ) - D11
                        HESS(II , JJ1) = HESS(II , JJ1) - D12
                        HESS(II , JJ2) = HESS(II , JJ2) - D13
                        HESS(JJ , II1) = HESS(JJ , II1) - D12
                        HESS(JJ , II2) = HESS(JJ , II2) - D13
                        HESS(II1, JJ1) = HESS(II1, JJ1) - D22
                        HESS(II1, JJ2) = HESS(II1, JJ2) - D23
                        HESS(JJ1, II2) = HESS(JJ1, II2) - D23
                        HESS(II2, JJ2) = HESS(II2, JJ2) - D33
                       END IF
 140                 CONTINUE
 190              CONTINUE
 200           CONTINUE
C                                       Fill in the lower triangular
C                                       part of the Hessian:
            DO 211 I = 2,NA3
               ILIM = I - 1
               DO 210 J = 1,ILIM
                  HESS(I,J) = HESS(J,I)
 210              CONTINUE
 211           CONTINUE
C                                       Add constraint penalty terms
C                                       Constrain about 0 delay, rate
C                                       Rate
            IF ((.NOT.FLGRAT) .AND. (PSTD(1).GT.0.0)) THEN
               ILIM = NANT + 1
               DO 220 I = ILIM,NA2
                  INDX = KANT(I-NANT)
                  GRAD(I) = GRAD(I) + 0.1 * SUMWT * (XPRM(INDX+NUMANT))
     *               / PSTD(1)**2
                  HESS(I,I) = HESS(I,I) + 0.1 * SUMWT / PSTD(1)**2
 220              CONTINUE
               END IF
C                                       Delay
            IF ((.NOT.FLGDEL) .AND. (PSTD(2).GT.0.0)) THEN
               ILIM = NA2 + 1
               DO 240 I = ILIM,NA3
                  INDX = KANT(I-NA2)
                  GRAD(I) = GRAD(I) + 0.1 * SUMWT * (XPRM(INDX+NANT2)) /
     *               PSTD(2)**2
                  HESS(I,I) = HESS(I,I) + 0.1 * SUMWT / PSTD(2)**2
 240              CONTINUE
               END IF
C                                       the proper terms have been zeroed
C                                       already, the only thing left
C                                       to do is to fill in the diagonal
C                                       elements
C
C                                       Zero terms not being solved for
C                                       FLGRAT = rate
            IF (FLGRAT) THEN
               ILIM = NANT + 1
               DO 281 I = ILIM,NA2
                  GRAD(I) = 0.0
                  HESS(I,I) = 1.0
                  DO 280 J = 1,NA3
                     HESS(I,J) = 0.0
                     HESS(J,I) = 0.0
 280                 CONTINUE
 281              CONTINUE
               END IF
C                                       FLGDEL = delay
            IF (FLGDEL) THEN
               ILIM  =  NA2 + 1
               DO 301 I = ILIM,NA3
                  GRAD(I) = 0.0
                  HESS(I,I) = 1.0
                  DO 300 J = 1,NA3
                     HESS(I,J) = 0.0
                     HESS(J,I) = 0.0
 300                 CONTINUE
 301              CONTINUE
               END IF
C                                       This should be done when filling
C                                       in the other part of the hessian
C
C                                       Save the Hessian, in case that
C                                       it's indefinite:
            DO 321 I = 1,NA3
               DO 320 J = 1,NA3
                  SHESS(I,J) = HESS(I,J)
 320              CONTINUE
 321           CONTINUE
C                                       Calculate and print the
C                                       Euclidean norm of the gradient
C                                       in order to monitor the
C                                       progress toward a critical
C                                       point:
            IF (PRTLV.GE.3) THEN
               CALL DNRM2 (NA3, GRAD, 1, GN)
               WRITE (MSGTXT,1000) IT, GN
               CALL MSGWRT (3)
               END IF
C                                       Factor the Hessian and obtain
C                                       an estimate of its cond. no.:
            CALL DSICO (HESS, LDH, NA3, KPVT, RCOND, GWORK)
            IF (PRTLV.GE.3) THEN
               WRITE (MSGTXT,1001) RCOND
               CALL MSGWRT (3)
               END IF
C                                       Compute determinant and
C                                       inertia:
            CALL DSIDI (HESS, LDH, NA3, KPVT, DET, INERT, GWORK, 110)
            ID2 = DET(2)
            IF (PRTLV.GE.3) THEN
               WRITE (MSGTXT,1002) DET(1), ID2, INERT
               CALL MSGWRT (3)
               END IF
C                                       If the Hessian is indefinite or
C                                       singular use Greenstadt's
C                                       modification:
            IF ((INERT(2).NE.0) .OR. (RCOND.EQ.0.D0)) THEN
               CALL GM (NA3, SHESS, HESS, GWORK, LDH, GRAD, IER)
            ELSE
C                                       Solve for the Newton correction
C                                       (to be placed in GRAD):
               CALL DSISL (HESS, LDH, NA3, KPVT, GRAD)
               END IF
C                                       Take a damped Newton step:
            DO 360 I = 1,NANT
               J = KANT(I)
               STEP(J) = GRAD(I)
               STEP(NUMANT+J) = GRAD(NANT+I)
               STEP(NANT2+J) = GRAD(NA2+I)
 360           CONTINUE
            W = 0.8
            IF (INERT(2).EQ.0) W = 1.0
            IF (IT.LE.2) W = 0.1
C                                       Find step size that improves
C                                       soln.
            IFP1 = IFP + 1
            DO 370 KK = 1,6
               CALL SEVAL (IFP1, XPRM, W, STEP, IS, JS, VREAL, VIMAG,
     *            TIMB, FREQX, MAXTIM, MAXFRQ, MAXBL, NUMANT, NUMBL,
     *            NFRQ, NUMTIM, WTB, WTT, WTF, PSTD, REFANT, PRTLV, S,
     *            GWORK)
               IF (S.LT.SL) GO TO 380
               W = 0.25 * W
 370           CONTINUE
C                                       Can't improve it, quit.
            GO TO 510
C                                       Convergence test
 380        IF (ABS (1.0D0 - S/SL).LE.5.0D-6) GO TO 510
C                                       Prepare for next iteration
            SL = S
            DO 390 I = 1,NANT3
               XPRM(I) = XPRM(I) - W * STEP(I)
 390           CONTINUE
C                                       See if solution changing
            DO 400 I = 1,NANT3
               IF (ABS (STEP(I)).GT.(ABS( XPRM(I))+1.0D-3)*TOL)
     *            GO TO 500
 400           CONTINUE
C                                       Not changing - quit.
            GO TO 510
C                                       End of iteration loop
 500        CONTINUE
C                                       Calculate std. errors:
 510     IF (INERT(1).LT.NA3) THEN
            WRITE (MSGTXT,1510) IIF
            CALL MSGWRT (7)
            MSGTXT = ' This probably means that the starting value for'
     *         // ' the'
            CALL MSGWRT (4)
            MSGTXT = 'delay or rate for one or more antennae is bad.'
     *         // '  You'
            CALL MSGWRT (4)
            MSGTXT = ' may want to set search windows and try again.'
            CALL MSGWRT (4)
            GO TO 790
            END IF
         SIGMA2 = S / (RNOBS - NA3) * RNOBS / SUMWT
         RMS = SQRT (SIGMA2)
         IF (PRTLV.GE.3)  THEN
            WRITE (MSGTXT,1700) IIF, RMS
            CALL MSGWRT (3)
            END IF
         CALL DSIDI (HESS, LDH, NA3, KPVT, DET, INERT, GWORK, 1)
         SIGMA2 = MAX (SIGMA2, 1.0D-25)
C                                       Ensure that delays are zero if
C                                       no delay search being done.
         IF (FLGDEL) CALL DFILL (NUMANT, 0.0D0, XPRM(NANT2+1))
C                                       Refer the phases to the
C                                       reference channel.
         DO 515 IANT = 1,NUMANT
            XPRM(IANT) = XPRM(IANT) -
     *         (XPRM(NANT2+IANT) - XPRM(NANT2+REFANT)) *
     *         (FREQS(IFP+1) - FREQIF(AIF)*1.0D-9)
 515        CONTINUE
C                                       Before returning, save results
         DO 520 IANT = 1,NUMANT
            IF (SWT(IANT).LE.0.0) GO TO 520
            CREAL(IST,IIF,IANT) = COS (XPRM(IANT) - XPRM(REFANT))
            CIMAG(IST,IIF,IANT) = SIN (XPRM(IANT) - XPRM(REFANT))
            CDELY(IST,IIF,IANT) = ((XPRM(NANT2+IANT) -
     *         XPRM(NANT2+REFANT)) / TWOPI) * 1.0E-9
            CRATE(IST,IIF,IANT) = (XPRM(NUMANT+IANT) -
     *         XPRM(NUMANT+REFANT)) / (TWOPI * 86400.0)
C         IF (IANT.EQ.REFANT) CWT(IST,IIF,IANT) = SNRMIN + 1.0
            JJJ = IKANT(IANT)
            IF (JJJ.EQ.0) CWT(IST,IIF,IANT) = SNRMIN + 1.0
            IF ((JJJ.GT.0) .AND. (HESS(JJJ,JJJ).GT.0.0)) THEN
               CWT(IST,IIF,IANT) = SQRT (SIGMA2 * HESS(JJJ,JJJ) * 0.5)
               IF (CWT(IST,IIF,IANT).GT.1.0E-20)
     *            CWT(IST,IIF,IANT) = 1.0 / CWT(IST,IIF,IANT)
               END IF
C                                       Check min SNR
            IF (CWT(IST,IIF,IANT).LT.SNRMIN) THEN
               CREAL(IST,IIF,IANT) = FBLANK
               CIMAG(IST,IIF,IANT) = FBLANK
               CDELY(IST,IIF,IANT) = FBLANK
               CRATE(IST,IIF,IANT) = FBLANK
               CWT(IST,IIF,IANT) = 0.0
               END IF
 520        CONTINUE
         IF (PRTLV.GE.2) THEN
C                                        Print sigmas in deg,mHz,nsec.
            WRITE (MSGTXT,1520) IIF, IST
            CALL MSGWRT (3)
            MSGTXT = 'Fitted phases, rates, delays and SNR: [ P =' //
     *         ' phase(deg),'
            CALL MSGWRT (3)
            MSGTXT = '  R = rate(mHz), D = Single-Band Delay(nsec), ' //
     *         'S = SNR ]'
            CALL MSGWRT (3)
            DO 530 I = 1,NANT
               II = KANT(I)
               I1 = I
               I2 = NANT + I
               I3 = NA2 + I
               GWORK(I1) = SQRT (SIGMA2 * HESS(I1,I1) * 0.5) * 57.296
               IF (FLGRAT) THEN
                  GWORK(I2) = 0.0
               ELSE
                  GWORK(I2) =
     *               SQRT (SIGMA2 * HESS(I2,I2) * 0.5) * 1.8420711E-3
                   END IF
               IF (FLGDEL) THEN
                  GWORK(I3) = 0.0
               ELSE
                  GWORK(I3) =
     *               SQRT (SIGMA2 * HESS(I3,I3) * 0.5) * 1.59154E-1
                  END IF
C                                       Print rates and delays, SNR
               PHAZ = (XPRM(II) - XPRM(REFANT)) * 57.296
               RATE =  (XPRM(NUMANT+II) - XPRM(NUMANT+REFANT)) *
     *            1.8420711E-3
               DELAY = (XPRM(NANT2+II) - XPRM(NANT2+REFANT)) *
     *            1.59154E-1
               SNR = SQRT (SIGMA2 * HESS(I1,I1) * 0.5)
               SNR = MIN (9999.999, SNR)
               IF (SNR.GT.1.0E-20) SNR = 1.0 / SNR
               WRITE (MSGTXT,1530) II, PHAZ, RATE, DELAY, SNR
               CALL MSGWRT (3)
 530           CONTINUE
            MSGTXT = 'Standard RMS errors (deg, mHz, nsec):'
            CALL MSGWRT (3)
            DO 540 K = 1,NANT
               WRITE (MSGTXT,1602) KANT(K), GWORK(K), GWORK(K+NANT),
     *            GWORK(K+NA2)
               CALL MSGWRT (3)
 540           CONTINUE
            END IF
C                                       If combined Stokes, copy
         IF (IC.LE.0) THEN
            DO 560 IANT = 1,NUMANT
               CREAL(2,IIF,IANT) = CREAL(1,IIF,IANT)
               CIMAG(2,IIF,IANT) = CIMAG(1,IIF,IANT)
               CDELY(2,IIF,IANT) = CDELY(1,IIF,IANT)
               CRATE(2,IIF,IANT) = CRATE(1,IIF,IANT)
               CWT(2,IIF,IANT) = CWT(1,IIF,IANT)
 560           CONTINUE
            REFAN(2,IIF) = REFAN(1,IIF)
            END IF
C                                       End of IF loop
 790     IF (DOEVLA.GT.0) CALL DPCOPY (MAXPRM, XPRM, YPRM(1,IIF))
 800     CONTINUE
C                                       Copy solns. to all IFs
      IF ((NUMIF.GT.1) .AND. (.NOT.DOIF)) THEN
         IF (DOEVLA.GT.0) THEN
            DO 820 NJ = DOEVLA,1,-1
               NIF1 = NUMIF / DOEVLA
               NIF2 = NJ * NIF1
               NIF1 = NIF2 - NIF1 + 1
               FTEMP = FREQIF(NIF1)
               DO 810 IIF = NIF1,NIF2
                  DO 805 IANT = 1,NUMANT
C                                       used all bands so set multiband
                     IF (DOEVLA.EQ.1) CMBDEL(IST,IANT) =
     *                  CDELY(IST,1,IANT)
C                                       This is wrong, even if the
C                                       soln was FBLANKed due to low
C                                       SNR, CREAL and CIMAG get filled
C                                       in anyway.
C                                       Phase
                     PHASE = (YPRM(IANT,NJ) - YPRM(REFANT,NJ))
C                                       Multiband correction
     *                  + ((YPRM(IANT+NANT2,NJ) - YPRM(REFANT+NANT2,NJ))
     *                  * (FREQIF(IIF) - FTEMP)*1.0D-9)
                     CREAL(IST,IIF,IANT) = COS(PHASE)
                     CIMAG(IST,IIF,IANT) = SIN(PHASE)
                     CDELY(IST,IIF,IANT) = CDELY(IST,NJ,IANT)
                     CRATE(IST,IIF,IANT) = CRATE(IST,NJ,IANT)
                     CWT(IST,IIF,IANT) = CWT(IST,NJ,IANT)
 805                 CONTINUE
                  REFAN(IST,IIF) = REFAN(IST,NJ)
 810              CONTINUE
 820           CONTINUE
            END IF
        END IF
C                                       If combined Stokes, copy
      IF (IC.LE.0) THEN
         DO 840 IIF = 1,NUMIF
            DO 835 IANT = 1,NUMANT
               CMBDEL(2,IANT) = CMBDEL(1,IANT)
               CREAL(2,IIF,IANT) = CREAL(1,IIF,IANT)
               CIMAG(2,IIF,IANT) = CIMAG(1,IIF,IANT)
               CDELY(2,IIF,IANT) = CDELY(1,IIF,IANT)
               CRATE(2,IIF,IANT) = CRATE(1,IIF,IANT)
               CWT(2,IIF,IANT) = CWT(1,IIF,IANT)
 835           CONTINUE
            REFAN(2,IIF) = REFAN(1,IIF)
 840        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Iteration #',I4,'   Gradient norm=',1PE15.5)
 1001 FORMAT ('Reciprocal cond. no.=',1PE15.5)
 1002 FORMAT ('Det=',F15.5,'E',I4,'    inertia=',3I3)
 1050 FORMAT ('ERROR: TOO MANY PARAMETERS (',I4,') FOR INTERNAL ARRAYS')
 1510 FORMAT ('FIT DID NOT CONVERGE FOR IF ', I4)
 1520 FORMAT ('IF number = ',I4,' Poln. =',I4)
 1530 FORMAT ('Ant(',I2.2,'): Phas=',F6.1,' rate=',F10.2,' delay=',
     *   F10.2,' SNR=',F6.1)
 1602 FORMAT ('Ant(',I2.2,'): Phas=',F6.2,' rate=',F10.3,' delay=',
     *   F10.3)
 1700 FORMAT ('IF(',I3,') RMS residual=',F10.5)
      END
      SUBROUTINE SEVAL (IFP1, XPRM, W, STEP, IS, JS, VREAL, VIMAG, TIME,
     *   FREQ, MAXTIM, MAXFRQ, MAXBL, NUMANT, NUMBL, NUMFRQ, NUMTIM,
     *   WTB, WTT, WTF, PSTD, REFANT, PRTLV, S, WORK)
C-----------------------------------------------------------------------
C   This routine takes a specified step in the model parameters used
C   by FRNDRP and compute the function being minimized, i.e. the chi-
C   squared sum plus penalty terms to constrain the delay and rate
C   solution.
C   Inputs:
C     IFP1           I    First frequency pointer
C     XPRM(NUMANT,3) D    Delay, rate and phase current model parms.
C                         In order, phase, rate, delay
C     W              D    Step size to take, fraction of STEP
C     STEP(NUMANT,3) D    Step for each of XPRM.
C     IS(*)          I    First ant. of baseline numbers
C     JS(*)          I    2nd ant. of baseline numbers
C     VREAL(MAXTIM,MAXFRQ,MAXBL)  R    Real part of visibility array
C     VIMAG(MAXTIM,MAXFRQ,MAXBL)  R    Imag part of visibility array
C     TIME(MAXTIM,MAXBL)          R    Time wrt center
C     FREQ(MAXFRQ)   R    Frequency array (only those in current IF)
C     MAXTIM         I    Maximum number of time integrations.
C     MAXFRQ         I    Maximum number of frequency channels.
C     MAXBL          I    Maximum number of baselines.
C     NUMANT         I    Number of antennas
C     NUMBL          I    Number of baselines
C     NUMFRQ         I    Number of frequencies
C     NUMTIM         I    Number of times
C     WTB(MAXBL)     R    Baseline weight array
C     WTT(MAXTIM)    R    Time weight array
C     WTF(MAXFRQ)    R    Frequency weight array
C     PSTD(2)        R    Model constraints, (rate, delay)
C                        .LE.0.0 => no constraint.
C     REFANT         I    Reference antenna
C     PRTLV          I    Print level, gives some results if .ge. 2
C   Output:
C     S              D    Value of function being minimized, chi-squares
C                         sum plus penalty terms.
C     WORK(*)        D    A work array equal in size to XPRM.
C-----------------------------------------------------------------------
      INTEGER   IFP1, MAXTIM, MAXFRQ, MAXBL, IS(*), JS(*), NUMANT,
     *   NUMBL, NUMFRQ, NUMTIM, REFANT, PRTLV
      REAL      VREAL(MAXTIM,MAXFRQ,MAXBL), VIMAG(MAXTIM,MAXFRQ,MAXBL),
     *   TIME(MAXTIM,MAXBL), FREQ(MAXFRQ), WTB(MAXBL), WTT(MAXTIM),
     *   WTF(MAXFRQ), PSTD(2)
      DOUBLE PRECISION XPRM(*), W, STEP(*), WORK(*), S
C
      INTEGER   ILIM, I, II, II1, II2, J, JJ, JJ1, JJ2, K, LIMF1, LIMF2,
     *   JX
      REAL      S1, S2, X1, X2, X3, WT, X1R, X1I, X2R, X2I, X3R, X3I,
     *   X12R, X12I, XTR, XTI, R(2), SUMWT
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      ILIM  =  3 * NUMANT
      LIMF1 = IFP1
      LIMF2 = IFP1 + NUMFRQ - 1
      SUMWT = 0.0
C                                       Take step, test parms in WORK
      DO 10 I = 1,ILIM
         WORK(I) = XPRM(I) - W * STEP(I)
 10      CONTINUE
      S1 = 0.0
C                                       Do chi square sum.
      DO 40 I = 1,NUMBL
         IF (WTB(I).EQ.0.) GO TO 40
         II = IS(I)
         JJ = JS(I)
         II1 = II + NUMANT * 2
         JJ1 = JJ + NUMANT * 2
         II2 = II + NUMANT
         JJ2 = JJ + NUMANT
         X1 = WORK(II) - WORK(JJ)
         X1R = COS (X1)
         X1I = SIN (X1)
         DO 30 J = LIMF1,LIMF2
            JX = J - LIMF1 + 1
            IF (WTF(J).EQ.0.) GO TO 30
            X2 = (WORK(II1) - WORK(JJ1)) * FREQ(JX)
            X2R = COS (X2)
            X2I = SIN (X2)
            X12R = X1R*X2R - X1I*X2I
            X12I = X1R*X2I + X1I*X2R
            DO 20 K = 1,NUMTIM
               WT = WTB(I) * WTF(J) * WTT(K)
               SUMWT = SUMWT + WT
C                                        Check for blanking.
               IF ((ABS (VREAL(K,J,I)) + ABS (VIMAG(K,J,I)))
     *           .LE.1.0E-20) WT = 0.0
               X3 = (WORK(II2) - WORK(JJ2)) * TIME(K,I)
               X3R = COS (X3)
               X3I = SIN (X3)
               XTR = X3R*X12R - X3I*X12I
               XTI = X3I*X12R + X3R*X12I
C                                       R = residual from model.
               R(1) = VREAL(K,J,I) - XTR
               R(2) = VIMAG(K,J,I) - XTI
               S1 = S1 + WT * (R(1)*R(1) + R(2)*R(2))
 20            CONTINUE
 30         CONTINUE
 40      CONTINUE
C                                       Add constraints
      S2 = 0.0
      DO 60 I=1,NUMANT
         IF (I.NE.REFANT) THEN
            IF (PSTD(1).GT.0.)
     *         S2 = S2 + 0.1 * SUMWT * ((WORK(NUMANT+I)) / PSTD(1))**2
            IF (PSTD(2).GT.0.)
     *         S2 = S2 + 0.1 * SUMWT * ((WORK(2*NUMANT+I)) / PSTD(2))**2
         END IF
 60      CONTINUE
      S = S1 + S2
      IF (PRTLV.GE.3) THEN
         WRITE (MSGTXT,1020) W, S, S2
         CALL MSGWRT (1)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (' W = ',F10.3,'         S = ',1PE15.5, E15.5)
      END
      SUBROUTINE FRNMSP (IS, JS, VREAL, VIMAG, TIMB, FREQS, FREQIF,
     *   CMBDEL, CREAL, CIMAG, CDELY, CRATE, CWT, REFAN, MAXFRQ, MAXTIM,
     *   MAXIFS, MAXBL, NUMANT, NUMBL, NUMFRQ, NUMTIM, NUMIF, WTB, WTT,
     *   WTF, DELWIN, RATWIN, IC, SNRMIN, GDSOLV, PRTLV)
C-----------------------------------------------------------------------
C   FRNMSP does least squares solutions for phase, singel and multiband
C   delay and rate.  The input values of CREAL,CIMAG,CMBDEL,CDELY,
C   and CRATE are the initial guess.  This routine explicitly assumes
C   that the difference between multi and single band delays is the same
C   for all IFs.
C   Inputs:
C      IS     I(*)    First ant. of baseline numbers
C      JS     I(*)    2nd ant. of baseline numbers
C      VREAL  R(MAXTIM,MAXFRQ,MAXBL)    Real part of visibility array
C      VIMAG  R(MAXTIM,MAXFRQ,MAXBL)    Imag part of visibility array
C      TIMB   R(MAXTIM,MAXBL)    Time wrt center
C      FREQS  D(*) Frequency array (GHz)
C      FREQIF D(*) Reference frequency per IF (Hz)
C      MAXTIM I    Maximum number of time integrations.
C      MAXFRQ I    Maximum number of frequency channels.
C      MAXIFS I    Maximum number of IFs
C      MAXBL  I    Maximum number of baselines
C      NUMANT I    Number of antennas
C      NUMBL  I    Number of baselines
C      NUMFRQ I    Number of frequencies
C      NUMTIM I    Number of times
C      NUMIF  I    Number of IFS
C      WTT    R(NUMTIM)    Time weight array
C      WTF    R(NUMFRQ)    Frequency weight array
C      WTB    R(NUMBL)    Baseline weight array
C      DELWIN R    delay window, <0 => no search in delay
C      RATWIN R    rate window, <0 => no search in rate
C      IC     I    Stokes' number passed, 0 => averaged.
C                  1 = R, 2 = L, 3 = I
C      SNRMIN R    Minimum SNR allowed
C      PRTLV  I    Print level, prints results at .ge. 2
C   Input/Output:
C      CMBDEL  R(2,NUMANT)          Multiband delays in seconds.
C      CREAL   R(2,NUMIF,NUMANT)    Real part of solution; includes
C                                   effects of multiband delay.
C      CIMAG   R(2,NUMIF,NUMANT)    Imag part of solution
C      CDELY   R(2,NUMIF,NUMANT)    single plus multi band delays in
C                                   seconds
C      CRATE   R(2,NUMIF,NUMANT)    Rates in Hz.
C      CWT     R(2,NUMIF,NUMANT)    Weights = SNR
C      REFAN   I(2,NUMIF)           Reference antennas used
C-----------------------------------------------------------------------
      INTEGER   MAXFRQ, MAXTIM, MAXIFS, MAXBL, IS(*), JS(*), NUMBL,
     *   NUMFRQ, NUMTIM, NUMANT, NUMIF, IC, PRTLV, REFAN(2,*)
      REAL      VREAL(MAXTIM,MAXFRQ,*), VIMAG(MAXTIM,MAXFRQ,*),
     *   CMBDEL(2,*), CREAL(2,MAXIFS,*), CIMAG(2,MAXIFS,*),
     *   CDELY(2,MAXIFS,*), CRATE(2,MAXIFS,*), CWT(2,MAXIFS,*),
     *   TIMB(MAXTIM,MAXBL), WTB(*), WTT(*), WTF(*), DELWIN,
     *   RATWIN, SNRMIN
      DOUBLE PRECISION FREQIF(*), FREQS(*)
      LOGICAL GDSOLV(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LDH, NA2, NA3, NA4, ITMAX, IANT, IST, IIF, REFANT, I,
     *   I1, I2, I3, I4, II, II1, II2, II3, III, J, JJ, JJ1, JJ2, JJ3,
     *   JJJ, K, KK, NFPIF, NANT, NANT2, NANT3, NANT4, NIT, IT,
     *   INDX, ISI, JSI, ID2, IER, ILIM, LIMF1, LIMF2, IFNO, INERT(3)
      LOGICAL   FLGRAT, FLGDEL, KFATI, KFATJ
      REAL TWOPI, PSTD(2), FREQX(MAXCIF), FREQY(MAXCIF), PHASE, RATE,
     *   MBDELY, SBDELY, SNR, PHAZ
      DOUBLE PRECISION SL, S, WT, X1, X2, X3, X4, RNOBS, SUMWT, CX,
     *   SX, G1, G2, G3, G4, D11, D12, D13, D14, D22, D23, D24, D33,
     *   D34, D44, GN, W, TOL, SIGMA2, RMS, RCOND, DET(2),
     *   XTR, XTI, XSDEL, XMDEL, XRATE
      INCLUDE 'INCS:GAIN.INC'
      INTEGER   KANT(MAXANT), IKANT(MAXANT), KPVT(MAXPRM)
      REAL      SWT(MAXANT)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA TOL/1.0D-4/
C-----------------------------------------------------------------------
      IF (PRTLV.GE.2) THEN
         MSGTXT = 'Refining solutions allowing SB delay'
         CALL MSGWRT (3)
         END IF
      LDH = MAXPRM
C                                       Set solution flags
      FLGRAT = RATWIN .LT. 0.0
      FLGDEL = DELWIN .LT. 0.0
      ITMAX = 50
      SL = 1.0D30
      TWOPI = 8.0 * ATAN (1.0)
      NANT2 = 2 * NUMANT
      NANT3 = 3 * NUMANT
      NANT4 = 4 * NUMANT
      IST = MAX (IC, 1)
C                                       IQUV?
      IF (IC.EQ.3) IST = 1
C                                       Set prior dist. widths.
CC      PSTD(1) = 0.5 * RATWIN * 5.4286721E2
CC      PSTD(2) = 0.5 * DELWIN * 6.283185308
C                                       Non-zero penalty terms caused
C                                       the solution to be biased
C                                       towards zero when PSTD dominated
C                                       over the Chi-squared
      PSTD(1) = 0.0
      PSTD(2) = 0.0
C                                       Do joint solution for all IFs
      REFANT = REFAN(IST,1)
C                                       Range of channels (all)
      LIMF1 = 1
      LIMF2 = NUMFRQ
C                                       Number of channels per IF
      NFPIF = NUMFRQ / NUMIF
C                                       Determine which antennas have
C                                       data:
      DO 10 I = 1,NUMANT
         SWT(I) = 0.0
 10      CONTINUE
      RNOBS = 0.0
      SUMWT = 0.0
      DO 40 I = 1,NUMBL
         IF (WTB(I).EQ.0.) GO TO 40
         ISI = IS(I)
         JSI = JS(I)
         DO 30 J = LIMF1,LIMF2
            IF (WTF(J).LE.0.0) GO TO 30
            DO 20 K = 1,NUMTIM
               WT = WTB(I) * WTF(J) * WTT(K)
               IF ((ABS (VREAL(K,J,I)) + ABS (VIMAG(K,J,I))) .LE.
     *            1.0E-20) WT = 0.0
               SWT(ISI) = SWT(ISI) + WT
               SWT(JSI) = SWT(JSI) + WT
               IF (WT.GT.1.0E-20) RNOBS = RNOBS + 1.0D0
               SUMWT = SUMWT + WT
 20            CONTINUE
 30         CONTINUE
 40      CONTINUE
C                                       Fill internal frequency arrays
      DO 45 J = LIMF1,LIMF2
         FREQX(J) = FREQS(J) - FREQS(1)
         IFNO = ((J-1) / NFPIF) + 1
         FREQY(J) = FREQS(J) - (FREQIF(IFNO)*1.0D-9)
 45      CONTINUE
      NANT = 0
C                                       Get lists of good antennas other
C                                       than the reference antenna.
      CALL FILL (MAXANT, -1, KANT)
      CALL FILL (MAXANT, -1, IKANT)
      DO 50 IANT = 1,NUMANT
         IF ((SWT(IANT).GT.1.0E-20) .AND.
     *      (CWT(IST,1,IANT).GT.0.0001)) THEN
            IF ((GDSOLV(1).AND.GDSOLV(1+IANT)).OR.(IANT.EQ.REFANT)) THEN
C                                       then IANT is neutral
               IKANT(IANT) = 0
            ELSE
C                                       otherwise IANT is good
               NANT = NANT + 1
               KANT(NANT) = IANT
               IKANT(IANT) = NANT
               END IF
            END IF
 50      CONTINUE
      NA2 = 2 * NANT
      NA3 = 3 * NANT
      NA4 = 4 * NANT
C                                       Be sure the problem is
C                                       constrained and arrays are large
C                                       enough.
      IF (((RNOBS.LE.(NA4+2)) .OR. (NANT.EQ.0)) .OR. (NA4.GT.MAXPRM))
     *   THEN
C                                       Message if arrays aren't large
C                                       enough
         IF (NA4.GT.MAXPRM) THEN
            WRITE (MSGTXT,1050) NA4
            CALL MSGWRT (7)
            MSGTXT = 'SOLUTION WAS SKIPPED - FLAGGED BAD'
            CALL MSGWRT (7)
            END IF
C                                       Flag solution and skip
         DO 55 IANT = 1,NUMANT
            CREAL(IST,1,IANT) = FBLANK
            CIMAG(IST,1,IANT) = FBLANK
            CDELY(IST,1,IANT) = FBLANK
            CRATE(IST,1,IANT) = FBLANK
            CWT(IST,1,IANT) = 0.0
 55         CONTINUE
         GO TO 800
         END IF
C                                       Transfer initial guesses to
C                                       internal array:
      DO 60 IANT = 1,NUMANT
         STEP(IANT) = 0.0
         STEP(IANT+NUMANT) = 0.0
         STEP(IANT+NANT2) = 0.0
         STEP(IANT+NANT3) = 0.0
         XPRM(IANT) = 0.0
         XPRM(IANT+NUMANT) = 0.0
         XPRM(IANT+NANT2) = 0.0
         XPRM(IANT+NANT3) = 0.0
C         IF (SWT(IANT).GT.0.0) THEN
C                                       Need both tests since CREAL,CIMAG
C                                       use CDELY without checking it.
C                                       all FBLANK tests in this loop are
C                                       now redundant
         IF ((SWT(IANT).GT.0.0).AND.(CWT(IST,1,IANT).GT.0.0)) THEN
            IF ((CIMAG(IST,1,IANT).NE.FBLANK) .AND.
     *          (CREAL(IST,1,IANT).NE.FBLANK))
     *         XPRM(IANT) = ATAN2 (CIMAG(IST,1,IANT),
     *         CREAL(IST,1,IANT)) +
C                                       Delay correction from ref. chan.
     *         (FREQS(1)*1.0D9 - FREQIF(1)) * CDELY(IST,1,IANT) * TWOPI
            IF (CRATE(IST,1,IANT).NE.FBLANK)
     *         XPRM(NUMANT+IANT) = CRATE(IST,1,IANT) * TWOPI * 86400.0
            IF (CMBDEL(IST,IANT).NE.FBLANK)
     *         XPRM(NANT2+IANT) = CMBDEL(IST,IANT) * TWOPI * 1.0E9
C                                       Initial value of singleband
C                                       delay is zero.
            END IF
C                                       In case it doesn't converge.
         CREAL(IST,1,IANT) = FBLANK
         CIMAG(IST,1,IANT) = FBLANK
         CDELY(IST,1,IANT) = FBLANK
         CRATE(IST,1,IANT) = FBLANK
         CWT(IST,1,IANT) = 0.0
 60      CONTINUE
      XPRM(REFANT) = 0.0
      XPRM(NUMANT+REFANT) = 0.0
      XPRM(NANT2+REFANT) = 0.0
      XPRM(NANT3+REFANT) = 0.0
C                                       Iterate:
      DO 500 IT = 1,ITMAX
         NIT = IT
C                                       Zero the gradient and Hessian
C                                       arrays:
         DO 80 I = 1,NA4
            GRAD(I) = 0.0D0
            DO 70 J = 1,NA4
               HESS(I,J) = 0.0D0
 70            CONTINUE
 80         CONTINUE
C                                       Initial value of function
C                                       minimized.
         W = 0.0D0
         IF (IT.EQ.1) CALL MEVAL (XPRM, W, STEP, IS, JS,
     *      VREAL, VIMAG, TIMB, FREQX, FREQY, MAXTIM, MAXFRQ, MAXBL,
     *      NUMANT, NUMBL, NUMFRQ, NUMTIM, WTB, WTT, WTF, PSTD, REFANT,
     *      PRTLV, SL, GWORK)
C                                       Accumulate (half) the gradient
C                                       of S and (half) the Hessian of
C                                       S:
         DO 200 I = 1, NUMBL
C                                       Data valid?
            IF (WTB(I).EQ.0.) GO TO 200
            III = IS(I)
            JJJ = JS(I)
            II = IKANT(III)
            JJ = IKANT(JJJ)
C                                       If both ends are bad or
C                                       this is not a cross-correlation
C                                       then skip this baseline
            IF (((II.LT.0).OR.(JJ.LT.0))   .OR.
     *          (II.EQ.JJ)                      ) GO TO 200
            II1 = NANT + II
            II2 = NANT + II1
            II3 = NANT + II2
            JJ1 = NANT + JJ
            JJ2 = NANT + JJ1
            JJ3 = NANT + JJ2
C                                       Phase at ref time, freq.
            X1    = XPRM(III       ) - XPRM(JJJ       )
            XSDEL = XPRM(III+ NANT2) - XPRM(JJJ+ NANT2)
            XMDEL = XPRM(III+ NANT3) - XPRM(JJJ+ NANT3)
            XRATE = XPRM(III+NUMANT) - XPRM(JJJ+NUMANT)
            KFATI = II.GT.0
            KFATJ = JJ.GT.0
            DO 190 J = LIMF1,LIMF2
               IF (WTF(J).EQ.0.) GO TO 190
C                                       Multiband delay term
               X2 = XSDEL * FREQX(J)
C                                       Singleband delay term (plus
C                                       multiband)
               X3 = XMDEL * FREQY(J)
               DO 140 K = 1,NUMTIM
                  WT = WTB(I) * WTF(J) * WTT(K)
C                                        Check for blanking.
                  IF ((ABS (VREAL(K,J,I)) + ABS (VIMAG(K,J,I)))
     *               .LE.1.0E-20) WT = 0.0
                  X4 = XRATE * TIMB(K,I)
                  XTR = COS (X1 + X2 + X3 + X4)
                  XTI = SIN (X1 + X2 + X3 + X4)
                  CX = WT * (VREAL(K,J,I) * XTR + VIMAG(K,J,I) * XTI)
                  SX = WT * (VIMAG(K,J,I) * XTR - VREAL(K,J,I) * XTI)
                  G1 = -SX
                  G2 = -TIMB(K,I) * SX
                  G3 = -FREQX(J) * SX
                  G4 = -FREQY(J) * SX
                  D11 = CX
                  D12 = TIMB(K,I) * CX
                  D13 = FREQX(J) * CX
                  D14 = FREQY(J) * CX
                  D22 = TIMB(K,I) * D12
                  D23 = FREQX(J) * D12
                  D24 = FREQY(J) * D12
                  D33 = FREQX(J) * D13
                  D34 = FREQY(J) * D13
                  D44 = FREQY(J) * D14
C                                       This code is still being tested
C                                       it should be commented out of the
C                                       check'ed in version
C                  IF (FLGDEL) THEN
C                     G3  = 0.0
C                     G4  = 0.0
C                     D13 = 0.0
C                     D14 = 0.0
C                     D23 = 0.0
C                     D24 = 0.0
C                     END IF
C                  IF (FLGRAT) THEN
C                     G2  = 0.0
C                     D12 = 0.0
C                     END IF
                  IF (KFATI) THEN
                     GRAD(II) = GRAD(II) + G1
                     HESS(II,II) = HESS(II,II) + D11
                     HESS(II,II1) = HESS(II,II1) + D12
                     HESS(II,II2) = HESS(II,II2) + D13
                     HESS(II,II3) = HESS(II,II3) + D14
                     GRAD(II1) = GRAD(II1) + G2
                     GRAD(II2) = GRAD(II2) + G3
                     GRAD(II3) = GRAD(II3) + G4
                     HESS(II1,II1) = HESS(II1,II1) + D22
                     HESS(II1,II2) = HESS(II1,II2) + D23
                     HESS(II1,II3) = HESS(II1,II3) + D24
                     HESS(II2,II2) = HESS(II2,II2) + D33
                     HESS(II2,II3) = HESS(II2,II3) + D34
                     HESS(II3,II3) = HESS(II3,II3) + D44
                     END IF
                  IF (KFATJ) THEN
                     GRAD(JJ) = GRAD(JJ) - G1
                     HESS(JJ,JJ) = HESS(JJ,JJ) + D11
                     HESS(JJ,JJ1) = HESS(JJ,JJ1) + D12
                     HESS(JJ,JJ2) = HESS(JJ,JJ2) + D13
                     HESS(JJ,JJ3) = HESS(JJ,JJ3) + D14
                     GRAD(JJ1) = GRAD(JJ1) - G2
                     GRAD(JJ2) = GRAD(JJ2) - G3
                     GRAD(JJ3) = GRAD(JJ3) - G4
                     HESS(JJ1,JJ1) = HESS(JJ1,JJ1) + D22
                     HESS(JJ1,JJ2) = HESS(JJ1,JJ2) + D23
                     HESS(JJ1,JJ3) = HESS(JJ1,JJ3) + D24
                     HESS(JJ2,JJ2) = HESS(JJ2,JJ2) + D33
                     HESS(JJ2,JJ3) = HESS(JJ2,JJ3) + D34
                     HESS(JJ3,JJ3) = HESS(JJ3,JJ3) + D44
                     END IF
                  IF (KFATI.AND.KFATJ) THEN
                     HESS(II,JJ) = HESS(II,JJ) - D11
                     HESS(II,JJ1) = HESS(II,JJ1) - D12
                     HESS(II,JJ2) = HESS(II,JJ2) - D13
                     HESS(II,JJ3) = HESS(II,JJ3) - D14
                     HESS(II1,JJ1) = HESS(II1,JJ1) - D22
                     HESS(II1,JJ2) = HESS(II1,JJ2) - D23
                     HESS(II1,JJ3) = HESS(II1,JJ3) - D24
                     HESS(II2,JJ2) = HESS(II2,JJ2) - D33
                     HESS(II2,JJ3) = HESS(II2,JJ3) - D34
                     HESS(II3,JJ3) = HESS(II3,JJ3) - D44
                     HESS(JJ,II1) = HESS(JJ,II1) - D12
                     HESS(JJ,II2) = HESS(JJ,II2) - D13
                     HESS(JJ,II3) = HESS(JJ,II3) - D14
                     HESS(JJ1,II2) = HESS(JJ1,II2) - D23
                     HESS(JJ1,II3) = HESS(JJ1,II3) - D24
                     HESS(JJ2,II3) = HESS(JJ2,II3) - D34
                     END IF
 140              CONTINUE
 190           CONTINUE
 200        CONTINUE
C                                       filling in the hessian, saving it
C                                       in SHESS, and filling in the diagonal
C                                       really ought to be done at the same
C                                       time.
C                                       Fill in the lower triangular
C                                       part of the Hessian:
         DO 210 I = 2,NA4
            ILIM = I - 1
            DO 210 J = 1,ILIM
               HESS(I,J) = HESS(J,I)
 210           CONTINUE
C                                       Add constraint penalty terms
C                                       Constrain about 0 delay, rate
      IF (FLGRAT .OR. (PSTD(1).LE.0.0)) GO TO 230
C                                       Rate
         ILIM = NANT + 1
         DO 220 I = ILIM,NA2
            INDX = KANT(I-NANT)
            GRAD(I) = GRAD(I) + 0.1 * SUMWT * (XPRM(INDX+NUMANT)) /
     *         PSTD(1)**2
            HESS(I,I) = HESS(I,I) + 0.1 * SUMWT / PSTD(1)**2
 220        CONTINUE
 230  IF (FLGDEL .OR. (PSTD(2).LE.0.0)) GO TO 250
C                                       Multiband delay
         ILIM = NA2 + 1
         DO 240 I = ILIM,NA3
            INDX = KANT(I-NA2)
            GRAD(I) = GRAD(I) + 0.1 * SUMWT * (XPRM(INDX+NANT2)) /
     *         PSTD(2)**2
            HESS(I,I) = HESS(I,I) + 0.1 * SUMWT / PSTD(2)**2
 240        CONTINUE
C                                       Singleband delay
         ILIM = NA3 + 1
         DO 245 I = ILIM,NA4
            INDX = KANT(I-NA3)
            GRAD(I) = GRAD(I) + 0.1 * SUMWT * (XPRM(INDX+NANT2)) /
     *         PSTD(2)**2
            HESS(I,I) = HESS(I,I) + 0.1 * SUMWT / PSTD(2)**2
 245        CONTINUE
C                                       Zeroed terms taken care of in the
C                                       main loop.  all that remains is
C                                       to put one's on the diagonal.
C                                       Zero terms not being solved for
 250  IF (FLGRAT) THEN
C                                       FLGRAT = rate
         ILIM = NANT + 1
         DO 290 I = ILIM,NA2
            GRAD(I) = 0.0
            HESS(I,I) = 1.0
            DO 280 J = 1,NA3
               HESS(I,J) = 0.0
               HESS(J,I) = 0.0
 280           CONTINUE
 290        CONTINUE
         END IF
      IF (FLGDEL) THEN
C                                       FLGDEL = delay (multi)
         ILIM  =  NA2 + 1
         DO 300 I = ILIM,NA3
            GRAD(I) = 0.0
            HESS(I,I) = 1.0
            DO 300 J = 1,NA3
               HESS(I,J) = 0.0
               HESS(J,I) = 0.0
 300           CONTINUE
C                                       Single band
         ILIM  =  NA3 + 1
         DO 310 I = ILIM,NA4
            GRAD(I) = 0.0
            HESS(I,I) = 1.0
            DO 310 J = 1,NA4
               HESS(I,J) = 0.0
               HESS(J,I) = 0.0
 310        CONTINUE
         END IF
C                                       Save the Hessian, in case that
C                                       it's indefinite:
      DO 320 I = 1,NA4
         DO 320 J = 1,NA4
            SHESS(I,J) = HESS(I,J)
 320        CONTINUE
C                                       Calculate and print the
C                                       Euclidean norm of the gradient
C                                       in order to monitor the
C                                       progress toward a critical
C                                       point:
         IF (PRTLV.GE.3) THEN
            CALL DNRM2 (NA4, GRAD, 1, GN)
            WRITE (MSGTXT,1000) IT, GN
            CALL MSGWRT (1)
            END IF
C                                       Factor the Hessian and obtain
C                                       an estimate of its cond. no.:
         CALL DSICO (HESS, LDH, NA4, KPVT, RCOND, GWORK)
         IF (PRTLV.GE.3) THEN
            WRITE (MSGTXT,1001) RCOND
            CALL MSGWRT (1)
            END IF
C                                       Compute determinant and
C                                       inertia:
         CALL DSIDI (HESS, LDH, NA4, KPVT, DET, INERT, GWORK, 110)
         ID2 = DET(2)
         IF (PRTLV.GE.3) THEN
            WRITE (MSGTXT,1002) DET(1), ID2, INERT
            CALL MSGWRT (1)
            END IF
C                                       If the Hessian is indefinite or
C                                       singular use Greenstadt's
C                                       modification:
         IF ((INERT(2).NE.0) .OR. (RCOND.EQ.0.D0)) THEN
            CALL GM (NA4, SHESS, HESS, GWORK, LDH, GRAD, IER)
         ELSE
C                                       Solve for the Newton correction
C                                       (to be placed in GRAD):
            CALL DSISL (HESS, LDH, NA4, KPVT, GRAD)
            END IF
C                                       Take a damped Newton step:
         DO 360 I = 1,NANT
            J = KANT(I)
            STEP(J) = GRAD(I)
            STEP(NUMANT+J) = GRAD(NANT+I)
            STEP(NANT2+J) = GRAD(NA2+I)
            STEP(NANT3+J) = GRAD(NA3+I)
 360        CONTINUE
         W = 0.8
         IF (INERT(2).EQ.0) W = 1.0
         IF (IT.LE.2) W = 0.25
C                                       Find step size that improves
C                                       soln.
         DO 370 KK = 1,6
            CALL MEVAL (XPRM, W, STEP, IS, JS, VREAL, VIMAG, TIMB,
     *         FREQX, FREQY, MAXTIM, MAXFRQ, MAXBL, NUMANT, NUMBL,
     *         NUMFRQ, NUMTIM, WTB, WTT, WTF, PSTD, REFANT, PRTLV, S,
     *         GWORK)
            IF (S.LT.SL) GO TO 380
            W = 0.25 * W
 370        CONTINUE
C                                       Can't improve it, quit.
         GO TO 510
C                                       Convergence test
 380     IF (ABS (1.0D0 - S/SL).LE.5.0D-6) GO TO 510
C                                       Prepare for next iteration
         SL = S
         DO 390 I = 1,NANT4
            XPRM(I) = XPRM(I) - W * STEP(I)
 390        CONTINUE
C                                       See if solution changing
         DO 400 I = 1,NANT4
            IF (ABS (STEP(I)).GT.(ABS( XPRM(I))+1.0D-3)*TOL) GO TO 500
 400        CONTINUE
C                                       Not changing - quit.
         GO TO 510
C                                       End of iteration loop
 500     CONTINUE
C                                       Calculate std. errors:
 510  IF (INERT(1).LT.NA4) GO TO 800
      SIGMA2 = S / (RNOBS - NA4) * RNOBS / SUMWT
      RMS = SQRT (SIGMA2)
      IF (PRTLV.GE.3)  THEN
         WRITE (MSGTXT,1700) RMS
         CALL MSGWRT (3)
         END IF
      CALL DSIDI (HESS, LDH, NA4, KPVT, DET, INERT, GWORK, 1)
      SIGMA2 = MAX (SIGMA2, 1.0D-25)
C                                       Refer the phases to the
C                                       reference channel.
      DO 515 IANT = 1,NUMANT
         XPRM(IANT) = XPRM(IANT) -
     *      (XPRM(NANT2+IANT) - XPRM(NANT2+REFANT)) *
     *      (FREQS(1) - FREQIF(1)*1.0D-9)
 515     CONTINUE
C                                       Before returning, save results
C                                       Multiband delays are
C                                       incorporated into the IF phase.
      DO 520 IANT = 1,NUMANT
         IF (SWT(IANT).LE.0.0) GO TO 520
C                                       Phase
         PHASE = (XPRM(IANT) - XPRM(REFANT))
         CREAL(IST,1,IANT) = COS (PHASE)
         CIMAG(IST,1,IANT) = SIN (PHASE)
C                                       Multiband delay
         CMBDEL(IST,IANT) = ((XPRM(NANT2+IANT) - XPRM(NANT2+REFANT)) /
     *      TWOPI) * 1.0E-9
C                                       Multi plus single band delay
         CDELY(IST,1,IANT) = (((XPRM(NANT3+IANT) -
     *      XPRM(NANT3+REFANT)) + (XPRM(NANT2+IANT) -
     *      XPRM(NANT2+REFANT))) / TWOPI) * 1.0E-9
C                                       Rate
         CRATE(IST,1,IANT) = (XPRM(NUMANT+IANT) -
     *      XPRM(NUMANT+REFANT)) / (TWOPI * 86400.0)
C        IF (IANT.EQ.REFANT) CWT(IST,1,IANT) = SNRMIN + 1.0
         JJJ = IKANT(IANT)
         IF (JJJ.EQ.0) CWT(IST,1,IANT) = SNRMIN + 1.0
         IF ((JJJ.GT.0) .AND. (HESS(JJJ,JJJ).GT.0.0)) THEN
            CWT(IST,1,IANT) = SQRT (SIGMA2 * HESS(JJJ,JJJ) * 0.5)
            IF (CWT(IST,1,IANT).GT.1.0E-20)
     *         CWT(IST,1,IANT) = 1.0 / CWT(IST,1,IANT)
            END IF
C                                       Check min SNR
         IF (CWT(IST,1,IANT).LT.SNRMIN) THEN
            CREAL(IST,1,IANT) = FBLANK
            CIMAG(IST,1,IANT) = FBLANK
            CDELY(IST,1,IANT) = FBLANK
            CRATE(IST,1,IANT) = FBLANK
            CWT(IST,1,IANT) = 0.0
            CMBDEL(IST,IANT) = FBLANK
            END IF
 520     CONTINUE
      IF (PRTLV.GE.2) THEN
C                                        Print sigmas in deg,mHz,nsec.
         MSGTXT = 'Fitted phases, rates, delays and SNR'
         CALL MSGWRT (3)
         MSGTXT = '[ P = phase(deg), R = rate(mHz), M = ' //
     *      'Multi-Band Delay(nsec),'
         CALL MSGWRT (3)
         MSGTXT = '  D = (Single-Band - Multi-Band) Delay(nsec), ' //
     *      'S = SNR ]'
         CALL MSGWRT (3)
         DO 530 I = 1,NANT
            II = KANT(I)
            I1 = I
            I2 = NANT + I
            I3 = NA2 + I
            I4 = NA3 + I
            GWORK(I1) = SQRT (SIGMA2 * HESS(I1,I1) * 0.5) * 57.296
            GWORK(I2) = SQRT (SIGMA2 * HESS(I2,I2) * 0.5) * 1.8420711E-3
            GWORK(I3) = SQRT (SIGMA2 * HESS(I3,I3) * 0.5) * 1.59154E-1
            GWORK(I4) = SQRT (SIGMA2 * HESS(I4,I4) * 0.5) * 1.59154E-1
            IF (FLGRAT) GWORK(I2) = 0.0
            IF (FLGDEL) GWORK(I3) = 0.0
            IF (FLGDEL) GWORK(I4) = 0.0
C                                       Print rates and delays, SNR
            PHAZ = (XPRM(II) - XPRM(REFANT)) * 57.296
            RATE =  (XPRM(NUMANT+II) - XPRM(NUMANT+REFANT)) *
     *         1.8420711E-3
            MBDELY = (XPRM(NANT2+II) - XPRM(NANT2+REFANT)) * 1.59154E-1
            SBDELY =  (XPRM(NANT3+II) - XPRM(NANT3+REFANT)) * 1.59154E-1
            SNR = SQRT (SIGMA2 * HESS(I1,I1) * 0.5)
            IF (SNR.GT.1.0E-20) SNR = 1.0 / SNR
            SNR = MIN (9999.0, SNR)
            WRITE (MSGTXT,1530) II, PHAZ, RATE, MBDELY, SBDELY, SNR
            CALL MSGWRT (3)
 530        CONTINUE
         MSGTXT =  'Standard RMS errors (deg, mHz, nsec, nsec):'
         CALL MSGWRT (3)
         DO 540 K = 1,NANT
            WRITE (MSGTXT,1602) KANT(K), GWORK(K), GWORK(K+NANT),
     *         GWORK(K+NA2), GWORK(K+NA3)
            CALL MSGWRT (3)
 540        CONTINUE
         END IF
C                                       If combined Stokes, copy
         IF (IC.GT.0) GO TO 800
            DO 560 IANT = 1,NUMANT
               CREAL(2,1,IANT) = CREAL(1,1,IANT)
               CIMAG(2,1,IANT) = CIMAG(1,1,IANT)
               CDELY(2,1,IANT) = CDELY(1,1,IANT)
               CRATE(2,1,IANT) = CRATE(1,1,IANT)
               CWT(2,1,IANT) = CWT(1,1,IANT)
 560           CONTINUE
            REFAN(2,1) = REFAN(1,1)
C                                       Copy solns. to all IFs
 800  DO 820 IIF = 2,NUMIF
         DO 810 IANT = 1,NUMANT
C                                       Phase
            PHASE = (XPRM(IANT) - XPRM(REFANT))
C                                       Multiband correction
     *         + ((XPRM(IANT+NANT2) - XPRM(REFANT+NANT2)) *
     *         (FREQIF(IIF) - FREQIF(1))*1.0D-9)
            CREAL(IST,IIF,IANT) = COS (PHASE)
            CIMAG(IST,IIF,IANT) = SIN (PHASE)
C                                       temporal 4
            IF (CREAL(IST,1,IANT) .EQ. FBLANK)
     *          CREAL(IST,IIF,IANT) =  FBLANK
            IF (CIMAG(IST,1,IANT) .EQ. FBLANK)
     *          CIMAG(IST,IIF,IANT) = FBLANK
            CDELY(IST,IIF,IANT) = CDELY(IST,1,IANT)
            CRATE(IST,IIF,IANT) = CRATE(IST,1,IANT)
            CWT(IST,IIF,IANT) = CWT(IST,1,IANT)
 810        CONTINUE
         REFAN(IST,IIF) = REFAN(IST,1)
 820     CONTINUE
C                                       If combined Stokes, copy
      IF (IC.LE.0 ) THEN
         DO 840 IIF = 1,NUMIF
            DO 830 IANT = 1,NUMANT
               CREAL(2,IIF,IANT) = CREAL(1,IIF,IANT)
               CIMAG(2,IIF,IANT) = CIMAG(1,IIF,IANT)
               CDELY(2,IIF,IANT) = CDELY(1,IIF,IANT)
               CRATE(2,IIF,IANT) = CRATE(1,IIF,IANT)
               CWT(2,IIF,IANT) = CWT(1,IIF,IANT)
 830           CONTINUE
            REFAN(2,IIF) = REFAN(1,IIF)
 840        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Iteration #',I4,'   Gradient norm=',1PE15.5)
 1001 FORMAT ('Reciprocal cond. no.=',1PE15.5)
 1002 FORMAT ('Det=',F15.5,'E',I4,'    inertia=',3I3)
 1050 FORMAT ('ERROR: TOO MANY PARAMETERS (',I4,') FOR INTERNAL ARRAYS')
 1530 FORMAT ('A= ',I2.2,' P=',F7.1,' R=',F10.1,' M=',F10.1,' D=',
     *   F10.1,' S=',F6.1)
 1602 FORMAT ('A= ',I2.2,' P=',F7.2,' R=',F10.2,' M=',F10.2,' D=',
     *   F10.2)
 1700 FORMAT ('RMS residual=',F10.5)
      END
      SUBROUTINE MEVAL (XPRM, W, STEP, IS, JS, VREAL, VIMAG, TIME,
     *   FREQX, FREQY, MAXTIM, MAXFRQ, MAXBL, NUMANT, NUMBL, NUMFRQ,
     *   NUMTIM, WTB, WTT, WTF, PSTD, REFANT, PRTLV, S, WORK)
C-----------------------------------------------------------------------
C   This routine takes a specified step in the model parameters used
C   by FRNMSP and compute the function being minimized, i.e. the chi-
C   squared sum plus penalty terms to constrain the delay and rate
C   solution.
C   Inputs:
C      XPRM   D(NUMANT,4) Delay, rate and phase current model parms.
C                         In order, phase, rate, multiband delay, single
C                         band delay.
C      W      D           Step size to take, fraction of STEP
C      STEP   D(NUMANT,4) Step for each of XPRM.
C      IS     I(*)        First ant. of baseline numbers
C      JS     I(*)        2nd ant. of baseline numbers
C      VREAL  R(MAXTIM,MAXFRQ,MAXBL) Real part of visibility array
C      VIMAG  R(MAXTIM,MAXFRQ,MAXBL) Imag part of visibility array
C      TIME   R(MAXTIM,MAXBL)        Time wrt center
C      FREQX  R(MAXFRQ)   Total Frequency array
C      FREQY  R(MAXFRQ)   IF Frequency array
C      MAXTIM I           Maximum number of time integrations.
C      MAXFRQ I           Maximum number of frequency channels.
C      MAXBL  I           Maximum number of baselines.
C      NUMANT I           Number of antennas
C      NUMBL  I           Number of baselines
C      NUMFRQ I           Number of frequencies
C      NUMTIM I           Number of times
C      WTB    R(MAXBL)    Baseline weight array
C      WTT    R(MAXTIM)   Time weight array
C      WTF    R(MAXFRQ)   Frequency weight array
C      PSTD   R(2)        Model constraints, (rate, delay)
C                         .LE.0.0 => no constraint.
C      REFANT I           Reference antenna
C      PRTLV  I           Print level, gives some results if .ge. 2
C   Output:
C      S      D           Value of function being minimized, chi-squares
C                         sum plus penalty terms.
C      WORK   D(*)        A work array equal in size to XPRM.
C-----------------------------------------------------------------------
      INTEGER   MAXTIM, MAXFRQ, MAXBL, IS(*), JS(*), NUMANT,
     *   NUMBL, NUMFRQ, NUMTIM, REFANT, PRTLV
      INCLUDE 'INCS:PUVD.INC'
      REAL      VREAL(MAXTIM,MAXFRQ,MAXBL), VIMAG(MAXTIM,MAXFRQ,MAXBL),
     *   TIME(MAXTIM,MAXBL), FREQX(MAXCIF), FREQY(MAXCIF), WTB(MAXBL),
     *   WTT(MAXTIM), WTF(MAXFRQ), PSTD(2)
      DOUBLE PRECISION XPRM(*), W, STEP(*), WORK(*), S
C
      INTEGER   ILIM, I, II, II1, II2, II3, J, JJ, JJ1, JJ2, JJ3, K,
     *   LIMF1, LIMF2
      DOUBLE PRECISION S1, S2, X1, X2, X3, X4, WT, X1R, X1I, X3R, X3I,
     *   X4R, X4I, X123R, X123I, XTR, XTI, R(2), SUMWT
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      ILIM  =  4 * NUMANT
      LIMF1 = 1
      LIMF2 = NUMFRQ
      SUMWT = 0.0
C                                       Take step, test parms in WORK
      DO 10 I = 1,ILIM
         WORK(I) = XPRM(I) - W * STEP(I)
 10      CONTINUE
      S1 = 0.0
C                                       Do chi square sum.
      DO 40 I = 1,NUMBL
         IF (WTB(I).EQ.0.) GO TO 40
         II = IS(I)
         JJ = JS(I)
         II1 = II + NUMANT * 2
         JJ1 = JJ + NUMANT * 2
         II2 = II + NUMANT
         JJ2 = JJ + NUMANT
         II3 = II1 + NUMANT
         JJ3 = JJ1 + NUMANT
C                                       Phase at ref time, freq.
         X1 = WORK(II) - WORK(JJ)
         X1R = COS (X1)
         X1I = SIN (X1)
         DO 30 J = LIMF1,LIMF2
            IF (WTF(J).EQ.0.) GO TO 30
C                                       Multiband delay term
            X2 = (WORK(II1) - WORK(JJ1)) * FREQX(J)
C                                       Singleband delay term (plus
C                                       multiband)
            X3 = (WORK(II3) - WORK(JJ3)) * FREQY(J) + X2
            X3R = COS (X3)
            X3I = SIN (X3)
C                                       Phase0 + multiband + single
            X123R = X1R*X3R - X1I*X3I
            X123I = X1R*X3I + X1I*X3R
            DO 20 K = 1,NUMTIM
               WT = WTB(I) * WTF(J) * WTT(K)
               SUMWT = SUMWT + WT
C                                        Check for blanking.
               IF ((ABS (VREAL(K,J,I)) + ABS (VIMAG(K,J,I)))
     *           .LE.1.0E-20) WT = 0.0
C                                       Rate term
               X4 = (WORK(II2) - WORK(JJ2)) * TIME(K,I)
               X4R = COS (X4)
               X4I = SIN (X4)
C                                       Total phase model
               XTR = X4R*X123R - X4I*X123I
               XTI = X4I*X123R + X4R*X123I
C                                       R = residual from model.
               R(1) = VREAL(K,J,I) - XTR
               R(2) = VIMAG(K,J,I) - XTI
               S1 = S1 + WT * (R(1)*R(1) + R(2)*R(2))
 20            CONTINUE
 30         CONTINUE
 40      CONTINUE
C                                       Add constraints
      S2 = 0.0
      DO 60 I=1,NUMANT
         IF (I.NE.REFANT) THEN
            IF (PSTD(1).GT.0.)
     *         S2 = S2 + 0.1 * SUMWT * ((WORK(NUMANT+I)) / PSTD(1))**2
            IF (PSTD(2).GT.0.) THEN
C                                       Multiband delay
               S2 = S2 + 0.1 * SUMWT * ((WORK(2*NUMANT+I)) / PSTD(2))**2
C                                       Single band delay
               S2 = S2 + 0.1 * SUMWT * ((WORK(3*NUMANT+I)) / PSTD(2))**2
            END IF
         END IF
 60      CONTINUE
      S = S1 + S2
      IF (PRTLV.GE.3) THEN
         WRITE (MSGTXT,1020) W, S, S2
         CALL MSGWRT (1)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (' W = ',F10.3,'         S = ',1PE15.5, E15.5)
      END
      SUBROUTINE FRDISP (IS, JS, VREAL, VIMAG, REFREQ, FREQS, FREQIF,
     *   CMBDEL, CDISP, CREAL, CIMAG, CDELY, CWT, REFAN, MAXFRQ, MAXTIM,
     *   MAXIFS, MAXBL, NUMANT, NUMBL, NUMFRQ, NUMTIM, NUMIF, WTB, WTT,
     *   WTF, IC, GDSOLV, PRTLV, TIMEC, DODISP)
C-----------------------------------------------------------------------
C   FRDISP uses least squares to fit the SB delays with a MB delay and
C   dispersion.  The output values of CREAL, CIMAG, and CDELY have been
C   corrected for the dispersion.
C   Inputs:
C      IS      I(*)   First ant. of baseline numbers
C      JS      I(*)   2nd ant. of baseline numbers
C      VREAL   R(MAXTIM,MAXFRQ,MAXBL)    Real part of visibility array
C      VIMAG   R(MAXTIM,MAXFRQ,MAXBL)    Imag part of visibility array
C      REFREQ  D      Reference frequency (Hz)
C      FREQS   D(*)   Frequency array (GHz) wrt reference freq
C      FREQIF  D(*)   Reference frequency per IF (Hz)
C      MAXTIM  I      Maximum number of time integrations.
C      MAXFRQ  I      Maximum number of frequency channels.
C      MAXIFS  I      Maximum number of IFs
C      MAXBL   I      Maximum number of baselines
C      NUMANT  I      Number of antennas
C      NUMBL   I      Number of baselines
C      NUMFRQ  I      Number of frequencies
C      NUMTIM  I      Number of times
C      NUMIF   I      Number of IFS
C      WTT     R(NUMTIM)    Time weight array
C      WTF     R(NUMFRQ)    Frequency weight array
C      WTB     R(NUMBL)    Baseline weight array
C      IC      I      Stokes' number passed, 0 => averaged.
C                        1 = R, 2 = L, 3 = I
C      PRTLV   I      Print level, prints results at .ge. 2
C      TIMEC   D      central time
C   Input/Output:
C      CREAL   R(2,NUMIF,NUMANT)    Real part of solution; includes
C                                   effects of multiband delay.
C      CIMAG   R(2,NUMIF,NUMANT)    Imag part of solution
C      CDELY   R(2,NUMIF,NUMANT)    single plus multi band delays in
C                                   seconds
C      CWT     R(2,NUMIF,NUMANT)    Weights = SNR
C      REFAN   I(2,NUMIF)           Reference antennas used
C   Output:
C      CMBDEL  R(2,NUMANT)          Multiband delays in seconds.
C      CDISP   R(2,NUMANT)          dispersion in seconds/m/m
C-----------------------------------------------------------------------
      INTEGER   MAXFRQ, MAXTIM, MAXIFS, MAXBL, IS(*), JS(*), NUMBL,
     *   NUMFRQ, NUMTIM, NUMANT, NUMIF, IC, PRTLV, REFAN(2,*), DODISP
      REAL      VREAL(MAXTIM,MAXFRQ,MAXBL), VIMAG(MAXTIM,MAXFRQ,MAXBL),
     *   CMBDEL(2,*), CREAL(2,MAXIFS,*), CIMAG(2,MAXIFS,*),
     *   CDELY(2,MAXIFS,*), CWT(2,MAXIFS,*), CDISP(2,*), WTB(*), WTT(*),
     *   WTF(*)
      DOUBLE PRECISION REFREQ, FREQIF(*), FREQS(*), TIMEC
      LOGICAL GDSOLV(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DOUBLE PRECISION TIMPR, FACTM(MAXIF), FACTP(MAXIF), SUMX, SUMXX,
     *   SUML, SUMLX, LAM, X
      CHARACTER CHM*1
      LOGICAL   PTITLE
      INTEGER   LOOP, ISUM, IPOL, HM(2), ISNR(2), NCHAN, NGROUP, IG,
     *   ICHAN, IANT, I, J, K, IBL
      REAL      WT(MAXIF), DAVG, GRDLE(MAXIF), A, P, SNR, SUMSNR, SEC,
     *   DISPE, DELY(MAXIF), W, SWT, GRDLY(MAXIF)
      INCLUDE 'INCS:PSTD.INC'
      DATA  TIMPR / 0.0D0 /
      DATA  PTITLE /.FALSE./
C-----------------------------------------------------------------------
C                                       Print header
      IF (TIMPR.NE.TIMEC) THEN
         TIMPR = TIMEC
         PTITLE = .TRUE.
         END IF
      PTITLE = .TRUE.
      IPOL = MAX (IC, 1)
      IF (IC.EQ.3) IPOL = 1
      NCHAN = NUMFRQ / NUMIF
      NGROUP = MAX (1, DODISP)
C                                       Loop over antenna
      DO 100 IANT = 1,NUMANT
C                                       This one any good?
         CMBDEL(IPOL,IANT) = FBLANK
         CDISP(IPOL,IANT) = FBLANK
         DAVG = 0.0
         SUMSNR = 0.0
         ISUM = 0
         SWT = 0.0
         DO 10 IBL = 1,NUMBL
            IF (((IS(IBL).EQ.IANT) .OR. (JS(IBL).EQ.IANT)) .AND.
     *         (WTB(IBL).GT.0.0)) THEN
               DO 9 J = 1,NUMFRQ
                  IF (WTF(J).GT.0.0) THEN
                     DO 8 K = 1,NUMTIM
                        W = WTB(IBL) * WTF(J) * WTT(K)
                        IF ((ABS (VREAL(K,J,IBL))+ABS (VIMAG(K,J,IBL)))
     *                     .LE.1.0E-20) W = 0.0
                        SWT = SWT + W
 8                      CONTINUE
                     END IF
 9                CONTINUE
               END IF
 10         CONTINUE
C                                       Form part of first line
         CALL COORDD (1, TIMEC, CHM, HM, SEC)
C
         IF ((SWT.GT.1.0E-20) .AND. (CWT(IPOL,1,IANT).GT.0.0001)) THEN
C                                       then IANT is neutral
            IF ((GDSOLV(1) .AND. GDSOLV(1+IANT)) .OR.
     *         (IANT.EQ.REFAN(IPOL,1))) THEN
               CMBDEL(IPOL,IANT) = 0.0
               CDISP(IPOL,IANT) = 0.0
C                                       otherwise IANT is good
            ELSE
               ICHAN = 0
               DO 50 I = 1,NUMIF
                  SUMX = 0.0D0
                  SUMXX = 0.0D0
                  SUML = 0.0D0
                  SUMLX = 0.0D0
                  DO 40 J = 1,NCHAN
                     ICHAN = ICHAN + 1
C                                       sum all channels - works
C                                       better than omitting flagged
                     X = FREQS(ICHAN)*1.D9 - FREQIF(I)
                     LAM = VELITE / (REFREQ + FREQS(ICHAN) * 1.D9)
                     SUMX = SUMX + X
                     SUMXX = SUMXX + X * X
                     SUML = SUML + LAM
                     SUMLX = SUMLX + LAM * X
 40                  CONTINUE
                  X = NCHAN * SUMXX - SUMX * SUMX
                  IF (X.NE.0.0) THEN
                     FACTM(I) = VELITE*(NCHAN * SUMLX - SUMX * SUML) / X
                     FACTP(I) = (SUMXX * SUML - SUMX * SUMLX) / X
                  ELSE
                     FACTM(I) = 0.0
                     FACTP(I) = 0.0
                     END IF
 50               CONTINUE
C                                       Convert to ampl. phase.; average
C                                       SNR.
               DO 60 LOOP = 1,NUMIF
                  IF ((CREAL(IPOL,LOOP,IANT).NE.FBLANK) .AND.
     *               (CIMAG(IPOL,LOOP,IANT).NE.FBLANK) .AND.
     *               (CDELY(IPOL,LOOP,IANT).NE.FBLANK) .AND.
     *               (CWT(IPOL,LOOP,IANT).GT.0.0)) THEN
                     WT(LOOP) = CWT(IPOL,LOOP,IANT)
                     ISUM = ISUM + 1
C                                       Get average SB delay
                     DELY(LOOP) = CDELY(IPOL,LOOP,IANT)
                     DAVG = DAVG + CDELY(IPOL,LOOP,IANT)
C                                       Sum SNR
                     SUMSNR = SUMSNR + WT(LOOP)
                  ELSE
                     DELY(LOOP) = 0.0
                     WT(LOOP) = 0.0
                     END IF
60                CONTINUE
C                                       Less than 2 IF's?
               IF (ISUM.GT.1) THEN
C                                       Poln. SNR average
                  SNR = SQRT (SUMSNR)
                  SNR = MAX (0.01, SNR)
                  ISNR(IPOL) = SNR + 0.5
C                                       Determine MB delay, disp
                  DAVG = DAVG / ISUM
                  CALL DELFIT (NUMIF, DODISP, DELY, WT, FACTM,
     *               GRDLY, GRDLE, CDISP(IPOL,IANT), DISPE)
C                                       Correct phases
                  IF (CDISP(IPOL,IANT).NE.FBLANK) THEN
                     CDISP(IPOL,IANT) = -CDISP(IPOL,IANT)
C                                       Don't write reference ant
                     IF ((IANT.NE.REFAN(IPOL,1)) .AND. (PRTLV.GE.1))
     *                  THEN
C                                       Write Heading
                        IF (PTITLE) THEN
                           WRITE (MSGTXT,1002)
                           CALL MSGWRT (4)
                           WRITE (MSGTXT,1013) CHM, HM, SEC,
     *                        REFAN(IPOL,1)
                           CALL MSGWRT (4)
                           WRITE (MSGTXT,1004)
                           CALL MSGWRT (4)
                           WRITE (MSGTXT,1006)
                           CALL MSGWRT (4)
                           PTITLE = .FALSE.
                           END IF
C                                       Write solution
                        DO 65 IG = 1,NGROUP
                           WRITE (MSGTXT,1025) IANT, DAVG*1.0E9,
     *                        GRDLY(IG)*1.E9, GRDLE(IG)*1.E9,
     *                        CDISP(IPOL,IANT)*1.E9, DISPE*1.E9,
     *                        ISNR(IPOL)
                           CALL MSGWRT (4)
 65                        CONTINUE
                        END IF
                     END IF
                  DO 70 LOOP = 1,NUMIF
                     IF ((CREAL(IPOL,LOOP,IANT).NE.FBLANK) .AND.
     *                  (CIMAG(IPOL,LOOP,IANT).NE.FBLANK)) THEN
C                                       correct them
                        CDELY(IPOL,LOOP,IANT) = CDELY(IPOL,LOOP,IANT)
     *                     - FACTM(LOOP) * CDISP(IPOL,IANT)
                        A = SQRT (CREAL(IPOL,LOOP,IANT)**2 +
     *                     CIMAG(IPOL,LOOP,IANT)**2)
                        P = ATAN2 (CIMAG(IPOL,LOOP,IANT),
     *                     CREAL(IPOL,LOOP,IANT))
                        P = P - TWOPI * VELITE * FACTP(LOOP) *
     *                     CDISP(IPOL,IANT)
                        CREAL(IPOL,LOOP,IANT) = A * COS (P)
                        CIMAG(IPOL,LOOP,IANT) = A * SIN (P)
                        END IF
 70                  CONTINUE
                  END IF
               END IF
            END IF
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1002 FORMAT (20(' - '))
 1004 FORMAT ('Ant    <SBD>      GRD            DISP         SNR')
 1006 FORMAT ('         ns        ns          ns/m/m')
 1013 FORMAT ('Time=',A1, 2I3, F6.2,'   Ref Ant= ',I2)
 1025 FORMAT (I3,F8.2,F10.4,'(',F6.4,')',F8.3,'(',F5.3,')',I5)
      END
      SUBROUTINE DELFIT (NUMIF, DODISP, DELY, PHASWT, FACTM, MB, MBE,
     *   DISP, DISPE)
C-----------------------------------------------------------------------
C   Fit Single-band delays (i) to MBdelay + FACTM(i) * Dispersion
C   Inputs:
C      NUMIF     I      Number of IF.
C      DODISP    I      Divide IFs into this many groups
C      DELY      R(*)   delay for IF (s).
C      PHASWT    R(*)   Weight for IF. (non positive means bad)
C      FACTM     D(*)   LSQ factor dispersion addition to delay
C      PRTLV     I      Print level, 1=MB result,
C                          2=also phase residuals
C   Output:
C      MB        R(*)   group (Multi band) delay (seconds)
C      MBE       R(*)   group (Multi band) delay error (seconds)
C      DISP      R      Dispersion (seconds/m/m)
C      DISPE     R      Dispersion error (seconds/m/m)
C-----------------------------------------------------------------------
      INTEGER   NUMIF, DODISP
      REAL      DELY(NUMIF), PHASWT(NUMIF), MB(*), MBE(*), DISP, DISPE
      DOUBLE PRECISION FACTM(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXPRM
      PARAMETER (MAXPRM=25)
C
      INTEGER   LOOP, COUNT, I, J, K, L, NFIT, IRET, NIFPGR
      DOUBLE PRECISION SUMX, SUMY, SUMXX, SUMXY, DIV, DTEMP1, DTEMP2,
     *   LL(MAXIF), R(MAXPRM), MATR(MAXPRM*MAXPRM), CI, CJ, SSQRES,
     *   VARY, FIT, NOBS , X(MAXPRM), VX(MAXPRM), VARRES
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       all 1 group
      IF (DODISP.LE.1) THEN
         MB(1) = FBLANK
         DISP = FBLANK
C                                       Init sums
         COUNT = 0
         SUMX  = 0.0D0
         SUMXX = 0.0D0
         SUMY  = 0.0D0
         SUMXY = 0.0D0
C                                       Least squares slope
         DO 30 LOOP = 1,NUMIF
            IF (PHASWT(LOOP).GT.0.0) THEN
               COUNT = COUNT + 1
               LL(LOOP) = FACTM(LOOP)
               SUMX  = SUMX + LL(LOOP)
               SUMXX = SUMXX + LL(LOOP) * LL(LOOP)
               SUMY  = SUMY + DELY(LOOP)
               SUMXY = SUMXY + DELY(LOOP) * LL(LOOP)
               END IF
 30         CONTINUE
C                                       Slope
         IF (COUNT.LE.0) GO TO 999
         DIV = COUNT * SUMXX - SUMX * SUMX
         IF (DIV.EQ.0.0) GO TO 999
         MB(1) = (SUMY * SUMXX - SUMX * SUMXY) / DIV
         DISP = (SUMY * SUMX - COUNT * SUMXY) / DIV
C                                       MB uncertainty done right
         MBE(1) = 0.0
         DISPE = 0.0
         DTEMP2 = 0.0D0
         DO 40 LOOP = 1,NUMIF
            IF (PHASWT(LOOP).GT.0.0) THEN
               DTEMP1 = (SUMXX - SUMX * LL(LOOP)) / DIV
               MBE(1) = MBE(1) + DTEMP1*DTEMP1
               DTEMP1 = (SUMX - COUNT * LL(LOOP)) / DIV
               DISPE = DISPE + DTEMP1*DTEMP1
               DTEMP2 = DTEMP2 + (DELY(LOOP)- MB(1) + DISP*LL(LOOP))**2
               END IF
 40         CONTINUE
         DTEMP2 = SQRT (DTEMP2/COUNT)
         MBE(1) = SQRT (MBE(1)) * DTEMP2
         DISPE = SQRT (DISPE) * DTEMP2
C                                       multiple groups
      ELSE
         SUMX  = 0.0D0
         SUMXX = 0.0D0
         CALL DFILL (MAXPRM, 0.0D0, R)
         I = MAXPRM * MAXPRM
         CALL DFILL (I, 0.0D0, MATR)
         NOBS = 0.0D0
         NFIT = 1 + DODISP
         NIFPGR = NUMIF / DODISP
         DO 150 LOOP = 1,NUMIF
            NOBS = NOBS + 1.0D0
            SUMX = SUMX + DELY(LOOP)
            SUMXX = SUMXX + DELY(LOOP) * DELY(LOOP)
            K = (LOOP - 1) / NIFPGR + 2
            DO 130 I = 1,NFIT
               CI = 0.0D0
               IF (I.EQ.1) THEN
                  CI = FACTM(LOOP)
               ELSE IF (I.EQ.K) THEN
                  CI = 1.0D0
                  END IF
               R(I) = R(I) + DELY(LOOP) * CI
               DO 120 J = 1,NFIT
                  CJ = 0.0D0
                  IF (J.EQ.1) THEN
                     CJ = FACTM(LOOP)
                  ELSE IF (J.EQ.K) THEN
                     CJ = 1.0D0
                     END IF
                  L = I + (J-1) * NFIT
                  MATR(L) = MATR(L) + CI * CJ
 120              CONTINUE
 130           CONTINUE
 150        CONTINUE
C                                       do the fit
         CALL DLESQR (NFIT, NOBS, SUMX, SUMXX, R, MATR, X, VX,
     *      SSQRES, VARRES, VARY, FIT, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'DLESQR RETURNS ERROR'
            CALL MSGWRT (8)
            DISP = FBLANK
            DISPE = FBLANK
            DO 160 I = 1,DODISP
               MB(I) = FBLANK
               MBE(I) = FBLANK
 160           CONTINUE
         ELSE
            DISP = -X(1)
            DISPE = SQRT (ABS(VX(1)))
            DO 170 I = 1,DODISP
               MB(I) = X(I+1)
               MBE(I) = SQRT (ABS (VX(I+1)))
 170           CONTINUE
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DELFIT ERROR',I4,' ON ',A)
      END
      LOGICAL FUNCTION WANSRC (SRC, DOCWNT, NCALWD, CALWAN)
C-----------------------------------------------------------------------
C   WANSRC looks through the list of calibrator sources to determine if
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
      WANSRC = .NOT.DOCWNT
      DO 100 I = 1, NCALWD
         IF (CALWAN(I).EQ.SRC) WANSRC = DOCWNT
  100    CONTINUE
      IF (NCALWD.EQ.0) WANSRC = DOCWNT
C
      RETURN
      END
      SUBROUTINE CHKAP (SOLINT, INTTIM, NUMFRQ, NUMIF, FREQS, DOIF,
     *   DOEVLA, RATWIN, DELWIN, IRET)
C-----------------------------------------------------------------------
C   adjust parameters - AP checking now elsewhere
C   check frequency order
C   Inputs:
C      SOLINT        R       Solution interval (sec)
C      INTTIM        R       Integration time (sec).
C      NUMFRQ        I       Number of frqeuencies
C      NUMIF         I       Number of IFs
C      FREQS         D(*)    Frequency array
C      DOIF          L       Treat IFs separately?
C      RATWIN        R       Rate window (0 => no search)
C      DELWIN        R       Delay window (0 => no search)
C
C   Output:
C      IRET          I       0 => data will fit
C                            1 => data will not fit
C-----------------------------------------------------------------------
      INTEGER   NUMTIM, NUMFRQ, NUMIF, DOEVLA, IRET
      REAL      SOLINT, INTTIM, RATWIN, DELWIN
      DOUBLE PRECISION FREQS(*)
      LOGICAL   DOIF
C
C   NFPIF      I        Number of frequencies per IF
C   NUMTIM     I        Number of integrations in SOLINT
C   DF         R        Frequency step
C   APREQ      I        Requested Ap memory (words)
C   APSIZ      I        Actual size of AP (words)
C   FCOUNT     I        Number of frequencies to grid
C
      INTEGER   NFPIF, FCOUNT
      REAL      DF
C
      INTEGER   MT, MF, NR, NF, ND
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NFPIF = NUMFRQ / NUMIF
C                                       Find minimum Freq step,
C                                       and frequency spread
C                                       Only need the spread for 1 IF if
C                                       IFs are separate.
      IF (DOIF) THEN
         FCOUNT = NUMFRQ / NUMIF
      ELSE IF (DOEVLA.GT.0) THEN
         FCOUNT = NUMFRQ / DOEVLA
      ELSE
         FCOUNT = NUMFRQ
         END IF
      CALL CHKFRQ (FREQS, NUMFRQ, FCOUNT, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GRDFRQ (FREQS, FCOUNT, 1.0E-3, NF, DF)
C
      NUMTIM = SOLINT / INTTIM + 1
C                                       Add one for safety.
C
      IF ((RATWIN.LE.0.0).OR.(NUMTIM.LE.1)) THEN
         MT = NUMTIM
      ELSE
         CALL POWER2 (NUMTIM, MT)
         MT = 8 * MT
C                                       Two for next largest power, 4
C                                       for expansion factor.
         END IF
      IF (FCOUNT.GT.1) THEN
         CALL POWER2 (NF, MF)
         MF = 8 * MF
      ELSE
         NF = 1
         MF = 1
         END IF
      NF = MAX (NF, 1)
      IF (DELWIN.LE.0.0) THEN
         MF = 1
         END IF
C
      IF (NUMTIM.GT.1) THEN
         NR = MT * RATWIN * INTTIM / 1000.0
      ELSE
         NR = 1
         END IF
      NR = MAX (NR, 1)
      IF (FCOUNT.GT.1) THEN
         ND = MF * DELWIN * ABS (DF)
      ELSE
         ND = 1
         END IF
      ND = MAX (ND, 1)
C                                       Make sure NR and ND are odd:
      NR = 2 * (NR / 2) + 1
      ND = 2 * (ND / 2) + 1
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE CHKFRQ (FREQS, NUMFRQ, FCOUNT, IRET)
C-----------------------------------------------------------------------
C   CHKFRQ checks the the frequencies are all increasing or all
C   decreasing within each group
C   Inputs:
C      FREQS    D(*)   Frequencies of all channels
C      NUMFRQ   I      Total number of frequencies
C      FCOUNT   I      Number in any one group
C   Output
C      IRET     I      0 all good, 1 mixed slopes
C-----------------------------------------------------------------------
      DOUBLE PRECISION FREQS(*)
      INTEGER   NUMFRQ, FCOUNT, IRET
C
      INTEGER   IG, NG, IC, IE, I, IP
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NG = NUMFRQ / FCOUNT
      NG = MAX (1, NG)
      DO 100 IG = 1,NG
         IC = (IG - 1) * FCOUNT + 1
         IE = IG * FCOUNT
         IF (FREQS(IC+1)-FREQS(IC).GT.0.0D0) THEN
            IP = 1
         ELSE
            IP = -1
            END IF
         DO 20 I = IC+1,IE-1
            IF (FREQS(I+1)-FREQS(I).GT.0.0D0) THEN
               IF (IP.EQ.-1) GO TO 200
            ELSE
               IF (IP.EQ.+1) GO TO 200
               END IF
 20         CONTINUE
 100     CONTINUE
      IRET = 0
      GO TO 999
C                                       mixed sign
 200  MSGTXT = 'FREQUENCY INCREMENTS NOT ALL SAME SIGN IN GROUP'
      CALL MSGWRT (8)
      MSGTXT = 'YOU MAY NEED TASK FLOPM'
      CALL MSGWRT (8)
      MSGTXT = 'WILL TRY SOLUTION ANYWAY - CHECK CAREFULLY'
      CALL MSGWRT (8)
      IRET = 0
C
 999  RETURN
      END
C                                       This is an entirely new subroutine
C                                       modelled upon and designed to
C                                       replace FRNSRC
      SUBROUTINE FRNALL (IS, JS, VREAL, VIMAG, TIME, FREQS, CMBDEL,
     *   CREAL, CIMAG, CDELY, CRATE, CWT, REFAN, MAXFRQ, MAXTIM, MAXIFS,
     *   NUMANT, NUMBL, NUMFRQ, NUMTIM, NUMIF, WGTMOD, WTB, WTT, WTF,
     *   DELWIN, RATWIN, REFANT, IC, SNRMIN, PRTLV, BLDO, DOIF, DOEVLA,
     *   FREQIF, BTCODE, PRIRTY, GDSOLV, IERR)
C-----------------------------------------------------------------------
C   FRNALL makes an initial estimate of the delay, rate and phase
C   of a visibility array on all baselines. Antenna based estimates
C   are then determined relative to the antenna with most detections.
C   One set of values is determined from all IFs together and then
C   filled into all.  If the Stokes' parameters were averaged (IC=0)
C   then the common solutions are copied into both.
C      Currently assumes that all frequencies are spaced by multiples
C   of the minimum spacing.
C   Input:
C    IS(*)                       I    First ant. of baseline numbers
C    JS(*)                       I    2nd ant. of baseline numbers
C    BTCODE(*)                   I    Preferred order of baselines
C    VREAL(MAXTIM,MAXFRQ,MAXBL)  R    Real part of visibility array
C    VIMAG(MAXTIM,MAXFRQ,MAXBL)  R    Imag part of visibility array
C    TIME(*)                     R    Time wrt center
C    FREQS(*)                    D    Frequency array
C    MAXTIM              I    Maximum number of time integrations.
C    MAXFRQ              I    Maximum number of frequency channels.
C    MAXIFS              I    Maximum number of IFs
C    NUMANT              I    Number of antennas
C    NUMBL               I    Number of baselines
C    NUMFRQ              I    Number of frequencies
C    NUMTIM              I    Number of times
C    NUMIF               I    Number of IFs
C    WGTMOD(NUMBL)       R    Weight modification array; used if
C                             unequal integration times in the data.
C    WTT(NUMTIM)         R    Time weight array
C    WTF(NUMFRQ)         R    Frequency weight array
C    DELWIN              R    delay window, <0 => no search in delay
C    RATWIN              R    rate window, <0 => no search in rate
C    REFANT              I    Reference antenna to use if possible.
C    IC                  I    Stokes' number passed, 0 => averaged.
C                             1=R, 2=L, 3=I
C    SNRMIN              R    Minimum SNR allowed
C    PRTLV               I    Print level
C    BLDO                I    -1 use refant set by user, -2 automatic choice
C    DOIF                L    If true then solve each IF independently
C    FREQIF(*)           D    Reference frequency offset per IF (Hz)
C   Output:
C    WTB(NUMBL)                  R    Baseline weight array, returned
C                                     normalized.
C    CMBDEL(2,NUMANT)            I    Multiband delays in seconds.
C    CREAL(2,NUMIF,NUMANT)       R    Real part of solution
C    CIMAG(2,NUMIF,NUMANT)       R    Imag part of solution
C    CDELY(2,NUMIF,NUMANT)       R    delays in seconds.
C    CRATE(2,NUMIF,NUMANT)       R    Rates in Hz.
C    CWT(2,NUMIF,NUMANT)         R    Weights = SNR
C    REFAN(2,NUMIF)      I    Reference antennas used
C    IERR                I    Return code, 0=>OK, 1 => all data bad,
C                             2=>insufficient memory for window.
C-----------------------------------------------------------------------
      INTEGER   MAXFRQ, MAXTIM, MAXIFS, IERR
      INTEGER   IS(*), JS(*), REFAN(2,*), NUMBL, NUMFRQ, NUMTIM, NUMIF,
     *   DOEVLA, NUMANT, REFANT, IC, PRTLV, BLDO, BTCODE(*)
      LOGICAL   DOIF, GDSOLV(*)
      DOUBLE PRECISION FREQIF(*), FREQS(*)
      REAL      VREAL(MAXTIM,MAXFRQ,*), VIMAG(MAXTIM,MAXFRQ,*),
     *   CMBDEL(2,*), CREAL(2,MAXIFS,*), CIMAG(2,MAXIFS,*),
     *   CDELY(2,MAXIFS,*), CRATE(2,MAXIFS,*), CWT(2,MAXIFS,*), TIME(*),
     *   WGTMOD(*), WTB(*), WTT(*), WTF(*), DELWIN, RATWIN,
     *   SNRMIN
      INTEGER   IIF, IB, IST, REFA, ANT, NFRQ, NFPIF, IFP, NOIF, IT, NF,
     *   IA, I, IS1, IS2, N2M1, IFQ, J, ISI, JSI, TREFAN, ITEMP, LIMF1,
     *   LIMF2, I4TEMP, NNT, NNF, MF, MT, ND, NR, NSUB, REFI, REFJ,
     *   BTBL, FCOUNT, APIAD, APINTR, APFIN, NEED, KAP, NIF1, NIF2, NJ
      LOGICAL   TRUE, FALSE, SEARCH, REREFD, FRSMSG, MEMALL
      REAL      AMP, DF, DT, SNRAT, WT, SUMWT, TWOPI, FRATE, FDELAY,
     *   WT0, AVGWTI, SPCNS, SPCMH, RFACT, CTEMP, CPREA, CPIMG, CPRAT,
     *   CPDEL, CPWT, CTREA, CTIMG, CTRAT, CTDEL
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   GOTSLN(MAXANT), NREFA(MAXANT), IBL, PRIRTY(MAXANT), AIF
      REAL      TREAL, TIMAG, TDELY, TRATE, TWT
      REAL      SWT(MAXANT), WTBT(MXBASE), WT1(MXBASE)
      INCLUDE 'INCS:GAIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:PMAD.INC'
      EQUIVALENCE (WT1, HESS)
      SAVE FRSMSG
      DATA TRUE, FALSE /.TRUE.,.FALSE./
      DATA FRSMSG /.TRUE./
C-----------------------------------------------------------------------
      TREFAN = REFANT
      TWOPI = 8. * ATAN (1.)
      NNT = NUMTIM
      MEMALL = .FALSE.
      IST = MAX (IC, 1)
C                                       IQUV?
      IF (IC.EQ.3) IST = 1
C                                       Find minimum Freq step,
C                                       and frequency spread
C                                       Only need the spread for 1 IF if
C                                       IFs are separate.
      IF (DOIF) THEN
         FCOUNT = NUMFRQ / NUMIF
         NOIF = NUMIF
      ELSE IF (DOEVLA.GT.0) THEN
         FCOUNT = NUMFRQ / DOEVLA
         NOIF = DOEVLA
      ELSE
         FCOUNT = NUMFRQ
         NOIF = 1
         END IF
      NFPIF = FCOUNT
      CALL GRDFRQ (FREQS, FCOUNT, 1.0E-3, NF, DF)
      NFRQ = NUMFRQ / NOIF
      NNF = MAX (NFRQ, NF)
C                                       Time step
      IF (NUMTIM.GT.1) THEN
         DT = (TIME(NUMTIM) - TIME(1)) / (NUMTIM - 1.0)
      ELSE
         DT = 0.1 / 86400.0
         END IF
C                                       Normalize data
      DO 20 IB = 1,NUMBL
         DO 20 IIF = 1,NUMFRQ
            DO 20 IT = 1,NUMTIM
               AMP = SQRT (VREAL(IT,IIF,IB)*VREAL(IT,IIF,IB) +
     *            VIMAG(IT,IIF,IB)*VIMAG(IT,IIF,IB))
               IF (AMP.GT.1.0E-15) THEN
                  VREAL(IT,IIF,IB) = VREAL(IT,IIF,IB) / AMP
                  VIMAG(IT,IIF,IB) = VIMAG(IT,IIF,IB) / AMP
                  END IF
 20            CONTINUE
C                                       FFT sizes
C                                       Time/rate dimension
      MT = LOG (16.0 * NUMTIM) / LOG (2.0) + 0.999
      IS1 = 2*MAXIMG
      IS2 = 2 ** MT
      MT = MIN (IS1, IS2)
      IF ((RATWIN.LE.1.0E-20) .OR. (NUMTIM.LE.1)) MT = NUMTIM
C                                       Freq./delay dimension
      MF = 1
      IF (FCOUNT.GT.1) MF = LOG (16.0 * NF) / LOG (2.0) + 0.999
      IS2 = 2**MF
      IF (FCOUNT.GT.1) MF = MIN (IS1, IS2)
      IF (DELWIN.LE.1.0E-20) MF = NF
C                                        search array size.
      ND = 1
      NR = 1
      IF (NUMTIM.GT.1) NR = MT * (RATWIN / (1000.0 / (DT * 86400.0)))
      NR = MAX (1, NR)
      IF (FCOUNT.GT.1) ND = MF * (DELWIN / (1.0 / ABS(DF)))
      ND = MAX (1, ND)
C                                       Make sure odd
      NR = (NR/2) * 2 + 1
      ND = (ND/2) * 2 + 1
C                                       Make sure that it will fit in
C                                       AP
      I4TEMP = 2 * NUMTIM * NNF
      APIAD = 10
      APINTR = APIAD + I4TEMP
      APFIN = APINTR + 2 * NNF * NR
      NEED = APFIN + 2 * ((NR+1) * (ND+1) + MAX (MT, MF))
      NEED = NEED / 1024 + 4
      CALL QINIT (NEED, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) GO TO 900
      IF (PSAPNW.LT.NEED) THEN
         MSGTXT = 'FRNALL: DID NOT GET WHAT I NEED FOR MEMORY'
         CALL MSGWRT (8)
         MSGTXT = 'FRNALL: USE SETMAXAP TO RAISE THE LIMIT, OR'
         CALL MSGWRT (8)
         GO TO 910
         END IF
C                                       Write message if under sampling
C                                       Make sure odd again
      NR = (NR/2) * 2 + 1
      ND = (ND/2) * 2 + 1
      IF ((MF.LT.(4*NF)) .AND. (MF.NE.NF)) THEN
         WRITE (MSGTXT,1020) 4*NF, MF
         CALL MSGWRT (8)
         IF (MT.LT.4*NUMTIM) THEN
            WRITE (MSGTXT,1021) 4*NUMTIM, MT
            CALL MSGWRT (8)
            END IF
         IERR = 2
         GO TO 999
         END IF
      IF ((MT.LT.4*NUMTIM) .AND. (MF.NE.NF)) THEN
         WRITE (MSGTXT,1021) 4*NUMTIM, MT
         CALL MSGWRT (8)
         IERR = 2
         GO TO 999
         END IF
C                                       Make sure WORK declaration in
C                                       FRNSR2 is not exceeded.
      IF (4*NUMTIM.GT.2*MAXIMG) THEN
         MSGTXT = 'FRNALL: WORK BUFFER TOO SMALL IN FRNSR2'
         CALL MSGWRT (8)
         MSGTXT = 'FRNALL: USE A SHORTER SOLINT OR AVG. IN TIME'
         CALL MSGWRT (8)
         IERR = 2
         GO TO 999
         END IF
C                                       Notify user regarding coarse
C                                       search spacing
      IF (PRTLV.GE.2) THEN
         IF (ABS(DF)*MF.GT.0.0) THEN
            SPCNS = 1.0 / (ABS(DF) * MF)
         ELSE
            SPCNS = 0.0
            END IF
         IF (DT*MT.GT.0.0) THEN
            SPCMH = 1000.0 / (DT * MT * 86400.0)
         ELSE
            SPCMH = 0.0
            END IF
C
         WRITE (MSGTXT,1041) SPCNS, SPCMH
         CALL MSGWRT (2)
         END IF
C
      N2M1 = 2 * NUMANT - 1
C                                       Normalize baseline weights.
      SUMWT = 0.0
      DO 50 IB = 1,NUMBL
         IF (WTB(IB).GT.0.0) SUMWT = SUMWT + WTB(IB)
 50      CONTINUE
      AVGWTI = 0.0
      IF (SUMWT.GT.1.0E-10) AVGWTI = NUMBL * 1.0 / SUMWT
      DO 60 IB = 1,NUMBL
         IF (WTB(IB).GT.0.0) WTB(IB) = WTB(IB) * AVGWTI
 60      CONTINUE
C                                       Loop over independent IFs
      DO 400 IIF = 1,NOIF
         IFP = (IIF-1) * NFPIF + 1
         AIF = IIF
         IF (DOEVLA.GT.1) AIF = (IIF - 1) * (NUMIF / DOEVLA) + 1
C                                       Copy baseline weights.
         CALL RCOPY (NUMBL, WTB, WTBT)
         CALL RCOPY (NUMBL, WTB, WT1)
C                                       Determine which antennas have
C                                       data. Initialize arrays
         DO 110 IB = 1,NUMANT
            SWT(IB) = 0.0
            GOTSLN(IB) = -1
 110        CONTINUE
         SUMWT = 0.0
         LIMF1 = IFP
         LIMF2 = IFP + NFRQ - 1
         DO 140 IB = 1,NUMBL
            IF (WT1(IB).NE.0.0) THEN
               ISI = IS(IB)
               JSI = JS(IB)
               DO 130 IFQ = LIMF1,LIMF2
                  IF (WTF(IFQ).NE.0.0) THEN
                     WT0 = WT1(IB) * WTF(IFQ)
                     DO 120 IT = 1,NUMTIM
                        IF ((ABS (VREAL(IT,IFQ,IB)) +
     *                     ABS (VIMAG(IT,IFQ,IB))).GE.1.0E-20) THEN
                           WT = WTT(IT) * WT0
                           SWT(ISI) = SWT(ISI) + WT
                           SWT(JSI) = SWT(JSI) + WT
                           END IF
 120                    CONTINUE
                     END IF
 130              CONTINUE
               END IF
 140        CONTINUE
C                                       init those antennas for which we have
C                                       data but are not to be solved for!
         IF (GDSOLV(1)) THEN
            DO 142 I = 1,NUMANT
               IF ((GDSOLV(I+1)).AND.(SWT(I).GT.0.0)) THEN
                  GOTSLN(I) = 0
                  CREAL(IST,IIF,I) = 1.0
                  CIMAG(IST,IIF,I) = 0.0
                  CRATE(IST,IIF,I) = 0.0
                  CDELY(IST,IIF,I) = 0.0
                  CWT(IST,IIF,I)   = SNRMIN + 1.0
                  END IF
 142           CONTINUE
            END IF
C                                       when all is said and done,
C                                       we go back and refer GOTSLN to
C                                       whatever is the reference antenna!
C
C                                        Loop over baselines
         NSUB = 0
C
C        Invariant (partial): GOTSLN(i) = -1 if i has not yet been
C            referenced to  a reference antenna, GOTSLN(i) = 0 if i is
C            an antenna in the set not be solved for, otherwise
C            GOTSLN(i) is the antenna to which i is referenced;
C            GOTSLN(i) = i implies that i is a reference antenna;
C            NSUB is the number of disjoint subarrays found.
C
C
         DO 350 BTBL = 1,NUMBL
            IBL = BTCODE(BTBL)
C                                        See if antennas have any data
C                                       Get antenna codes
            ISI = IS(IBL)
            JSI = JS(IBL)
C                                       and the designated reference antennas
C                                       for each antenna
            REFI = GOTSLN(ISI)
            REFJ = GOTSLN(JSI)
C
            IF ((REFI .LT. 0) .OR. (REFJ .LT. 0)) THEN
C                                       One or both antennae have not
C                                       yet been connected to a
C                                       reference antenna so a search is
C                                       required.
               SEARCH = .TRUE.
            ELSE
C                                       A search is needed if the
C                                       antennae do not have a common
C                                       reference.
C
C              Invariant: REFI is a reference antenna for ISI or 0
C              Bound: The number of items in the reference antenna
C                     chain starting at REFI
  351          IF (REFI .NE. 0) THEN
                  IF (GOTSLN(REFI) .NE. REFI) THEN
C
C                    REFI is not at the end of the reference chain so
C                    move to the next level (decreasing the length of
C                    the reference chain by 1):
C
                     REFI = GOTSLN(REFI)
                     GO TO 351
                  END IF
               END IF
C
C              Invariant: REFJ is a reference antenna for JSI or 0
C              Bound: The number of items in the reference antenna
C                     chain starting at REFI
  352          IF (REFJ .NE. 0) THEN
                  IF (GOTSLN(REFJ) .NE. REFJ) THEN
C
C                    REFJ is not at the end of the reference chain so
C                    move to the next level (decreasing the length of
C                    the reference chain by 1):
C
                     REFJ = GOTSLN(REFJ)
                     GO TO 352
                  END IF
               END IF
C
C              REFI is now the ultimate reference antenna for ISI and
C              REFJ is now the ultimate reference antenna for JSI.
C
               SEARCH = REFI .NE. REFJ
            END IF
C                                       Check baseline weights before
C                                       going on
            SEARCH = SEARCH.AND.(SWT(ISI).GT.0.0).AND.(SWT(JSI).GT.0.0)
C*********************************************************
C                                       NEED MORE HERE!
C                                       check what kinds of paraonia
C                                       FRNSRC takes care of and make
C                                       sure that we do those things
C                                       here also!
C**********************************************************
            IF (SEARCH) THEN
C                                       init soln before going in.
               TREAL = FBLANK
               TIMAG = FBLANK
               TDELY = FBLANK
               TRATE = FBLANK
               TWT = 0.0
C                                       Do stacked(?) BS search
               CALL FRNSR2 (IFP, VREAL, VIMAG, TIME, FREQS, DT, DF,
     *            MAXFRQ, MAXTIM, NUMANT, NFRQ, NUMTIM, WGTMOD, WTBT,
     *            WTT, WTF, ISI, JSI, FREQIF(AIF), BLDO, MF, MT, NR, ND,
     *            APIAD, APINTR, APFIN, TREAL, TIMAG, TDELY, TRATE, TWT)
C                                       If stacked, and failed, try unstacked
               IF ((BLDO.GT.1) .AND. (TWT.LT.SNRMIN)) THEN
                  ITEMP = 1
                  CALL FRNSR2 (IFP, VREAL, VIMAG, TIME, FREQS, DT, DF,
     *               MAXFRQ, MAXTIM, NUMANT, NFRQ, NUMTIM, WGTMOD, WTBT,
     *               WTT, WTF, ISI, JSI, FREQIF(AIF), ITEMP, MF, MT, NR,
     *               ND, APIAD, APINTR, APFIN, TREAL, TIMAG, TDELY,
     *               TRATE, TWT)
C                                       The old FRING here, did something
C                                       very strange.  was it the correct
C                                       thing to do?  See CWTTST in FRNSRC...
                  END IF

C                                       Tell results if requested
               IF ((PRTLV.GE.2) .AND. (TDELY.NE.FBLANK) .AND.
     *            (TRATE.NE.FBLANK)) THEN
                  IF (FRSMSG) THEN
                     MSGTXT = 'B = Baseline  R = Rate (mHz)  ' //
     *                 'D = Delay (nsec)'
                     CALL MSGWRT (2)
                     FRSMSG = .FALSE.
                     END IF
                  FDELAY = TDELY * 1.0E9
                  FRATE = TRATE * 1000.0
                  SNRAT = MIN (999.0, TWT)
                  WRITE (MSGTXT,2002,ERR=275) ISI, JSI, AIF, FRATE,
     *               FDELAY, SNRAT
 275              CALL MSGWRT (2)
                  END IF
C                                       Cut off for bad ones
               IF (TWT.LT.SNRMIN) TWT = 0.0
C                                       Count statistics for good solns
               IF (TWT .GT. 0.0) THEN
C                                       load current soln
                  CPREA = TREAL
                  CPIMG = TIMAG
                  CPRAT = TRATE
                  CPDEL = TDELY
                  CPWT  = TWT*2.0
                  REFI  = ISI
                  REFJ  = JSI
C
C                 Invariant: CRREA, CPIMG, CPRAT, CPDEL, and CPWT are
C                            cosine of phase, sine of phase, rate, delay
C                            and weight for baseline REFI - REFJ
C                 Bound: number of antennae left in reference chain for
C                        REFI
C
  353             IF (GOTSLN(REFI) .GT. 0) THEN
                     IF (GOTSLN(REFI) .NE. REFI) THEN
C
C                       Move to next reference antenna in the chain
C
                        CTEMP = CPREA * CREAL(IST, IIF, REFI)
     *                          - CPIMG * CIMAG(IST, IIF, REFI)
                        CPIMG = CPREA * CIMAG(IST, IIF, REFI)
     *                          + CPIMG * CREAL(IST, IIF, REFI)
                        CPREA = CTEMP
                        CPRAT = CPRAT + CRATE(IST, IIF, REFI)
                        CPDEL = CPDEL + CDELY(IST, IIF, REFI)
                        CPWT = 1.0 / (1.0 / CPWT
     *                                + 1.0 / CWT(IST, IIF, REFI))
                        REFI = GOTSLN(REFI)
                        GO TO 353
                     END IF
                  END IF
C
C                 Invariant: CRREA, CPIMG, CPRAT, CPDEL, and CPWT are
C                            cosine of phase, sine of phase, rate, delay
C                            and weight for baseline REFI - REFJ
C                 Bound: number of antennae left in reference chain for
C                        REFJ
C
  354             IF (GOTSLN(REFJ) .GT. 0) THEN
                     IF (GOTSLN(REFJ) .NE. REFJ) THEN
C
C                       Move to next reference antenna in the chain
C
                        CTEMP = CPREA * CREAL(IST, IIF, REFJ)
     *                          - CPIMG * CIMAG(IST, IIF, REFJ)
                        CPIMG = CPIMG * CREAL(IST, IIF, REFJ)
     *                          - CPREA * CREAL(IST, IIF, REFJ)
                        CPREA = CTEMP
                        CPRAT = CPRAT - CRATE(IST, IIF, REFJ)
                        CPDEL = CPDEL - CDELY(IST, IIF, REFJ)
                        CPWT = 1.0 / (1.0 / CPWT
     *                                + 1.0 / CWT(IST, IIF, REFJ))
                        REFJ = GOTSLN(REFJ)
                        GO TO 354
                     END IF
                  END IF
C
C                 For each of REFI and REFJ there are three cases.
C                 1 - If GOTSLN(REFI) is positive then ISI is referenced
C                     to REFI
C                 2 - If GOTSLN(REFI) is zero then ISI is referenced to
C                     an antenna in the do-not-solve set
C                 3 - If GOTSLN(REFI) is negative then ISI has not been
C                     referenced to any other antenna.
C                 With similar conditions relating REFJ and JSI.
C
                  IF (GOTSLN(REFI) .GT. 0) THEN
                     IF (GOTSLN(REFJ) .GT. 0) THEN
C
C                       Choose the higher priority reference antenna
C                       and decrement subarray count since we have
C                       linked two separate subarrays:
C
                        IF (PRIRTY(REFI) .LT. PRIRTY(REFJ)) THEN
                           REFA  = REFJ
                           ANT   = REFI
                           RFACT = -1.0
                        ELSE
                           REFA  = REFI
                           ANT   = REFJ
                           RFACT = +1.0
                        END IF
                        NSUB = NSUB - 1
                     ELSE IF (GOTSLN(REFJ) .EQ. 0) THEN
C
C                       Choose REFJ as the reference and decrement the
C                       subarray count:
C
                        REFA  = REFJ
                        ANT   = REFI
                        RFACT = -1.0
                        NSUB  = NSUB - 1
                     ELSE
C
C                       REFI must be the reference:
C
                        REFA  = REFI
                        ANT   = REFJ
                        RFACT = +1.0
                     END IF
                  ELSE IF (GOTSLN(REFI) .EQ. 0) THEN
                     IF (GOTSLN(REFJ) .EQ. 0) THEN
C
C                       There is nothing to do (probably shouldn't
C                       reach this branch)
C
                     ELSE
C
C                       Reference REFJ to REFI:
C
                        REFA  = REFI
                        ANT   = REFJ
                        RFACT = +1.0
                        IF (GOTSLN(REFJ) .GT. 0) THEN
C
C                          Two subarrays have been connected so
C                          decrement the array count:
C
                           NSUB = NSUB - 1
                        ELSE
C
C                          Set the subarray count if this is the
C                          first antenna referenced to the no-solve
C                          set:
C
                           IF (NSUB .EQ. 0) THEN
                              NSUB = 1
                           END IF
                        END IF
                     END IF
                  ELSE
                     IF (GOTSLN(REFJ) .GT. 0) THEN
C
C                    Reference REFI to REFJ:
C
                        REFA  = REFJ
                        ANT   = REFI
                        RFACT = -1.0
                     ELSE IF (GOTSLN(REFJ) .EQ. 0) THEN
C
C                       Reference REFI to the REFJ:
C
                        REFA  = REFJ
                        ANT   = REFI
                        RFACT = -1.0
C
C                       Set the subarray count if this is the first
C                       time an antenna has been reference to the
C                       no-solve set:
C
                        IF (NSUB .EQ. 0) THEN
                           NSUB = 1
                        END IF
                     ELSE
C
C                       REFI - REFJ is the start of a disconnected
C                       subarray so choose the highest priority
C                       reference and increment the subarray count:
C
                        IF (PRIRTY(REFI) .LT. PRIRTY(REFJ)) THEN
                           REFA  = REFJ
                           ANT   = REFI
                           RFACT = -1.0
                        ELSE
                           REFA  = REFI
                           ANT   = REFJ
                           RFACT = +1.0
                        END IF
                        CREAL(IST, IIF, REFA) = 1.0
                        CIMAG(IST, IIF, REFA) = 0.0
                        CRATE(IST, IIF, REFA) = 0.0
                        CDELY(IST, IIF, REFA) = 0.0
                        CWT(IST, IIF, REFA) = SNRMIN + 1.0
                        GOTSLN(REFA) = REFA
                        NSUB = NSUB + 1
                     END IF
                  END IF
C                                       save CP as ANT soln
                  CREAL(IST,IIF,ANT) = CPREA
                  CIMAG(IST,IIF,ANT) = CPIMG * RFACT
                  CRATE(IST,IIF,ANT) = CPRAT * RFACT
                  CDELY(IST,IIF,ANT) = CPDEL * RFACT
                  CWT(IST,IIF,ANT)   = CPWT
C                                       set refant for ANT to REFA
                  GOTSLN(ANT) = REFA
C                                       check how these weights should add
                  END IF
               END IF
 350        CONTINUE
C                                       No solns found, leave NOW.
         IF (NSUB.EQ.0) GO TO 400
C                                       NSUB is the number of distinct refants
C                                       that survived the fringe search
C
C                                       announce the presence of disjoint
C                                       subarrays
         IF (NSUB.GT.1) THEN
            MSGTXT = 'The data have partitioned into multiple disjoint'
            CALL MSGWRT (8)
            MSGTXT = '  subarrays.  Must flag baselines with good ' //
     *         'solutions'
            CALL MSGWRT (8)
            MSGTXT = '  since phases cannot be joined.  Try a lower ' //
     *         'SNR limit'
            CALL MSGWRT (8)
            MSGTXT = '  or APARM(9) = 0'
            CALL MSGWRT (8)
            END IF
C                                       rereference all possible antennas
C                                       to the refants.
 357     CONTINUE
         REREFD = FALSE
         DO 355 ANT = 1, NUMANT
C                                       compute ANTs refant
            REFA = GOTSLN(ANT)
            IF (REFA.GT.0) THEN
C                                       -> ANT is a good antenna
               IF (REFA.EQ.ANT) THEN
C                                       -> ANT is a itself a refant
               ELSE IF (REFA.EQ.GOTSLN(REFA)) THEN
C                                       -> ANT is referred to a refant
               ELSE IF (REFA.NE.GOTSLN(REFA)) THEN
C                                       -> ANT is referred to a non-refant
C
C                                       load ANT soln into CP
                  CPREA = CREAL(IST,IIF,ANT)
                  CPIMG = CIMAG(IST,IIF,ANT)
                  CPRAT = CRATE(IST,IIF,ANT)
                  CPDEL = CDELY(IST,IIF,ANT)
C                                       load REFA soln into CT
                  CTREA = CREAL(IST,IIF,REFA)
                  CTIMG = CIMAG(IST,IIF,REFA)
                  CTRAT = CRATE(IST,IIF,REFA)
                  CTDEL = CDELY(IST,IIF,REFA)
C                                       add CP to CT and save as ANT soln
                  CREAL(IST,IIF,ANT) = CPREA * CTREA - CPIMG * CTIMG
                  CIMAG(IST,IIF,ANT) = CPREA * CTIMG + CPIMG * CTREA
                  CRATE(IST,IIF,ANT) = CPRAT + CTRAT
                  CDELY(IST,IIF,ANT) = CPDEL + CTDEL
                  GOTSLN(ANT) = GOTSLN(REFA)
                  CWT(IST,IIF,ANT) = 1.0/ (1.0/CWT(IST,IIF,ANT) +
     *                                     1.0/CWT(IST,IIF,REFA)  )
                  REREFD = TRUE
                  END IF
               END IF
 355     CONTINUE
C                                       keep going back until nothing left
C                                       to rereference
         IF (REREFD) GO TO 357
C
C                                       If the reference antenna has a bad
C                                       soln, then dechoose it
         IF (GOTSLN(TREFAN).LT.0) TREFAN = 0
C
C                                       If no refant and subset solve chosen
         IF (GDSOLV(1).AND.(TREFAN.EQ.0)) THEN
            J = -1
            DO 356 IA = 1,NUMANT
C                                       then take the nosolve antenna
               IF (GDSOLV(1+IA)     .AND.
C                                       with data
     *             (SWT(IA).GT.0.0) .AND.
C                                       that has the highest priority
     *             (PRIRTY(IA).GT.J)     ) THEN
C                                       as the reference antenna
                  TREFAN = IA
                  J = PRIRTY(TREFAN)
                  END IF
 356           CONTINUE
            END IF
C                                       If nothing matches, or if subset solve
C                                       not chosen, then REFANT still = 0 so
         IF (TREFAN.EQ.0) THEN
C                                       zero the counter array
            DO 359 IA = 1,NUMANT
               NREFA(IA) = 0
 359           CONTINUE
C                                       pick up most commonly occuring antenna
C                                       with highest priority on user list
            DO 360 IA = 1,NUMANT
               REFA = GOTSLN(IA)
               IF (REFA.GT.0) THEN
C                                       increment refant counter
                  NREFA(REFA) = NREFA(REFA) + 1
C                                       pick up first occuring REFANT
                  IF (TREFAN.EQ.0) TREFAN = REFA
C                                       pick up most commonly occuring refant
                  IF (NREFA(REFA).GT.NREFA(TREFAN)) TREFAN = REFA
C
                  IF ((NREFA(REFA).EQ.NREFA(TREFAN)).AND.
     *               (PRIRTY(REFA).GT.PRIRTY(TREFAN)) ) TREFAN = REFA
                  END IF
 360           CONTINUE
            END IF
C                                       take the REFANT thusly chosen
C                                       as the refant for this soln interval
         REFAN(IST,IIF) = TREFAN
C                                       If TREFAN = 0 at this stage, all
C                                       solns are blanked since no good
C                                       reference antenna was found!
C                                       blank bad solns and
C                                       solns not referenced to this REFANT
         DO 375 IA = 1,NUMANT
            IF ((GOTSLN(IA).NE.TREFAN).AND.(GOTSLN(IA).GT.0)) THEN
               CREAL(IST,IIF,IA) = FBLANK
               CIMAG(IST,IIF,IA) = FBLANK
               CDELY(IST,IIF,IA) = FBLANK
               CRATE(IST,IIF,IA) = FBLANK
               CWT(IST,IIF,IA) = 0.0
               WRITE (MSGTXT, 2006) IA
               CALL MSGWRT (8)
               END IF
 375        CONTINUE
C                                       End independent IF loop
 400     CONTINUE
C                                       Copy solns. to all IFs
      IF ((NUMIF.GT.1) .AND. (.NOT.DOIF)) THEN
         IF (DOEVLA.GT.0) THEN
            DO 420 NJ = DOEVLA,1,-1
               NIF1 = NUMIF/DOEVLA
               NIF2 = NJ * NIF1
               NIF1 = NIF2 - NIF1 + 1
               NIF1 = MAX (2, NIF1)
               DO 410 IIF = NIF1,NIF2
                  DO 405 IA = 1,NUMANT
                     CREAL(IST,IIF,IA) = CREAL(IST,NJ,IA)
                     CIMAG(IST,IIF,IA) = CIMAG(IST,NJ,IA)
                     CDELY(IST,IIF,IA) = CDELY(IST,NJ,IA)
                     CRATE(IST,IIF,IA) = CRATE(IST,NJ,IA)
                     CWT(IST,IIF,IA) = CWT(IST,NJ,IA)
 405                 CONTINUE
                  REFAN(IST,IIF) = REFAN(IST,NJ)
 410              CONTINUE
 420           CONTINUE
            END IF
         END IF
C                                       If combined Stokes, copy
      IF (IC.LE.0) THEN
         DO 485 IIF = 1,NUMIF
            DO 480 IA = 1,NUMANT
               CMBDEL(2,IA) = CMBDEL(1,IA)
               CREAL(2,IIF,IA) = CREAL(1,IIF,IA)
               CIMAG(2,IIF,IA) = CIMAG(1,IIF,IA)
               CDELY(2,IIF,IA) = CDELY(1,IIF,IA)
               CRATE(2,IIF,IA) = CRATE(1,IIF,IA)
               CWT(2,IIF,IA)   = CWT(1,IIF,IA)
 480           CONTINUE
            REFAN(2,IIF) = REFAN(1,IIF)
 485        CONTINUE
         END IF
      GO TO 999
C                                       error
 900  MSGTXT = 'FRNALL: DYNAMIC MEMORY AP FAILS'
      CALL MSGWRT (8)
      MSGTXT = 'FRNALL: MEMORY TOO SMALL FOR SPECIFIED FFT SEARCH'
      CALL MSGWRT (8)
 910  MSGTXT = 'REDUCE DELAY AND/OR RATE WINDOW OR AVERAGE IN'
      CALL MSGWRT (8)
      MSGTXT = 'FREQUENCY OR USE A SHORTER SOLINT'
      CALL MSGWRT (8)
      IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('FRNALL: MIN FFT IN FREQ REQUESTED',I8,' MAX ALLOWED',I7)
 1021 FORMAT ('FRNALL: MIN FFT IN TIME REQUESTED',I8,' MAX ALLOWED',I7)
 1041 FORMAT ('Coarse search spacing:',E12.3,' ns;',E12.3,' mHz')
 2002 FORMAT ('B=',I3.2,' -',I3.2,' IF=',I3,' R=',F10.2,' D=',F10.1,
     *   ' SNR=',F6.1)
 2006 FORMAT ('No path from REFANT to ANT #',I3,', blanking solution')
      END
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
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('CALZER: TABIO ERROR',I3,' on record ',I5)
      END
      SUBROUTINE GRDFRQ (FREQS, FCOUNT, FTOL, NF, DF)
C-----------------------------------------------------------------------
C   Given a list of FCOUNT channel frequencies in FREQS such that
C   FREQS(i) >= FREQS(1) for all i such that 1 <= i <= FCOUNT, calculate
C   the minimum spacing between adjacent frequencies DF (where the
C   adjacent frequencies are not identical to within FTOL) and the
C   number of grid cells NF required to hold all FCOUNT frequencies at
C   spacing DF.
C
C   Issue a warning message if any frequency FREQS(i) is more than
C   FTOL away from a grid point and if no such warning has yet been
C   issued.
C
C   If FCOUNT is 1 then NF = 1 and DF = 1.0E20 (larger than any
C   reasonable bandwidth)
C
C   IF FCOUNT is greater than 1 then FREQS(2) must differ from FREQS(1).
C
C   Inputs:
C      FREQS     D(*)  List of frequencies in GHz
C      FCOUNT    I     Number of elements to consider in FREQS
C      FTOL      R     Frequency tolerance - fractional
C
C   Outputs:
C      NF        I     Number of grid cells
C      DF        R     Grid spacing in GHz
C-----------------------------------------------------------------------
      DOUBLE PRECISION FREQS(*)
      REAL    FTOL, DF
      INTEGER FCOUNT, NF
C
C     Local variables:
C
C     FRANGE    Frequency range
C     F         Frequency counter
C     FERROR    Offset of frequency from grid point
C     UNGRID    True iff frequencies do not fit on grid
C     WARNED    Has an uneven-grid warning been issued
C
      REAL      FRANGE
      INTEGER   F
      REAL      FERROR
      LOGICAL   UNGRID, WARNED
      DOUBLE PRECISION DPDF, FMIN, FMAX
      SAVE      WARNED
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA WARNED /.FALSE./
C-----------------------------------------------------------------------
      IF (FCOUNT.LE.1) THEN
         DPDF = 1.0E20
         NF = 1
      ELSE
C                                       Find minimum spacing,
C                                       maximum range
C
         DPDF   = ABS (FREQS(2) - FREQS(1))
         FMIN = 1.D15
         FMAX = 0.0D0
         FRANGE = ABS (FREQS(2) - FREQS(1))
C                                       Invariant: FRANGE is the range
C                                       of frequencies spanned by
C                                       FREQS(1:F-1)
         DO 10 F = 3,FCOUNT
            IF ((ABS(FREQS(F)-FREQS(F-1)).LT.DPDF) .AND.
     *         (ABS(FREQS(F)-FREQS(F-1)).GT.FTOL*DPDF))
     *         DPDF = ABS (FREQS(F) - FREQS(F-1))
            IF (ABS (FREQS(F)-FREQS(1)).GT.FRANGE)
     *         FRANGE = ABS (FREQS(F) - FREQS(1))
            FMIN = MIN (FREQS(F), FMIN)
            FMAX = MAX (FREQS(F), FMAX)
 10         CONTINUE
         IF (FMAX-FMIN.GT.FRANGE) FRANGE = FMAX - FMIN
         NF = NINT (FRANGE / ABS(DPDF)) + 1
C                                       If no uneven gridding warning
C                                       message has been issued then
C                                       check for uneven gridding:
         IF (.NOT.WARNED) THEN
            UNGRID = .FALSE.
            DO 20 F = 2,FCOUNT
               FERROR = MOD (ABS (FREQS(F) - FREQS(1)), DPDF)
C
C              Note that there is a possibility of getting the wrong
C              grid point from MOD.
C
               IF ((FTOL*DPDF.LT.FERROR) .AND.
     *            (FERROR.LT.(DPDF-FTOL*DPDF))) UNGRID = .TRUE.
 20            CONTINUE
C
            IF (UNGRID) THEN
               MSGTXT = 'FREQUENCIES DO NOT LIE ON A UNIFORM GRID.'
               CALL MSGWRT (6)
               MSGTXT = 'THIS MAY DEGRADE DETERMINATION OF DELAYS.'
               CALL MSGWRT (6)
               WARNED = .TRUE.
               END IF
            END IF
         END IF
C
      DF = DPDF
      IF (FREQS(2).LT.FREQS(1)) DF = -DPDF
C
      END

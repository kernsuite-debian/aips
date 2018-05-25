LOCAL INCLUDE 'RLDLY.INC'
C                                       Include RLDLY
C                                       Local include for RLDLY
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXPRM, XBFSZ, MAXSCN
C                                       MAXPRM = maximum no. parms in
C                                       Least squares solutions
      PARAMETER (MAXPRM = MAXANT * 3)
C                                       XBFSZ = buffer size
      PARAMETER (XBFSZ = UVBFSL)
C                                       MAXSCN = max cal scans
      PARAMETER (MAXSCN = 100)
C
      INTEGER   CATIN(256), SEQIN, DISKIN, CNOIN, JBUFSZ, BUFFS(XBFSZ),
     *   NANT, NFREQ, REFANT, NPOL, NVAL, VISDSK, VISCNO, VER, NUMNOD,
     *   NUMBL, NUMTIM, PRTLV, MINNO, LOCIF, LOCF, CHINC, CLIVER,
     *   CLOVER, SNOVER, DOEVLA, LBIF, LEIF, NUMSCN, SRCSCN(MAXSCN)
      LOGICAL   SINGLE, DOMODL, TSMOTH, GDSOLV(MAXANT+1)
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XCALCO*4
      REAL      XSI, XDI, XQUAL, XBAND, XFREQ, XFQID, XTIME(8), XBCHAN,
     *   XECHAN, XCHINC, XANTS(50), XSUBA, XUVRA(2), XWTUV, XWTIT,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH(3), XREFA, XSOLIN, DOIFS, XBIF, XEIF, aparm(10), XPRTL,
     *   XBADD(10), DELTIM, BUFF1(XBFSZ), BUFF2(XBFSZ), IATOFF, TINT,
     *   TINTG, SOLINT, DELWIN, SNRMIN, MXPABL, MNPABL, WTPABL, RINWIN,
     *   RATWIN, RLDELY(MAXIF), RPHASE(MAXIF), RRMS(MAXIF),
     *   TIMSCN(2,MAXSCN)
      DOUBLE PRECISION  RANOD, DECNOD
      REAL      FINC(MAXIF)
      INTEGER   ISBAND(MAXIF)
      COMMON /CINFO/ RANOD, DECNOD, CATIN, DELTIM, TINT, IATOFF, TINTG,
     *   SOLINT, DELWIN, SNRMIN, MXPABL, MNPABL, WTPABL, SINGLE, DOMODL,
     *   TSMOTH, RINWIN, GDSOLV, RATWIN, CNOIN, NANT, NFREQ, NPOL,
     *   NVAL, REFANT, VISDSK, VISCNO, VER, NUMNOD, NUMBL, NUMTIM,
     *   PRTLV, MINNO, LOCIF, LOCF, DISKIN, SEQIN, CHINC, CLIVER,
     *   CLOVER, SNOVER, DOEVLA, LBIF, LEIF, RLDELY, RPHASE, RRMS,
     *   NUMSCN, TIMSCN, SRCSCN
      COMMON /BUFRS/ BUFF1, BUFF2, BUFFS, FINC, ISBAND, JBUFSZ
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XCALCO
      COMMON /XINPUT/ XNAMEI, XCLAIN, XSI, XDI, XXSOUR, XQUAL, XXCALC,
     *   XBAND, XFREQ, XFQID, XTIME, XBCHAN, XECHAN, XCHINC, XANTS,
     *   XSUBA, XUVRA, XWTUV, XWTIT, XDOCAL, XGUSE, XDOPOL, XPDVER,
     *   XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XREFA, XSOLIN, DOIFS,
     *   XBIF, XEIF, APARM, XPRTL, XBADD
C                                                          End RLDLY
LOCAL END
      PROGRAM RLDLY
C-----------------------------------------------------------------------
C! Fringe fit interferometer data for R-L delay difference
C# UV Calibration AP-appl VLBA
C-----------------------------------------------------------------------
C;  Copyright (C) 2010-2016
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
C   This task determines the delay difference between R and L
C   polarizations for uv data.  A CL table will be written to be
C   applied to a CL table or the data as neede3d.
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
C   BADDISK            0.0         9.0 Disk no. not to use for
C                                         scratch files.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   MAXBL, MAXFRQ, MAXTIM, NUMSUB, ISUB, IS1, IS2, IR1, IR2,
     *   IST, IRET, I, ISCAN, BUFFER(512,2), IREFAN
      LOGICAL   GOTD, GOTDS, GOTS, GOTSS
      INCLUDE 'RLDLY.INC'
      REAL      TIMEI, RWTS(MAXIF)
      DOUBLE PRECISION SLDELY(MAXIF), SPHASE(2,MAXIF), SWTS(MAXIF), WT,
     *   SSDELY(MAXIF), ARMS
      DOUBLE PRECISION DTIME
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA PRGM /'RLDLY '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL RLDLIN (PRGM, MAXBL, MAXFRQ, MAXTIM, NUMSUB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Loop over subarrays
      GOTDS = .FALSE.
      GOTSS = .FALSE.
      IST = SUBARR
      IF (SUBARR.EQ.0) THEN
         IS1 = 1
         IS2 = NUMSUB
      ELSE
         IS1 = SUBARR
         IS2 = SUBARR
         END IF
      IREFAN = REFANT
      IF (REFANT.LE.0) THEN
         IR1 = 1
         IR2 = NANT
      ELSE
         IR1 = REFANT
         IR2 = REFANT
         END IF
      DO 100 ISUB = IS1,IS2
         WRITE (MSGTXT,1000) ISUB, IS2
         IF (IS1.NE.IS2) CALL MSGWRT (4)
         SUBARR = ISUB
         CALL RFILL (8, 0.0, TIMRNG)
         DO 90 ISCAN = 1,NUMSCN
            GOTD = .FALSE.
            GOTS = .FALSE.
            TIMRNG(1) = TIMSCN(1,ISCAN)
            TIMRNG(5) = TIMSCN(2,ISCAN)
            CALL DFILL (EIF, 0.0D0, SLDELY)
            CALL DFILL (EIF, 0.0D0, SSDELY)
            CALL DFILL (2*EIF, 0.0D0, SPHASE)
            CALL DFILL (EIF, 0.0D0, SWTS)
            DO 30 REFANT = IR1,IR2
               WRITE (MSGTXT,1001) REFANT
               IF (IR1.NE.IR2) CALL MSGWRT (4)
C                                       Select data.
               CALL RLDSEL (IRET)
               IF (IRET.GT.0) GO TO 990
               IF ((NVIS.GT.0) .AND. (IRET.EQ.0)) THEN
C                                       Check if data found
                  GOTD = .TRUE.
                  GOTDS = .TRUE.
C                                       Do solutions.
                  CALL RLDSOL (MAXBL, MAXTIM, MAXFRQ, IRET)
                  IF (IRET.GT.0) GO TO 990
C                                       sum up
                  IF (IRET.EQ.0) THEN
                     DO 20 I = 1,EIF
                        IF ((RRMS(I).GT.0.0) .AND.
     *                     (RRMS(I).LT.APARM(3))) THEN
                           GOTS = .TRUE.
                           GOTSS = .TRUE.
                           WT = 1.0D0 / (RRMS(I)**2)
                           SLDELY(I) = SLDELY(I) + WT * RLDELY(I)
                           SSDELY(I) = SSDELY(I) + WT * RLDELY(I) *
     *                        RLDELY(I)
                           SPHASE(1,I) = SPHASE(1,I) +
     *                        WT * COS (RPHASE(I))
                           SPHASE(2,I) = SPHASE(2,I) +
     *                        WT * SIN (RPHASE(I))
                           SWTS(I) = SWTS(I) + WT
                           END IF
 20                     CONTINUE
                     END IF
                  END IF
 30            CONTINUE
            IF (GOTS) THEN
               WRITE (MSGTXT,1080)
               CALL MSGWRT (4)
               DO 50 I = 1,EIF
                  RWTS(I) = SWTS(I)
                  IF (SWTS(I).GT.0.0D0) THEN
                     RLDELY(I) = SLDELY(I) / SWTS(I)
                     ARMS = SSDELY(I) / SWTS(I) - (SLDELY(I)/SWTS(I))**2
                     ARMS = SQRT (MAX (0.0D0, ARMS))
                     RPHASE(I) = ATAN2 (SPHASE(2,I), SPHASE(1,I))
                     WT = SQRT (1.0D0 / SWTS(I))
                     WRITE (MSGTXT,1081) I, RLDELY(I)*1.E9, WT,
     *                  ARMS*1.D9, RPHASE(I)*RAD2DG
                     CALL MSGWRT (4)
                  ELSE
                     RLDELY(I) = 0.0
                     RPHASE(I) = 0.0
                     END IF
 50               CONTINUE
               END IF
C                                       Write solution record.
            IF ((GOTD) .AND. (GOTS)) THEN
               IF ((NUMSCN.EQ.1) .AND. (APARM(2).LE.0.0)) THEN
                  MSGTXT = 'Average solution being applied to CL table'
                  CALL MSGWRT (4)
                  CALL RLDCLF (DISKIN, CNOIN, CATIN, SUBARR, RPHASE,
     *               RLDELY, CLIVER, CLOVER, BUFFER, IRET)
                  IF (IRET.GT.0) GO TO 990
                  END IF
               MSGTXT = 'Average solution being written to SN table'
               CALL MSGWRT (4)
               DTIME = (TIMSCN(1,ISCAN) + TIMSCN(2,ISCAN)) / 2.0D0
               TIMEI = TIMSCN(2,ISCAN) - TIMSCN(1,ISCAN)
               NPOL = 2
               CALL RLDSNF (DISKIN, CNOIN, CATIN, SUBARR, RPHASE,
     *            RLDELY, NANT, NPOL, EIF, SRCSCN(ISCAN), FRQSEL, DTIME,
     *            TIMEI, IREFAN, RWTS, SNOVER, BUFFER, IRET)
               IF (IRET.GT.0) GO TO 990
            ELSE
               IF (.NOT.GOTD) THEN
                  MSGTXT = 'NO DATA SELECTED THIS SCAN/SUBARRAY'
               ELSE
                  MSGTXT = 'NO VALID SOLUTIONS FOUND THIS SCAN/SUBARRAY'
                  END IF
               CALL MSGWRT (6)
               END IF
 90         CONTINUE
 100     CONTINUE
C                                       No data selected
      IF (.NOT.GOTDS) THEN
         IRET = 1
         MSGTXT = 'WARNING: NO DATA SELECTED'
         CALL MSGWRT (8)
         GO TO 990
         END IF
      IF (.NOT.GOTSS) THEN
         IRET = 1
         MSGTXT = 'WARNING: NO VALID SOLUTIONS FOUND'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Restore subarray
      SUBARR = IST
C                                       Write history.
      CALL RLDHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Start subarray',I4,' of',I4)
 1001 FORMAT ('Start REFANT',I4)
 1080 FORMAT (' IF  RL delay (ns) +- fe     rms    L phase (deg)')
 1081 FORMAT (I3,F12.4,2F10.4,F10.1)
      END
      SUBROUTINE RLDLIN (PRGN, MAXBL, MAXFRQ, MAXTIM, NUMSUB, IRET)
C-----------------------------------------------------------------------
C   RLDLIN gets input parameters for RLDLY and creates an output file
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
C   See prologue comments in RLDLY for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6, STAT*4, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   MAXBL, MAXFRQ, MAXTIM, NUMSUB, IRET
      INTEGER   IERR, NPARM, I, MXFLD, IROUND, LUN1, I4TEMP, ANVER,
     *   LIM1, LIM2, J
      LOGICAL   T, TABLE, EXIST, FITASC, MATCH, WASERR, COMPRS
      REAL      CATR(256), T1, T2
      DOUBLE PRECISION CATD(128)
      INCLUDE 'RLDLY.INC'
      REAL      TAU(MXBASE), TAUMIN, TAUMAX
      INTEGER   IBLAVG(MXBASE), NBLAVG
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
      NPARM = 235
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
      DISKIN = IROUND (XDI)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 20      CONTINUE
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
      IF (SINGLE) APARM(2) = 1.0
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
            MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - ' //
     *         'CHECK INPUTS'
            IRET = 1
            GO TO 990
            END IF
         IF (IRET.GT.0) GO TO 999
         END IF
C                                       General parms
      MXPABL = XUVRA(2)
      IF (MXPABL.LE.1.0E-20) MXPABL = 1.0E15
      MNPABL = XUVRA(1)
      WTPABL = XWTUV
C                                       Def. min. no. of antennas = 3
      MINNO = 3
      DOMODL = .TRUE.
      PRTLV = IROUND (XPRTL)
      SNRMIN = APARM(1)
      IF (SNRMIN.LT.0.5) SNRMIN = 5.0
      IF (APARM(3).LE.0.0) APARM(3) = 1.E6
      DOEVLA = IROUND (DOIFS)
      IF (JLOCIF.LT.0) DOEVLA = 0
      IF (DOEVLA.GT.0) THEN
         IF ((CATBLK(KINAX+JLOCIF)/DOEVLA)*DOEVLA.NE.
     *      CATBLK(KINAX+JLOCIF)) THEN
            WRITE (MSGTXT,1035) DOEVLA
            IRET = 1
            GO TO 990
            END IF
         END IF
C                                       BADDISK
      DO 70 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 70      CONTINUE
C                                       Check sort order, must be T*
      IF (ISORT(1:1).NE.'T') THEN
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
      DELWIN = 1.0D+9 / (ABS (CATR(KRCIC+JLOCF)) * CHINC)
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
      IF (NANT.LE.1) THEN
         MSGTXT = 'ENCOUNTERED PROBLEM DETERMINING NO. ANTENNAS'
         IRET = 8
         GO TO 990
         END IF
      REFANT = IROUND (XREFA)
C                                       Determine max. bl., time.
      NUMANT = NANT
C                                       Antennas
      DO 85 I = 1,50
         ANTENS(I) = IROUND (XANTS(I))
 85      CONTINUE
C                                       If any GSOLV entries are non-blank
C                                       solve only for those antennas.
C                                       excepting the reference antenna
      CALL LFILL (MAXANT, .TRUE., GDSOLV(2))
      GDSOLV(1) = .FALSE.
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
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
      TINTG = XSOLIN / 86400.0
C                                       Find minumum pre-average time
C                                       even if supplied so if
C                                       specified int. time > than
C                                       found int. time warning can
C                                       be given.
      T1= XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / 1440.0 +
     *   XTIME(4) / 86400.0
      T2 = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / 1440.0 +
     *   XTIME(8) / 86400.0
      IF (T2.LE.T1) T2 = 1000.
      SOLINT = T2 - T1
C                                       Read the uv-data to find
C                                       the pre-average times
      NBLAVG = 0
      DO 200 J = 1,NANT
         IF (J.NE.REFANT) THEN
            NBLAVG = NBLAVG + 1
            IBLAVG(NBLAVG) = 32768 * MIN (J, REFANT) + MAX (J, REFANT)
            END IF
200      CONTINUE
C
      CALL AVERT (T1, T2, IBLAVG, NBLAVG, BUFF1, TAU, TAUMIN, TAUMAX,
     *   IRET )
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
            MSGTXT = 'CANNOT DETERMINE INT. TIME FROM DATA; ' //
     *         'SET SOLINT'
            GO TO 990
            END IF
         TINTG = TAUMIN / 86400.0
         WRITE (MSGTXT,1200) TAUMIN
         CALL MSGWRT (4)
         XSOLIN = TAUMIN
      ELSE
         IF (TINTG.GT.TAUMIN/86400.0) THEN
            MSGTXT = 'WARNING: Input int. time (SOLINT) is greater '
     *         // 'than'
            CALL MSGWRT (6)
            MSGTXT = '         that found in the data.  Please be aware'
            CALL MSGWRT (6)
            MSGTXT = '         that this can cause odd errors.'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Correct SOLINT by TINTG/2
      SOLINT = SOLINT - TINTG * 0.5
      RATWIN = 1000.0 / (TINTG * 86400.0)
C                                       Set integration time for
C                                       rate smearing correction
C                                       in UVGET.
      IF (ILOCIT.GE.0) THEN
         DXTIME = 0.0
      ELSE
         DXTIME = TINTG
         END IF
C                                       UVGET has been called
C                                       find list of included scans
      CALL FNDSCN (NUMSCN, TIMSCN, SRCSCN, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'FNDSCN RETURNS ERROR'
         GO TO 990
         END IF
C
      NUMIF = 1
      IF (JLOCIF.GE.0) NUMIF = CATBLK(KINAX+JLOCIF)
      MAXBL = NANT
      MAXFRQ = NFREQ * NUMIF
C                                       Number of channels to process
      I4TEMP = ((ECHAN - BCHAN) / CHINC + 1) * NUMIF
      IF (MAXFRQ.GT.I4TEMP) MAXFRQ = I4TEMP
      I4TEMP = MAXBL * MAXFRQ
C                                       How big to make the buffers
      MAXTIM = 2 * SOLINT / TINTG + 1.5
      MAXTIM = MAX (2, MAXTIM)
      MAXTIM = 2
      IRET = 0
      IRET = 0
C                                       Stokes' - ask for what's needed
      STOKES = 'FULL'
      CLOVER = 0
      SNOVER = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RLDLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,' DISK=',I3,' USID=',I4)
 1035 FORMAT ('NUMBER IFS NOT INTEGER TIMES',I2,' QUITTING')
 1050 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('SORT ORDER IS ',A2,' NOT ',A2,' AS REQUIRED')
 1200 FORMAT ('Set integration time to',F10.6,
     *   ' seconds, hope that is ok')
      END
      SUBROUTINE FNDSCN (NUMSCN, TIMSCN, SRCSCN, IRET)
C-----------------------------------------------------------------------
C   Find included scans
C   Outputs:
C      NUMSCN   I        Number scans found
C      TIMSCN   R(2,*)   Time range of scans
C      SRCSCN   I(*)     Source number
C      IRET     I        Error
C-----------------------------------------------------------------------
      INTEGER   NUMSCN, SRCSCN(*), IRET
      REAL      TIMSCN(2,*)
C
      INTEGER   JERR, J, NXIDSO, NXSUBA
      LOGICAL   MATCH, TABLE, EXIST, FITASC
      INTEGER   I, NXSTA, NXEND, FREQID
      REAL      NXTIME, NXDTIM, TEPS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA TEPS /1.3E-7/
C-----------------------------------------------------------------------
C                                       Initialize output
      INXRNO = -1
      NUMSCN = 1
      TIMSCN(1,1) = TSTART
      TIMSCN(2,1) = TEND
      SRCSCN(1) = 0
C                                       See if NX file exists.
      CALL ISTAB ('NX', IUDISK, IUCNO, 1, IXLUN, NXBUFF, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((JERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST)) GO TO 800
C                                       Open NX table
      CALL NDXINI ('READ', NXBUFF, IUDISK, IUCNO, 1, CATUV, IXLUN,
     *   INXRNO, NXKOLS, NXNUMV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INDEX TABLE'
         GO TO 990
         END IF
C                                       Get number of scans
      NINDEX = NXBUFF(5)
C                                       Check if empty
      IF (NINDEX.LE.0) THEN
         CALL TABNDX ('CLOS', NXBUFF, INXRNO, NXKOLS, NXNUMV, NXTIME,
     *      NXDTIM, NXIDSO, NXSUBA, NXSTA, NXEND, FREQID, I)
         GO TO 800
         END IF
C                                       Locate first selected scan.
C                                       Loop through records
      NUMSCN = 0
      DO 60 I = 1,NINDEX
         INXRNO = I
C                                       Read record.
         CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV,
     *      NXTIME, NXDTIM, NXIDSO, NXSUBA, NXSTA, NXEND, FREQID,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING INDEX TABLE'
            GO TO 990
            END IF
C                                       Check time range.
         IF (TSTART.GT.(NXTIME+NXDTIM/2.0)) GO TO 60
         IF (TEND.LT.(NXTIME-NXDTIM/2.0)) GO TO 60
C                                       Check freq id
         IF ((FREQID.GT.0) .AND. (FREQID.NE.FRQSEL) .AND.
     *       (FRQSEL.GT.0)) GO TO 60
C                                       Check subarray
         IF ((SUBARR.GT.0) .AND. (NXSUBA.NE.SUBARR) .AND.
     *       (NXSUBA.GT.0)) GO TO 60
C                                       See if all sources selected.
         CURSOU = NXIDSO
         MATCH = .TRUE.
         IF (NSOUWD.GT.0) THEN
C                                       Search source lists
            MATCH = .FALSE.
            DO 40 J = 1,NSOUWD
               MATCH = NXIDSO .EQ. SOUWAN(J)
               IF (MATCH) GO TO 50
 40            CONTINUE
C                                       See if found match
 50         IF (((MATCH) .AND. (.NOT.DOSWNT)) .OR.
     *         ((.NOT.MATCH) .AND. (DOSWNT))) GO TO 60
            END IF
C                                       got one
         NUMSCN = NUMSCN + 1
         TIMSCN(1,NUMSCN) = NXTIME - NXDTIM/2.0 - TEPS
         TIMSCN(2,NUMSCN) = NXTIME + NXDTIM/2.0 + TEPS
         SRCSCN(NUMSCN) = NXIDSO
 60      CONTINUE
C                                       No valid data
      IF (NUMSCN.LE.0) THEN
         IRET = -1
         MSGTXT = 'INDXIN: NO DATA FOUND MEETING SELECTION CRITERIA'
         CALL MSGWRT (6)
         END IF
C                                       Close NX file
      CALL TABNDX ('CLOS', NXBUFF, INXRNO, NXKOLS, NXNUMV, NXTIME,
     *   NXDTIM, NXIDSO, NXSUBA, NXSTA, NXEND, FREQID, I)
      INXRNO = -1
      GO TO 999
C                                       No INDEX file
 800  MSGTXT = 'INDEX TABLE MISSING OR EMPTY: TIMERANGE DEFINES SCAN'
      CALL MSGWRT (7)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FNDSCN: ERROR',I3,' ON ',A)
      END
      SUBROUTINE RLDSEL (IRET)
C-----------------------------------------------------------------------
C   RLDSEL will read a multi source data set into a temporary scratch
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
      INTEGER   IRET, LUN1, LUN2, IIVER, OOVER
      REAL     DUM
      INCLUDE 'RLDLY.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      CHARACTER BNDCOD(MAXIF)*8
      DOUBLE PRECISION FOFF(MAXIF), FQBIF
      INTEGER NIF, I, CHIF, J, I1, I2
      DATA LUN1, LUN2 /28, 29/
C-----------------------------------------------------------------------
C                                       Setup
      CALL UVGET ('INIT', DUM, DUM, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NVIS.LE.0) GO TO 100
C                                       Message
      WRITE (MSGTXT,2000)
      IF (DOCAL) WRITE (MSGTXT,2001)
      IF (DOFLAG) WRITE (MSGTXT,2002)
      IF (DOCAL.AND.DOFLAG) WRITE (MSGTXT,2003)
      CALL MSGWRT (6)
      CLIVER = CLUSE
C                                       Copy
      VISDSK = 0
      VISCNO = 0
      CALL CALCOP (VISDSK, VISCNO, REFANT, BUFF1, JBUFSZ, IRET)
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
 100  CALL UVGET ('CLOS', DUM, DUM, IRET)
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
 2040 FORMAT ('RLDSEL: ERROR',I3,' READING OLD FQ TABLE')
 2050 FORMAT ('RLDSEL: ERROR',I3,' WRITING NEW FQ TABLE')
      END
      SUBROUTINE RLDHIS
C-----------------------------------------------------------------------
C   RLDHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12, HILINE*72
      INTEGER   LUN2, IERR, TIME(3), DATE(3)
      LOGICAL   T
      REAL      XSOL
      INCLUDE 'RLDLY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN2 /28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
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
C                                       calibration history
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Soln. interval.
      XSOL = SOLINT * 24.0 * 60.0
      IF (XSOL.GT.9999.99) XSOL = 9999.99
      WRITE (HILINE,2010) TSKNAM, XSOL
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Reference ant
      WRITE (HILINE,2012) TSKNAM, REFANT
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                        Bchan/Echan
      WRITE (HILINE,2017) TSKNAM, BCHAN, ECHAN, CHINC
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Output CL/SN tables
      WRITE (HILINE,2061) TSKNAM, CLIVER
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
      IF (CLOVER.GT.0) THEN
         WRITE (HILINE,2062) TSKNAM, CLOVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      IF (SNOVER.GT.0) THEN
         WRITE (HILINE,2063) TSKNAM, SNOVER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
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
C                                        Integration time.
      WRITE (HILINE,2043) TSKNAM, XSOLIN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       averaging
      IF (DOEVLA.GT.0) THEN
         WRITE (HILINE,2050) TSKNAM, DOEVLA
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,2051) TSKNAM, LBIF+1
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,2052) TSKNAM, LEIF+1
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       Close HI file
 190  CALL HICLOS (LUN2, T, BUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2010 FORMAT (A6,'SCANLENG= ',F7.2,' /Solution interval (min)')
 2012 FORMAT (A6,'REFANT = ',I4,' /Reference antenna')
 2017 FORMAT (A6,'BCHAN, ECHAN, CHINC =',2(I5,','),I4)
 2035 FORMAT (A6,'UVRANGE(1)=',1PE12.5,' /Min. bl. full weight')
 2036 FORMAT (A6,'UVRANGE(2)=',1PE12.5,' /Max. bl. full weight')
 2037 FORMAT (A6,'WTUV =',1PE12.5,' /Weight outside annulus')
 2043 FORMAT (A6,'SOLINT=',F5.2,' /Input integ. time (sec)')
 2050 FORMAT (A6,'DOIFS=',I2,6X,' /IFs combined in n groups')
 2051 FORMAT (A6,'BIF=',I3,6X,' / First IF when DOIFS=1')
 2052 FORMAT (A6,'EIF=',I3,6X,' / Last IF when DOIFS=1')
 2061 FORMAT (A6,'GAINUSE =',I4,' / Input CL table version')
 2062 FORMAT (A6,'OUTVERS =',I4,' / Output CL table version')
 2063 FORMAT (A6,'SNVER =',I4,' / Output SN table version')
      END
      SUBROUTINE RLDSOL (MAXBL, MAXTIM, MAXFRQ, IRET)
C-----------------------------------------------------------------------
C   RLDSOL calls RLDFIT.  Its only purpose is to declare various arrays
C   outside of the main routine, for the benefit of
C   machines that require overlaying.  See RLDFIT for details.
C   Output:
C      IRET     I   Error code: -1 no solutions
C-----------------------------------------------------------------------
      INTEGER   MAXBL, MAXFRQ, MAXTIM, IRET
C
      INTEGER   MAXIFS
      INCLUDE 'RLDLY.INC'
      INCLUDE 'INCS:DGDS.INC'
C                                       dynamic array declarations
      REAL      TIMB(2), VREAL(2), VIMAG(2), CMBDEL(2*MAXANT), CREAL(2),
     *   CIMAG(2), CDELY(2), CRATE(2), CWT(2), WTT(2), TIME(2)
      INTEGER   NWD, JERR, MCOR
      LONGINT   OVREAL, OVIMAG, OTIMB, OCREAL, OCIMAG, OCDELY,
     *   OCRATE, OCWT
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (CATBLK, CATD)
C-----------------------------------------------------------------------
      MAXIFS = 1
      IF (JLOCIF.GT.0) MAXIFS = CATBLK(KINAX+JLOCIF)
C                                       Number correlators
      MCOR = 1
      IF (NCOR.GE.2) MCOR = 2
C                                       allocate memory
      NWD = (MAXTIM + 2) * MAXFRQ * MAXBL * MCOR
      NWD = ((NWD - 1) / 256 + 1) * 256
      NWD = (NWD - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', 'RLDSOL', NWD, VREAL, OVREAL, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'RLDSOL', NWD, VIMAG, OVIMAG,
     *   IRET)
      NWD = (MAXTIM + 2) * MAXBL
      NWD = ((NWD - 1) / 256 + 1) * 256
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'RLDSOL', NWD, TIMB, OTIMB,
     *   IRET)
      NWD = 2 * MAXIFS * NANT
      NWD = ((NWD - 1) / 256 + 1) * 256
      NWD = (NWD - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'RLDSOL', NWD, CREAL, OCREAL,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'RLDSOL', NWD, CIMAG, OCIMAG,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'RLDSOL', NWD, CDELY, OCDELY,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'RLDSOL', NWD, CRATE, OCRATE,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'RLDSOL', NWD, CWT, OCWT,
     *   IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'RLDSOL: UNABLE TO GET NEEDED MEMORY, reduce SOLINT'
         CALL MSGWRT (8)
         MSGTXT = '        OR USE A BIGGER OR LESS BUSY COMPUTER'
         CALL MSGWRT (8)
      ELSE
         CALL RLDFIT (VREAL(1+OVREAL), VIMAG(1+OVIMAG), TIMB(1+OTIMB),
     *      CMBDEL, CREAL(1+OCREAL), CIMAG(1+OCIMAG), CDELY(1+OCDELY),
     *      CRATE(1+OCRATE), CWT(1+OCWT), WTT, TIME, MAXBL, MAXTIM,
     *      MAXFRQ, MAXIFS, MCOR, IRET)
         END IF
C                                       clear allocations
      CALL ZMEMRY ('FRAL', 'RLDSOL', NWD, VREAL, OVREAL, JERR)
C
 999  RETURN
      END
      SUBROUTINE RLDFIT (VREAL, VIMAG, TIMB, CMBDEL, CREAL, CIMAG,
     *   CDELY, CRATE, CWT, WTT, TIME, MAXBL, MAXTIM, MAXFRQ, MAXIFS,
     *   MCOR, IERR)
C-----------------------------------------------------------------------
C   RLDFIT reads thru a data file which has been divided by the model
C   and makes the requested solutions which are written into a solution
C   (SN) table.
C   Input:
C    VREAL(MAXTIM,MAXFRQ,MAXBL)     R    Work array.
C    VIMAG(MAXTIM,MAXFRQ,MAXBL)     R    Work array.
C    CMBDEL(2,NUMANT)               R    Work array
C    CREAL(2,MAXIFS,NUMANT)         R    Work array.
C    CIMAG(2,MAXIFS,NUMANT)         R    Work array.
C    CDELY(2,MAXIFS,NUMANT)         R    Work array.
C    CRATE(2,MAXIFS,NUMANT)         R    Work array.
C    CWT(2,MAXIFS,NUMANT)           R    Work array.
C    TIMB(MAXTIM,MAXBL)             R    Big array to hold times.
C      MAXBL    I      Max. number of baselines in data.
C      MAXTIM   I      Maximum number of time integrations.
C      MAXFRQ   I      Maximum number of frequency channels.
C   From common:
C    SOLINT        R    Solution interval (days).
C    TINTG         R    Integration time (sec)
C    DELWIN        R    Delay window (nsec)
C    REFANT        I    Ref ant to use.
C    DOIF          L    True then do "fringe fit" independently in each
C                       IF.
C    NUMBL         I    Number of baselines
C    NUMTIM        I    Number of time intervals
C    NUMIF         I    Number of IFs
C    SNRMIN        R    Minimum acceptable SNR
C    PRTLV         I    Print level
C    MINNO         I    Min. no. antennas.
C    CATBLK(256)   I    Output catalog header.
C    CATIN(256)    I    Input catalog header.
C    CNOIN         I    Input data cat. #.
C    CNOOUT        I    Output data cat #.
C    DISKIN        I    Input data disk number.
C    DISOUT        I    Output data disk number.
C    JBUFSZ        I    Buffer size.
C    BUFF1(*)      I    Work buffer
C    BUFF2(*)      I    Work buffer. Used for EQUIVALENCEs.
C   Output:
C    IERR          I    Return code, 0=>OK, otherwise error.
C                                    5=> solution interval too long
C-----------------------------------------------------------------------
      INTEGER   MAXBL, MAXFRQ, MAXTIM, MAXIFS, MCOR
      INTEGER   IERR, IRET
      REAL      VREAL(MAXTIM,MAXFRQ,MAXBL,MCOR),
     *   VIMAG(MAXTIM,MAXFRQ,MAXBL,MCOR), TIMB(MAXTIM,MAXBL),
     *   CMBDEL(2,*), CREAL(2,MAXIFS,*), CIMAG(2,MAXIFS,*),
     *   CDELY(2,MAXIFS,*), CRATE(2,MAXIFS,*), CWT(2,MAXIFS,*),
     *   WTT(MAXTIM), TIME(MAXTIM)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LUNI, FINDI, BINDI, J, IJK, NIN, NBL, NTIM, IC, IBASE,
     *   IITEMP, NBLANK, I, IBL, ICOR, DISK, INDEX, JBL, KK1, KK3, II,
     *   INCIII, I1, I2, IDAY, KDAY, KHR, KMN, KSEC, IROUND, SCNSOU,
     *   SCNSUB, SUBA, INTNO, NUMINT, NFPIF, NUMFRQ, INCJJJ, NOIF, MFRQ,
     *   MIF, ISUB, IM1, JFRQ, JIF, NXVER, FREQID, TVER, BO, VO, IDUM1,
     *   IDUM2, VCNO, CNTOK, KFRQ, KF, CNTBAD, TMPNIF, TMPNFQ, NDXSOU,
     *   ONESOU, NMSG, BLCODE(MAXANT), LWT, IS(MAXANT), JS(MAXANT),
     *   REFAN(2,MAXIF), ISU, BTCODE(MAXANT), NTIMES, DROUND, JCOR
      REAL      CATR(256), WTF(MAXCIF), WT, AMP, TIMEX, XINC, SCNTIM,
     *   CATIR(256), SCNDT, BASEL, MX2BAS, MN2BAS, BLFACT, IFRM,
     *   TAU(MXBASE), WTB(MAXANT,2), WGTMOD(MXBASE), WTFACT
      CHARACTER IFILE*48, BNDCOD(MAXIF)*8
      LOGICAL   T, F, JUSRED, WARN, DONDX, WANSRC, GOTANT(MAXANT),
     *   EXPAND, REFLOW
      DOUBLE PRECISION X8, TIMEC, FREQIF(MAXIF), CATD(128),
     *   FREQS(MAXCIF), LASTIM, TIMNOM, STTIME, CURTIM, CUREND,
     *   ENDTIM, DEPS, SIUSE, TINTGH, CURSUB, SCNEND, SCNBEG, SCNINT
      INCLUDE 'RLDLY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATIN, CATIR), (CATBLK, CATR, CATD)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNI /16/
      DATA BO, VO /1, 0/
C-----------------------------------------------------------------------
C                                       0.001 sec
      DEPS = 1.157407D-8
      LWT = XWTIT + 0.1
      RATWIN = 1000.0 / (TINTG * 86400.0)
C                                       Message(s)
      MSGTXT = 'Determining solutions'
      CALL MSGWRT (6)
      CNTOK = 0
      CNTBAD = 0
      NMSG = 0
      DO 20 IBL = 1,NUMANT
         DO 15 J = 1,MAXIFS
            DO 10 I = 1,2
               CREAL(I,J,IBL) = FBLANK
               CIMAG(I,J,IBL) = FBLANK
               CDELY(I,J,IBL) = FBLANK
               CRATE(I,J,IBL) = FBLANK
               CWT(I,J,IBL) = 0.0
 10            CONTINUE
 15         CONTINUE
 20      CONTINUE
C                                       Square baseline limits
      MX2BAS = MXPABL * MXPABL * 1.0E6
      MN2BAS = MNPABL * MNPABL * 1.0E6
      NOIF = EIF - BIF + 1
      TINTGH = TINTG * 0.5
C                                       Increment if averaging in
C                                       frequency
      INCIII = 1
      INCJJJ = (ECHAN - BCHAN) / CHINC + 1
      WARN = F
      NUMBL = (NUMANT * (NUMANT-1)) / 2
C                                       If only one source selected get
C                                       it's number
      IF (NSOUWD.EQ.1) THEN
         ONESOU = SOUWAN(1)
      ELSE
         ONESOU = 0
         END IF
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
C                                        Set baseline arrays.
      NBL = 0
      IBASE = 256
      IF (ILOCB.LT.0) IBASE = 32768
      DO 30 I1 = 1,NUMANT
         IF (I1.NE.REFANT) THEN
            NBL = NBL + 1
            IS(NBL) = MIN (I1, REFANT)
            JS(NBL) = MAX (I1, REFANT)
            BLCODE(NBL) = IS(NBL) * IBASE + JS(NBL)
            END IF
 30      CONTINUE
      DO 33 I1 = 1,NBL
         BTCODE(I1) = I1
 33      CONTINUE
C                                       Get IF frequency offsets.
      IXLUN = 29
      TVER = 1
      CALL CHNDAT ('READ', NXBUFF, DISK, VCNO, TVER, CATBLK, IXLUN, MIF,
     *   FREQIF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Fill frequency array for each
C                                       channel
      CALL FRQTAB (DISK, VCNO, IXLUN, CATBLK, FRQSEL, NXBUFF,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      NUMFRQ = 0
      FREQS(1) = 0.0D0
C
      TMPNIF = EIF - BIF + 1
      TMPNFQ = ECHAN - BCHAN + 1
C                                       Check number of frequencies.
      IF ((TMPNFQ.GT.MAXCHA) .OR. (TMPNIF.GT.MAXIF) .OR.
     *   (TMPNIF*TMPNFQ.GT.MAXCIF)) THEN
         IERR = 1
         MSGTXT = 'RLDFIT: VISIBILITIES TOO BIG FOR BUFFERS'
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
C                                       Check AP size
      CALL CHKAP (SOLINT * 86400.0, TINTG * 86400, NUMFRQ, NUMIF,
     *   FREQS, DOEVLA, RATWIN, DELWIN, IRET)
      IF (IRET.NE.0) GO TO 999
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
      NIN = 1
C                                       Clear "Got data" flags
         DO 85 KK1 = 1,NUMANT
            GOTANT(KK1) = F
 85         CONTINUE
C                                       Clear source id
         SCNSOU = ONESOU
C                                       Blank/zero solution values
         NBLANK = 2 * NUMIF * NUMANT
         CALL RFILL (2*NUMANT, 0.0, CMBDEL)
         CALL RFILL (NBLANK, FBLANK, CREAL)
         CALL RFILL (NBLANK, FBLANK, CIMAG)
         CALL RFILL (NBLANK, FBLANK, CDELY)
         CALL RFILL (NBLANK, FBLANK, CRATE)
         CALL RFILL (NBLANK, 0.0, CWT)
         NTIMES = 0
C                                        Init. for sol. interval.
C                                        Zero weights. etc
         CALL RFILL (MAXTIM, 0.0, WTT)
         KK1 = MAXANT * 2
         CALL RFILL (KK1, 0.0, WTB)
         KK1 = NUMBL
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
         NTIMES = 1
         STTIME = CURTIM
         CURSUB = SOLINT
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
            CURSUB = SIUSE
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
         ISUB = 1
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
            JBL = 32768 * I1 + I2
            END IF
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
 230     IF ((TAU(IBL).EQ.0.0) .AND. (ILOCIT.GE.0)) THEN
            TAU(IBL) = BUFF1(BINDI+ILOCIT)
            WGTMOD(IBL) = TAU(IBL) / (TINTG * 86400.0)
            END IF
C                                        Check if new time.
         IF ((CURTIM-TIMNOM).GT.(TINTGH-DEPS)) THEN
            IDAY = CURTIM
            X8 = (CURTIM - IDAY) / TINTG + 0.5D0
            TIMNOM = INT(X8) * TINTG + IDAY
C                                         Compute time increment.
            XINC = (TIMNOM - TIME(NTIM)) / TINTG
            NTIMES = NTIMES + 1
            TIME(NTIM) = TIME(NTIM) + TIMNOM
            END IF
C                                       Baseline factors
         I1 = IS(IBL)
         I2 = JS(IBL)
         REFLOW = I1.EQ.REFANT
         GOTANT(I1) = T
         GOTANT(I2) = T
         CUREND = CURTIM
         BLFACT = 1.0
         BASEL = BUFF1(BINDI+ILOCU) * BUFF1(BINDI+ILOCU) +
     *      BUFF1(BINDI+ILOCV) * BUFF1(BINDI+ILOCV)
         IF ((BASEL.LT.MN2BAS) .OR. (BASEL.GT.MX2BAS))
     *      BLFACT = BLFACT * WTPABL
C                                       Accumulate
         ENDTIM = CURTIM
         DO 260 ICOR = 1,MCOR
C                                       move LR/RL into 1 and 2
            IF (REFLOW) THEN
               JCOR = 5 - ICOR
C                                       move RL/LR into 1 and 2
            ELSE
               JCOR = 2 + ICOR
               END IF
            DO 255 I = 1,NUMFRQ
               IM1 = I - 1
               JFRQ = MOD (IM1, NFPIF)
               JIF = IM1 / NFPIF
               II = JIF * INCJJJ + JFRQ * INCIII + 1
               INDEX = BINDI + NRPARM + JFRQ * CHINC * INCF +
     *            JIF * INCIF + (JCOR-1) * INCS
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
                     WTB(IBL,ICOR) = WTB(IBL,ICOR) + WT
                     END IF
                  INDEX = INDEX + INCF
 254              CONTINUE
 255           CONTINUE
 260        CONTINUE
         TIMB(NTIM,IBL) = CURTIM
C                                       Loop back for next visibility
         GO TO 200
C                                       End of solution interval.
 300  JUSRED = T
C                                       Do solution.
C                                       Adjust time to center.
C                                       Center time defined by first
C                                       poln.
      TIMEC = (STTIME + ENDTIM) * 0.5
      TIME(1) = TIME(1) / MAX (1, NTIMES)
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
C                                       double up to foil rate sol'n
      NUMTIM = 2
      EXPAND = .TRUE.
      TIME(2) = 10.D0 * DEPS
      TIME(1) = -10.D0 * DEPS
      DO 340 IBL = 1,NBL
         TIMB(2,IBL) = 10.0D0*DEPS
         TIMB(1,IBL) = -10.0D0*DEPS
         DO 330 ICOR = 1,MCOR
            DO 320 I = 1,NUMFRQ
               IM1 = I - 1
               JFRQ = MOD (IM1, NFPIF)
               JIF = IM1 / NFPIF
               II = JIF * INCJJJ + JFRQ * INCIII + 1
               VREAL(2,II,IBL,ICOR) = VREAL(1,II,IBL,ICOR)
               VIMAG(2,II,IBL,ICOR) = VIMAG(1,II,IBL,ICOR)
 320           CONTINUE
 330        CONTINUE
 340     CONTINUE
      WTT(2) = WTT(1)
C                                       Loop over Stokes', baseline
      NMSG = 0
      DO 400 IBL = 1,NBL
         IF (PRTLV.GT.0) THEN
            WRITE (MSGTXT,2001) KDAY, KHR, KMN, KSEC, IS(IBL), JS(IBL)
            CALL MSGWRT (2)
          ELSE IF ((PRTLV.EQ.0) .AND. (NMSG.EQ.0)) THEN
            WRITE (MSGTXT,2000) KDAY, KHR, KMN, KSEC, REFANT
            CALL MSGWRT (2)
            NMSG = NMSG + 1
            END IF
         DO 390 ICOR = 1,MCOR
            IC = ICOR
            MFRQ = NUMFRQ
            MIF = NOIF
C                                       Find initial delay-rate solution
C                                       Use original method
            CALL RLDSRC (IS(IBL), JS(IBL), VREAL(1,1,IBL,ICOR),
     *         VIMAG(1,1,IBL,ICOR), TIME, FREQS, CREAL, CIMAG, CDELY,
     *         CRATE, CWT, REFAN, MAXFRQ, MAXTIM, MAXIFS, NUMANT,
     *         MFRQ, NUMTIM, NOIF, WTB(IBL,ICOR), WTT, WTF, DELWIN,
     *         RATWIN, REFANT, IC, SNRMIN, PRTLV, DOEVLA, FREQIF, IERR)
            IF (IERR.EQ.2) GO TO 999
            IF (IERR.NE.0) GO TO 400
C                                       Release AP if time
            IJK = 0
            CALL QROLL (0, NXBUFF, IJK, IERR)
C                                       Delay, rate, phase soln.
            CALL RLDDRP (IS(IBL), JS(IBL), VREAL(1,1,IBL,ICOR),
     *         VIMAG(1,1,IBL,ICOR), TIMB(1,IBL), FREQS, FREQIF, CMBDEL,
     *         CREAL, CIMAG, CDELY, CRATE, CWT, REFAN, MAXFRQ, MAXTIM,
     *         MAXIFS, NUMANT, MFRQ, NUMTIM, NOIF, DOEVLA,
     *         WTB(IBL,ICOR), WTT, WTF, DELWIN, RATWIN, IC, SNRMIN,
     *         PRTLV, RINWIN)
 390        CONTINUE
 400     CONTINUE
C                                       Close index file
      IF (DONDX) CALL TABIO ('CLOS', 0, INXRNO, TIMEC, NXBUFF, IERR)
C                                       get RLDLY solution
      CALL RLDFND (REFANT, NOIF, NUMANT, CREAL, CIMAG, CDELY, CWT,
     *   SNRMIN, GOTANT, CNTOK, CNTBAD, PRTLV, RPHASE, RLDELY, RRMS)
C                                       Release AP if have it.
      CALL QRLSE
C                                       Close solution files and
C                                       uv file.
      CALL ZCLOSE (LUNI, FINDI, IERR)
      IERR = 0
C                                       Give body count
      IF (CNTOK.LE.0) THEN
C                                       Nothing worked
         MSGTXT = 'ERROR: NO VALID SOLUTIONS FOUND'
         IERR = -1
         GO TO 990
      ELSE
         WRITE (MSGTXT,1440) CNTOK
         CALL MSGWRT (6)
         IF (CNTBAD.GT.0) THEN
            WRITE (MSGTXT,1441) CNTBAD
            CALL MSGWRT (6)
            END IF
         END IF
      GO TO 999
C                                        Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('RLDFIT: ERROR',I3,' OPENING INPUT UV FILE')
 1010 FORMAT ('RLDFIT: ERROR',I3,' INITING UV FILE')
 1100 FORMAT ('RLDFIT: ERROR',I3,1X,A4,'ING UV FILE')
 1130 FORMAT ('BAD BASELINE CODE=',I4,'-',I4,' NO. ANT.=',I4)
 1440 FORMAT ('Found ', I8, ' good solutions')
 1441 FORMAT ('Failed on ', I8, ' solutions')
 2000 FORMAT ('Time=',I4,'/',3I3.2,'  refant=',I3)
 2001 FORMAT ('Time=',I4,'/',3I3.2,'  baseline=',I3,' -',I3)
      END
      SUBROUTINE RLDFND (REFANT, NIF, NANT, CREAL, CIMAG, CDELY, CWT,
     *   SNRMIN, GOTANT, CNTOK, CNTBAD, PRTLV, RPHASE, RLDELY, RRMS)
C-----------------------------------------------------------------------
C   average robustly the answers from the various baselines
C   Inputs
C      NIF      I      Number IFs
C      NANT     I      Number antennas
C      CREAL    R(*)   Real (pol, if, antenna)
C      CIMAG    R(*)   Imag (pol, if, antenna)
C      CDELY    R(*)   Delay (pol, if, antenna)
C      CWT      R(*)   Weight (pol, if, antenna)
C      SNRMIN   R      Min SNR for "good"
C      GOTANT   L(*)   got any data for ant?
C   In/Out:
C      CNTOK    I      Counts up okay solutions
C      CNTBAD   I      Counts bad solutions
C   Outputs:
C      RPHASE   R(*)   Phase to apply to L (if)
C      RLDELY   R(*)   Delay to apply to L (if)
C-----------------------------------------------------------------------
      INTEGER   REFANT, NIF, NANT, CNTOK, CNTBAD, PRTLV
      REAL      CREAL(2,NIF,*), CIMAG(2,NIF,*), CDELY(2,NIF,*),
     *   CWT(2,NIF,*), SNRMIN, RPHASE(*), RLDELY(*), RRMS(*)
      LOGICAL   GOTANT(*)
C
      INTEGER   JIF, JA, NS, L, NT
C      REAL      V, W, SUM, SUMS, SUMW, AVG, RMS, WS(7), VMIN, VMAX, SRE,
C     *   SIM, PH1, PH2, P, DDELY(2,16,30)
      DOUBLE PRECISION
     *   V, W, SUM, SUMS, SUMW, AVG, RMS, WS(7), VMIN, VMAX, SRE,
     *   SIM, PH1, PH2, P
      REAL    DDELY(30,2,16), DPHAS(30,2,16), DREAL(30,2,16),
     *   DIMAG(30,2,16)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA WS /6.0, 5.0, 4.3, 3.6, 3.3, 3.0, 3.0/
C-----------------------------------------------------------------------
      DO 300 JIF = 1,NIF
         DO 290 JA = 1,NANT
            DO 280 L = 1,2
               DDELY(JA,L,JIF) = CDELY(L,JIF,JA)
               DREAL(JA,L,JIF) = CREAL(L,JIF,JA)
               DIMAG(JA,L,JIF) = CIMAG(L,JIF,JA)
               DPHAS(JA,L,JIF) = RAD2DG * ATAN2 (CIMAG(L,JIF,JA),
     *            CREAL(L,JIF,JA)+1.E-20)
 280           CONTINUE
 290        CONTINUE
 300     CONTINUE
      DO 100 JIF = 1,NIF
         VMAX = 1.D6
         VMIN = -1.D6
         RMS = 0.0D0
         AVG = 0.0D0
         DO 20 L = 1,7
            SUM = 0.0D0
            SUMS = 0.0D0
            SUMW = 0.0D0
            NS = 0
            NT = 0
            SRE = 0.0D0
            SIM = 0.0D0
            DO 10 JA = 1,NANT
C                                       R polarization
               PH1 = 0.0D0
               IF (CREAL(1,1,JA).NE.FBLANK) PH1 = ATAN2 (CIMAG(1,1,JA),
     *            CREAL(1,1,JA)+1.E-20)
               V = CDELY(1,JIF,JA)
               W = CWT(1,JIF,JA)
               IF ((JA.NE.REFANT) .AND. (V.NE.FBLANK) .AND. (W.GT.0.0))
     *            THEN
                  NT = NT + 1
                  IF ((V.GE.VMIN) .AND. (V.LE.VMAX)) THEN
                     SUM = SUM + W * V
                     SUMS = SUMS + W * V * V
                     SUMW = SUMW + W
                     NS = NS + 1
                     IF (CREAL(1,JIF,JA).NE.FBLANK) THEN
                        P = ATAN2 (CIMAG(1,JIF,JA), 1.E-20+
     *                     CREAL(1,JIF,JA)) - PH1
                        SRE = SRE + COS (P)
                        SIM = SIM + SIN (P)
                        END IF
                     END IF
                  END IF
C                                       L polarization
               PH2 = 0.0D0
               IF (CREAL(2,1,JA).NE.FBLANK) PH2 = ATAN2 (CIMAG(2,1,JA),
     *            CREAL(2,1,JA)+1.E-20)
               V = CDELY(2,JIF,JA)
               IF (V.NE.FBLANK) V = -V
               W = CWT(2,JIF,JA)
               IF ((JA.NE.REFANT) .AND. (V.NE.FBLANK) .AND. (W.GT.0.0))
     *            THEN
                  NT = NT + 1
                  IF ((V.GE.VMIN) .AND. (V.LE.VMAX)) THEN
                     SUM = SUM + W * V
                     SUMS = SUMS + W * V * V
                     SUMW = SUMW + W
                     NS = NS + 1
                     IF (CREAL(2,JIF,JA).NE.FBLANK) THEN
                        P = PH2 - ATAN2 (CIMAG(2,JIF,JA), 1.E-20+
     *                     CREAL(2,JIF,JA))
                        SRE = SRE + COS (P)
                        SIM = SIM + SIN (P)
                        END IF
                     END IF
                  END IF
 10            CONTINUE
            IF (NS.GT.0) THEN
               AVG = SUM / SUMW
               RMS = SUMS / SUMW - AVG * AVG
               RMS = SQRT (MAX (0.0D0, RMS))
               VMAX = AVG + WS(L) * RMS
               VMIN = AVG - WS(L) * RMS
               END IF
 20         CONTINUE
         RPHASE(JIF) = -ATAN2 (SIM, SRE+1.E-20)
         RLDELY(JIF) = AVG
         AVG = AVG * 1.E9
         RMS = RMS * 1.E9
         RRMS(JIF) = RMS
         IF (PRTLV.GT.0) THEN
            WRITE (MSGTXT,1020) JIF, AVG, RMS, NS, NT
            CALL MSGWRT (4)
            END IF
         DO 30 JA = 1,NANT
            IF (GOTANT(JA)) THEN
               IF (CWT(1,JIF,JA).GE.SNRMIN) THEN
                  CNTOK = CNTOK + 1
               ELSE
                  CNTBAD = CNTBAD + 1
                  END IF
C                                       Second poln.
               IF (CWT(2,JIF,JA).GE.SNRMIN) THEN
                  CNTOK = CNTOK + 1
               ELSE
                  CNTBAD = CNTBAD + 1
                  END IF
               END IF
 30         CONTINUE
 100     CONTINUE
      IF (PRTLV.GT.0) THEN
         DO 110 JIF = 1,NIF
            WRITE (MSGTXT,1100) JIF, RPHASE(JIF)*RAD2DG
            CALL MSGWRT (4)
 110        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('RL delay IF',I3,F11.4,' +-',F9.4,' ns using',I3,' of',
     *   I3,' samples')
 1100 FORMAT ('L phase IF',I3,F8.2,' degrees')
      END
      SUBROUTINE RLDCLF (DISK, CNO, CATB, SUBARR, RPHASE, RLDELY, IVER,
     *   OVER, BUFFER, IERR)
C-----------------------------------------------------------------------
C   Copies the highest CL table into one higher subtracting a delay
C   offset from the L delays
C   Inputs:
C      DISK     I        Input volume number
C      CNOI     I        Input catalog number
C      CATB     I(256)   Input catalog header
C      SUBARR   I        Selected subarray (= 0 any)
C      RLDELY   R(*)     Delay correction (noif)
C      IVER     I        CL ver input
C   Input/Output:
C      BUFFER   I(*)     Work buffers (512,2)
C   Output:
C      OVER     I        CL ver output
C      IERR     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATB(256), SUBARR, IVER, OVER,
     *   BUFFER(512,2), IERR
      REAL      RPHASE(*), RLDELY(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ICLRNO, CLKOLS(MAXCLC), CLNUMV(MAXCLC), NUMANT, NUMPOL,
     *   NUMIF, OKOLS(MAXCLC), ONUMV(MAXCLC), NCLROW, I, SOURID, ANTNO,
     *   SUBA, FREQID, REFA(2,MAXIF), OCLRNO, NTERM, JIF, IPOL, LUNI,
     *   LUNO
      REAL      GMMOD, TIMEI, IFR, ATMOS, DATMOS, MBDELY(2), CLOCK(2),
     *   DCLOCK(2), DISP(2), DDISP(2), DOPOFF(MAXIF), CREAL(2,MAXIF),
     *   CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF),
     *   RPI(MAXIF), RPR(MAXIF), RE, IM
      DOUBLE PRECISION TIME, GEODLY(10)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUNI, LUNO /28, 29/
C-----------------------------------------------------------------------
      CALL FNDEXT ('CL', CATB, I)
      IF (IVER.LE.0) THEN
         MSGTXT = 'THERE ARE NO CL TABLES, CAN''T COPY ONE'
         IERR = 8
         GO TO 990
         END IF
      OVER = I + 1
C                                       Open CL file
      CALL CALINI ('READ', BUFFER(1,1), DISK, CNO, IVER, CATB, LUNI,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN INPUT CL TABLE'
         GO TO 990
         END IF
C                                       # rows in old table
      NCLROW = BUFFER(5,1)
C                                       Open up new CL table
      CALL CALINI ('WRIT', BUFFER(1,2), DISK, CNO, OVER, CATB, LUNO,
     *   OCLRNO, OKOLS, ONUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT CL TABLE'
         GO TO 990
         END IF
      DO 10 JIF = 1,NUMIF
         RPI(JIF) = SIN (RPHASE(JIF))
         RPR(JIF) = COS (RPHASE(JIF))
 10      CONTINUE
C                                       Loop and copy
      DO 100 I = 1,NCLROW
         CALL TABCAL ('READ', BUFFER, ICLRNO, CLKOLS, CLNUMV,
     *      NUMPOL, NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *      IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *      DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C                                       Error
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ CL RECORD'
            GO TO 990
            END IF
C                                       Suba selection
         IPOL = 2
         IF ((SUBA.LE.0) .OR. (SUBA.EQ.SUBARR)) THEN
            DO 90 JIF = 1,NUMIF
               IF (DELAY(IPOL,JIF).NE.FBLANK) DELAY(IPOL,JIF) =
     *            DELAY(IPOL,JIF) - RLDELY(JIF)
               IF (CREAL(IPOL,JIF).NE.FBLANK) THEN
                  RE = CREAL(IPOL,JIF)
                  IM = CIMAG(IPOL,JIF)
                  CREAL(IPOL,JIF) = RE * RPR(JIF) - IM * RPI(JIF)
                  CIMAG(IPOL,JIF) = IM * RPR(JIF) + RE * RPI(JIF)
                  END IF
 90            CONTINUE
            END IF
         CALL TABCAL ('WRIT', BUFFER(1,2), OCLRNO, OKOLS, ONUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA,IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE CL RECORD'
            GO TO 990
            END IF
 100     CONTINUE
      IERR = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ICLRNO, BUFFER(1,1), BUFFER(1,1), IERR)
      CALL TABIO ('CLOS', 0, OCLRNO, BUFFER(1,2), BUFFER(1,2), IERR)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         WRITE (MSGTXT,1100) 'Corrected CL', DISK, CNO, IVER, OVER
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RLDCLF: ERROR ',I3,' ON ',A)
 1100 FORMAT (A,' file from vol/cno',I3,I5,' vers',I4,' to',I4)
      END
      SUBROUTINE RLDSNF (DISK, CNO, CATB, SUBARR, RPHASE, RLDELY,
     *   NUMANT, NUMPOL, NUMIF, SOURID, FREQID, TIME, TIMEI, REFANT,
     *   WTS, OVER, BUFFER, IERR)
C-----------------------------------------------------------------------
C   Makes an SN table one higher than current max and fills it with
C   subtracting a delay offset from the L delays
C   Inputs:
C      DISK     I        Input volume number
C      CNOI     I        Input catalog number
C      CATB     I(256)   Input catalog header
C      SUBARR   I        Selected subarray (= 0 any)
C      NUMANT   I
C      NUMPO    I
C      NUMIF    I
C      SOURID   I
C      FREQID   I
C      TIME     D
C      TIMEI    R
C      REFANT   I
C      RLDELY   R(*)     Delay correction (noif)
C   Input/Output:
C      BUFFER   I(*)     Work buffers (512,2)
C   Output:
C      OVER     I        CL ver output
C      IERR     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATB(256), SUBARR, NUMANT, NUMPOL, NUMIF,
     *   OVER, SOURID, FREQID, REFANT, BUFFER(512), IERR
      REAL      RPHASE(*), RLDELY(*), TIMEI, WTS(*)
      DOUBLE PRECISION TIME
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC), I, ANTNO,
     *   REFA(2,MAXIF), JIF, LUNO, NUMNOD, NODENO, luntmp
      REAL      GMMOD, IFR, MBDELY(2), DDISP(2), CREAL(2,MAXIF),
     *   CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF),
     *   RANOD(25), DECNOD(25), DISP(2)
      LOGICAL   ISAPPL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IF (OVER.EQ.0) THEN
         CALL FNDEXT ('SN', CATB, I)
         OVER = I + 1
         NUMNOD = 0
         GMMOD = 1.0
         ISAPPL = .FALSE.
         END IF
      LUNO = LUNTMP (1)
C                                       Open up new SN table
      CALL SNINI ('WRIT', BUFFER, DISK, CNO, OVER, CATB, LUNO, SNRNO,
     *   SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD, RANOD,
     *   DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT SN TABLE'
         GO TO 990
         END IF
C                                       set values
      DO 30 JIF = 1,NUMIF
         CREAL(1,JIF) = 1.0
         CIMAG(1,JIF) = 0.0
         DELAY(1,JIF) = 0.0
         RATE(1,JIF) = 0.0
         CREAL(2,JIF) = COS (RPHASE(JIF))
         CIMAG(2,JIF) = SIN (RPHASE(JIF))
         DELAY(2,JIF) = - RLDELY(JIF)
         RATE(2,JIF) = 0.0
         REFA(1,JIF) = REFANT
         REFA(2,JIF) = REFANT
         WEIGHT(1,JIF) = WTS(JIF)
         WEIGHT(2,JIF) = WTS(JIF)
 30      CONTINUE
      IFR = 0.0
      NODENO = 0
      MBDELY(1) = 0.0
      DISP(1)   = 0.0
      DDISP(1)  = 0.0
      MBDELY(2) = 0.0
      DISP(2)   = 0.0
      DDISP(2)  = 0.0
      DO 100 ANTNO = 1,NUMANT
         CALL TABSN ('WRIT', BUFFER, SNRNO, SNKOLS, SNNUMV, NUMPOL,
     *      TIME, TIMEI, SOURID, ANTNO, SUBARR, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE SN RECORD'
            GO TO 990
            END IF
 100     CONTINUE
      CALL TABIO ('CLOS', 0, SNRNO, BUFFER, BUFFER, JIF)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RLDSNF: ERROR ',I3,' ON ',A)
      END
      SUBROUTINE RLDSRC (IS, JS, VREAL, VIMAG, TIME, FREQS, CREAL,
     *   CIMAG, CDELY, CRATE, CWT, REFAN, MAXFRQ, MAXTIM, MAXIFS,
     *   NUMANT, NUMFRQ, NUMTIM, NUMIF, WTB, WTT, WTF, DELWIN, RATWIN,
     *   REFANT, IC, SNRMIN, PRTLV, DOEVLA, FREQIF, IERR)
C-----------------------------------------------------------------------
C   RLDSRC makes an initial estimate of the delay, rate and phase
C   of a visibility array by the method of F. Schwab.
C   One set of values is determined from all IFs together and then
C   filled into all.  If the Stokes' parameters were averaged (IC=0)
C   then the common solutions are copied into both.
C      Currently assumes that all frequencies are spaced by multiples
C   of the minimum spacing.
C   special version for 1 baseline at a time
C   Input:
C    IS                    I    First ant. of baseline numbers
C    JS                    I    2nd ant. of baseline numbers
C    VREAL(MAXTIM,MAXFRQ)  R    Real part of visibility array
C    VIMAG(MAXTIM,MAXFRQ)  R    Imag part of visibility array
C    TIME(*)               R    Time wrt center
C    FREQS(*)              D    Frequency array
C    MAXTIM              I    Maximum number of time integrations.
C    MAXFRQ              I    Maximum number of frequency channels.
C    MAXIFS              I    Maximum number of IFs
C    NUMANT              I    Number of antennas
C    NUMFRQ              I    Number of frequencies
C    NUMTIM              I    Number of times
C    NUMIF               I    Number of IFs
C    WTT(NUMTIM)         R    Time weight array
C    WTF(NUMFRQ)         R    Frequency weight array
C    DELWIN              R    delay window, <0 => no search in delay
C    RATWIN              R    rate window, <0 => no search in rate
C    REFANT              I    Reference antenna to use if possible.
C    IC                  I    Stokes' number passed, 0 => averaged.
C                             1=R, 2=L, 3=I
C    SNRMIN              R    Minimum SNR allowed
C    PRTLV               I    Print level
C    DOIF                L    If true then solve each IF independently
C    FREQIF(*)           D    Reference frequency offset per IF (Hz)
C   Output:
C    WTB                 R    Baseline weight array, returned
C                                     normalized.
C    CREAL(2,NUMIF,NUMANT)       R    Real part of solution
C    CIMAG(2,NUMIF,NUMANT)       R    Imag part of solution
C    CDELY(2,NUMIF,NUMANT)       R    delays in seconds.
C    CRATE(2,NUMIF,NUMANT)       R    Rates in Hz.
C    CWT(2,NUMIF,NUMANT)         R    Weights = SNR
C    REFAN(2,NUMIF)      I    Reference antennas used
C    IERR                I    Return code, 0=>OK, 1 => all data bad,
C                             2=>insufficient memory
C-----------------------------------------------------------------------
      INTEGER   IS, JS, REFAN(2,*), MAXFRQ, MAXTIM, MAXIFS, NUMANT,
     *   NUMFRQ, NUMTIM, NUMIF, REFANT, IC, PRTLV, DOEVLA, IERR
      REAL      VREAL(MAXTIM,MAXFRQ), VIMAG(MAXTIM,MAXFRQ), TIME(*),
     *   CREAL(2,MAXIFS,*), CIMAG(2,MAXIFS,*), CDELY(2,MAXIFS,*),
     *   CRATE(2,MAXIFS,*), CWT(2,MAXIFS,*), WTB, WTT(*), WTF(*),
     *   DELWIN, RATWIN, SNRMIN
      DOUBLE PRECISION FREQS(*), FREQIF(*)
C
      INTEGER   IIF, IB, IST, REFA, ANT, NFRQ, NFPIF, IFP, NOIF, REF2,
     *   IREF, IT, NF, IA, I, IS1, IS2, N2M1, IFQ, GOODCT, ISI, JSI,
     *   TREFAN, NUMPAS, LIMF1, LIMF2, I4TEMP, NNT, NNF, MF, MT,
     *   ND, NR, FCOUNT, APIAD, APINTR, APFIN, NEED, KAP, AIF
      LOGICAL   TRUE, FALSE, REDO, ALLBAD
      REAL      AMP, CTR, CTI, CPR, CPI, DF, DT, SNRAT, XMAX, XMAX2, WT,
     *   SUMWT, TWOPI, FRATE, FDELAY, AVGWTI, SPCNS, SPCMH
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   BADANT(MAXANT)
      INTEGER   REFLST(MAXANT), NIF1, NIF2, NJ
      REAL      SWT(MAXANT), WTBT, WT1, SAWT(MAXANT)
      INCLUDE 'INCS:GAIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:PMAD.INC'
      DATA TRUE, FALSE /.TRUE.,.FALSE./
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
      FCOUNT = NFPIF
      IF (DOEVLA.LT.1) THEN
         FCOUNT = NUMFRQ / NUMIF
         NOIF = NUMIF
      ELSE
         FCOUNT = NUMFRQ / DOEVLA
         NOIF = DOEVLA
         END IF
      NFPIF = FCOUNT
      CALL CHKFRQ (FREQS, NUMFRQ, FCOUNT, IERR)
      IF (IERR.NE.0) GO TO 999
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
      DO 20 IIF = 1,NUMFRQ
         DO 10 IT = 1,NUMTIM
            AMP = SQRT (VREAL(IT,IIF)*VREAL(IT,IIF) +
     *         VIMAG(IT,IIF)*VIMAG(IT,IIF))
            IF (AMP.GT.1.0E-15) THEN
               VREAL(IT,IIF) = VREAL(IT,IIF) / AMP
               VIMAG(IT,IIF) = VIMAG(IT,IIF) / AMP
               END IF
 10         CONTINUE
 20      CONTINUE
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
         MSGTXT = 'DID NOT GET WHAT I NEED FOR MEMORY'
         CALL MSGWRT (8)
         MSGTXT = 'USE SETMAXAP TO RAISE THE LIMIT, OR'
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
C                                       RLDSR2 is not exceeded.
      IF (4*NUMTIM.GT.2*MAXIMG) THEN
         MSGTXT = 'RLDSRC: WORK BUFFER TOO SMALL FOR RLDSR2'
         CALL MSGWRT (8)
         MSGTXT = 'RLDSRC: USE A SHORTER SOLINT OR AVG. IN TIME'
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
      IF (WTB.GT.0.0) SUMWT = SUMWT + WTB
      AVGWTI = 0.0
      IF (SUMWT.GT.1.0E-10) AVGWTI = 1.0 / SUMWT
      IF (WTB.GT.0.0) WTB = WTB * AVGWTI
C                                       Loop over independent IFs
      CALL RFILL (NUMANT, 0.0, SAWT)
      DO 400 IIF = 1,NOIF
         IFP = (IIF-1) * NFPIF + 1
         AIF = 1
         IF (DOEVLA.GT.0) AIF = (IIF - 1) * (NUMIF / DOEVLA) + 1
C                                       Blank solution
         REFAN(IST,IIF) = 0
         DO 90 ANT = 1,NUMANT
            BADANT(ANT) = FALSE
            REFLST(ANT) = 0
 90         CONTINUE
         NUMPAS = 0
C                                       Copy baseline weights.
         WTBT = WTB
         WT1 = WTB
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
         IF (WT1.NE.0.) THEN
            ISI = IS
            JSI = JS
            DO 130 IFQ = LIMF1,LIMF2
               DO 120 IT = 1,NUMTIM
                  WT = WT1 * WTT(IT) * WTF(IFQ)
                  IF ((ABS (VREAL(IT,IFQ)) +
     *               ABS (VIMAG(IT,IFQ))).LT.1.0E-20) WT = 0.0
                  SWT(ISI) = SWT(ISI) + WT
                  SWT(JSI) = SWT(JSI) + WT
                  SAWT(ISI) = SAWT(ISI) + WT
                  SAWT(JSI) = SAWT(JSI) + WT
                  SUMWT = SUMWT + WT
 120              CONTINUE
 130           CONTINUE
            END IF
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
            CALL RLDSR2 (IFP, VREAL, VIMAG, TIME, FREQS, DT, DF, MAXFRQ,
     *         MAXTIM, NUMANT, NFRQ, NUMTIM, WTBT, WTT, WTF, IREF, ANT,
     *         FREQIF(AIF), MF, MT, NR, ND, APIAD, APINTR, APFIN,
     *         CREAL(IST,IIF,ANT), CIMAG(IST,IIF,ANT),
     *         CDELY(IST,IIF,ANT), CRATE(IST,IIF,ANT), CWT(IST,IIF,ANT))
C                                       Save refence ant. used.
            REFAN(IST,IIF) = IREF
            IF (CWT(IST,IIF,ANT).GE.SNRMIN) REFLST(ANT) = IREF
C                                       Tell results if requested
            IF ((PRTLV.GE.2) .AND. (CDELY(IST,IIF,ANT).NE.FBLANK) .AND.
     *         (CRATE(IST,IIF,ANT).NE.FBLANK)) THEN
               FDELAY = CDELY(IST,IIF,ANT) * 1.0E9
               FRATE = CRATE(IST,IIF,ANT) * 1000.0
               SNRAT = MIN (999.0, CWT(IST,IIF,ANT))
               WRITE (MSGTXT,2001,ERR=275) ANT, IREF, IIF, FRATE,
     *            FDELAY, SNRAT
 275           CALL MSGWRT (2)
               END IF
C                                        Check SNR.
            IF (CWT(IST,IIF,ANT).GE.SNRMIN) THEN
               GOODCT = GOODCT + 1
C                                        Bad antenna.
            ELSE
               REDO = NUMPAS.EQ.1
               IF (BADANT(ANT)) THEN
                  IF ((IS.EQ.ANT) .OR. (JS.EQ.ANT)) WT1 = 0.0
                  END IF
               BADANT(ANT) = TRUE
C                                       Blank solution
               CREAL(IST,IIF,ANT) = FBLANK
               CIMAG(IST,IIF,ANT) = FBLANK
               CRATE(IST,IIF,ANT) = FBLANK
               CDELY(IST,IIF,ANT) = FBLANK
               CWT(IST,IIF,ANT) = 0.0
               END IF
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
               IF ((JS.EQ.REFA) .OR. (IS.EQ.REFA)) WTBT = 0.0
               ALLBAD = (WTBT.LE.0.0)
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
               WT1 = WTBT
               END IF
C                                       Try again.
            GO TO 100
            END IF
C                                       End independent IF loop
 400     CONTINUE
C                                       Copy solns. to all IFs
      IF ((NUMIF.GT.1) .AND. (DOEVLA.GT.0)) THEN
         DO 420 NJ = DOEVLA,1,-1
            NIF1 = NUMIF/DOEVLA
            NIF2 = NJ * NIF1
            NIF1 = NIF2 - NIF1 + 1
            NIF1 = MAX (2, NIF1)
            DO 410 IIF = NIF1,NIF2
               DO 405 IA = 1,NUMANT
                  IF (SAWT(IA).GT.0.0) THEN
                     CREAL(IST,IIF,IA) = CREAL(IST,NJ,IA)
                     CIMAG(IST,IIF,IA) = CIMAG(IST,NJ,IA)
                     CDELY(IST,IIF,IA) = CDELY(IST,NJ,IA)
                     CRATE(IST,IIF,IA) = CRATE(IST,NJ,IA)
                     CWT(IST,IIF,IA) = CWT(IST,NJ,IA)
                     END IF
 405              CONTINUE
               REFAN(IST,IIF) = REFAN(IST,NJ)
 410           CONTINUE
 420        CONTINUE
         END IF
      GO TO 999
C                                       error
 900  MSGTXT = 'RLDSRC: DYNAMIC MEMORY AP FAILS'
      CALL MSGWRT (8)
      MSGTXT = 'RLDSRC: MEMORY TOO SMALL FOR SPECIFIED FFT SEARCH'
      CALL MSGWRT (8)
 910  MSGTXT = 'REDUCE DELAY AND/OR RATE WINDOW OR AVERAGE IN'
      CALL MSGWRT (8)
      MSGTXT = 'FREQUENCY OR USE A SHORTER SOLINT'
      CALL MSGWRT (8)
      IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('RLDSRC: MIN FFT IN FREQ REQUESTED',I8,' MAX ALLOWED',I7)
 1021 FORMAT ('RLDSRC: MIN FFT IN TIME REQUESTED',I8,' MAX ALLOWED',I7)
 1041 FORMAT (' Coarse search spacing:',E12.3,' ns;',E12.3,' mHz')
 2001 FORMAT (' Ant=',I3,' ref=',I3,' IF=', I3,' rate=',F6.1,
     *   ' delay=',F11.1,' SNR=',F6.1)
      END
      SUBROUTINE RLDSR2 (IFP, VREAL, VIMAG, TIME, FREQS, DT, DF, MAXFRQ,
     *   MAXTIM, NUMANT, NUMFRQ, NUMTIM, WTB, WTT, WTF, REFAN,
     *   ANT, FREQIF, MF, MT, NR, ND, APIAD, APINTR, APFIN, CREAL,
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
C      VREAL    R(MAXTIM,MAXFRQ)   Real part of visibility array
C      VIMAG    R(MAXTIM,MAXFRQ)   Imag part of visibility array
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
C                                  unequal integration times in the data.
C      WTB      R                  Baseline weight array
C      WTT      R(*)               Time weight array
C      WTF      R(*)               Frequency weight array
C      REFAN    I                  Reference antenna to use.
C      ANT      I                  Antenna for solutions.
C      FREQIF   D                  IF Reference frequency offset (Hz)
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
     *   MF, MT, NR, ND, APIAD, APINTR, APFIN
      REAL      VREAL(MAXTIM,MAXFRQ), VIMAG(MAXTIM,MAXFRQ), CREAL,
     *   CIMAG, CDELY, CRATE, CWT, TIME(*), DT, DF, WTB, WTT(*), WTF(*)
      DOUBLE PRECISION FREQIF, FREQS(*)
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   JJ, II, J1, IR, ID, IIAD, I1, N2M1, JNF, JNT, I4TEMP,
     *   IAD, NT2, LIMF1, LIMF2
      LOGICAL   USEAP, GOOD
      REAL      W, SUMW, SUMWW, FAZ, TWOPI, XCOUNT, WORK(MAXIMG), S1,
     *   VR1
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
         I4TEMP = ((FREQS(IFP+NUMFRQ-1) - FREQS(IFP)) / DF) + 1.5
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
         IF (WTB.LE.0.0) GO TO 110
         DO 100 II = 1,NUMTIM
            W = WTB * WTT(II) * WTF(JJ)
            GOOD = (ABS (VREAL(II,JJ)) + ABS (VIMAG(II,JJ)))
     *        .GT.1.0E-20
            IF (.NOT.GOOD) W = 0.0
            SUMWW = SUMWW + W * W
            SUMW = SUMW + W
            IF (GOOD) XCOUNT = XCOUNT + 1.0
            WORK(II*2-1) = WORK(II*2-1) + W * VREAL(II,JJ)
            WORK(II*2) = WORK(II*2) + W * VIMAG(II,JJ) * S1
 100        CONTINUE
 110     CONTINUE
C                                       End of frequency loop
         NT2 = 2 * NUMTIM
C                                       Get correct frequency bin.
         I4TEMP = ((FREQS(JJ) - FREQS(IFP)) / DF) + 0.5
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
C
 999  RETURN
      END
      SUBROUTINE RLDDRP (IS, JS, VREAL, VIMAG, TIMB, FREQS, FREQIF,
     *   CMBDEL,  CREAL, CIMAG, CDELY, CRATE, CWT, REFAN, MAXFRQ,
     *   MAXTIM, MAXIFS, NUMANT, NUMFRQ, NUMTIM, NUMIF, DOEVLA,
     *   WTB, WTT, WTF, DELWIN, RATWIN, IC, SNRMIN, PRTLV, RINWIN)
C-----------------------------------------------------------------------
C   RLDDRP does least squares solutions for delay and rate
C   The input values of CREAL,CIMAG,CDELY, and CRATE are the initial
C   guess.
C   Input:
C    IS                     I    First ant. of baseline numbers
C    JS                     I    2nd ant. of baseline numbers
C    VREAL(MAXTIM,MAXFRQ)   R    Real part of visibility array
C    VIMAG(MAXTIM,MAXFRQ)   R    Imag part of visibility array
C    TIMB(MAXTIM)        R    Time wrt center
C    FREQS(*)            D    Frequency array
C    FREQIF(*)           D    Reference frequency offset per IF (Hz)
C    MAXTIM              I    Maximum number of time integrations.
C    MAXFRQ              I    Maximum number of frequency channels.
C    MAXIFS              I    Maximum number of IFs
C    NUMANT              I    Number of antennas
C    NUMFRQ              I    Number of frequencies
C    NUMTIM              I    Number of times
C    NUMIF               I    Number of IFS
C    DOIF                L    If true do each IF independently, else
C                             data averaged.
C    WTT(NUMTIM)         R    Time weight array
C    WTF(NUMFRQ)         R    Frequency weight array
C    WTB                 R    Baseline weight array
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
      INTEGER   MAXFRQ, MAXTIM, MAXIFS, IS, JS, NUMFRQ, NUMTIM,
     *   NUMANT, NUMIF, DOEVLA, IC, PRTLV, REFAN(2,*), INERT(3)
      REAL      VREAL(MAXTIM,MAXFRQ), VIMAG(MAXTIM,MAXFRQ),
     *   CMBDEL(2,*), CREAL(2,MAXIFS,*), CIMAG(2,MAXIFS,*),
     *   CDELY(2,MAXIFS,*), CRATE(2,MAXIFS,*), CWT(2,MAXIFS,*),
     *   TIMB(MAXTIM), WTB, WTT(*), WTF(*), DELWIN, RATWIN, SNRMIN,
     *   RINWIN
      DOUBLE PRECISION FREQIF(*), FREQS(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LDH, NA3, ITMAX, IANT, IST, IIF, NOIF, IFP, REFANT, I,
     *   I1, I2, I3, II, II1, II2, III, J, JJ, JJ1, JJ2, JJJ, K, KK,
     *   NFPIF, NFRQ, NANT, NANT2, NANT3, NIT, NA2, IT, INDX, ISI, JSI,
     *   ID2, IER, ILIM, LIMF1, LIMF2, JX, IFP1, AIF, NIF1, NIF2, NJ
      LOGICAL   FLGRAT, FLGDEL, KFATI, KFATJ
      REAL      TWOPI, XTR, XTI, PSTD(2), FREQX(MAXCIF), RATE, DELAY,
     *   SNR, X1R, X1I, X2R, X2I, X12R, X12I, X3R, X3I, PHAZ, PHASE
      DOUBLE PRECISION SL, S, WT, X1, X2, RNOBS, SUMWT, X3, CX, SX,
     *   G1, G2, G3, D11, D12, D13, D22, D23, D33, GN, W, TOL,
     *   SIGMA2, RMS, RCOND, DET(2)
      INCLUDE 'INCS:GAIN.INC'
      INTEGER   KANT(MAXANT), IKANT(MAXANT), KPVT(MAXPRM)
      REAL      SWT(MAXANT), PSWT(MAXANT), SAWT(MAXANT)
      DOUBLE PRECISION YPRM(MAXPRM,MAXIF)
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
      IF (DOEVLA.LE.0) THEN
         NOIF = NUMIF
         NFRQ = NUMFRQ / NUMIF
      ELSE
         NOIF = DOEVLA
         NFRQ = NUMFRQ / DOEVLA
         END IF
      NFPIF = NFRQ
C                                       Loop over independent IFs
      CALL RFILL (NUMANT, 0.0, SAWT)
      DO 800 IIF = 1,NOIF
         AIF = IIF
         IF (DOEVLA.GT.1) AIF = (IIF - 1) * (NUMIF / DOEVLA) + 1
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
         IF (WTB.GT.0.) THEN
            ISI = IS
            JSI = JS
            DO 30 J = LIMF1,LIMF2
C                                       Use freq offset in IF.
               JX = J - LIMF1 + 1
               FREQX(JX) = FREQS(J) - FREQS(LIMF1)
               IF (WTF(J).LE.0.0) GO TO 30
               DO 20 K = 1,NUMTIM
                  WT = WTB * WTF(J) * WTT(K)
                  IF ((ABS (VREAL(K,J)) + ABS (VIMAG(K,J))) .LE.
     *               1.0E-20) WT = 0.0
                  SWT(ISI) = SWT(ISI) + WT
                  SWT(JSI) = SWT(JSI) + WT
                  SAWT(ISI) = SAWT(ISI) + WT
                  SAWT(JSI) = SAWT(JSI) + WT
                  IF (WT.GT.1.0E-20) RNOBS = RNOBS + 1.0D0
                  SUMWT = SUMWT + WT
 20               CONTINUE
 30            CONTINUE
            END IF
         IF (IIF.EQ.1) CALL RCOPY (MAXANT, SWT, PSWT)
         NANT = 0
C                                       correction for DOEVLA
C                                       move data back to low IF
         IF (AIF.NE.IIF) THEN
            DO 5 IANT = 1,NUMANT
               IF (SAWT(IANT).GT.0.0) THEN
                  CREAL(IST,IIF,IANT) = CREAL(IST,AIF,IANT)
                  CIMAG(IST,IIF,IANT) = CIMAG(IST,AIF,IANT)
                  CDELY(IST,IIF,IANT) = CDELY(IST,AIF,IANT)
                  CRATE(IST,IIF,IANT) = CRATE(IST,AIF,IANT)
                  CWT(IST,IIF,IANT) = CWT(IST,AIF,IANT)
                  END IF
 5             CONTINUE
            REFAN(IST,IIF) = REFAN(IST,AIF)
            END IF
         REFANT = REFAN(IST,IIF)
C                                       Get lists of good antennas other
C                                       than the reference antenna.
         CALL FILL (MAXANT, -1, KANT)
         CALL FILL (MAXANT, -1, IKANT)
         DO 50 IANT = 1,NUMANT
            IF ((SWT(IANT).GT.1.0E-20) .AND.
     *         (CWT(IST,IIF,IANT).GT.0.0001)) THEN
C                                       if subset solve and nosolve for I
C                                       then IANT is neutral
               IF (IANT.EQ.REFANT) THEN
                  IKANT(IANT) = 0
C                                       otherwise IANT is good
               ELSE
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
               IF (SWT(IANT).GT.1.E-20) THEN
                  CREAL(IST,IIF,IANT) = FBLANK
                  CIMAG(IST,IIF,IANT) = FBLANK
                  CDELY(IST,IIF,IANT) = FBLANK
                  CRATE(IST,IIF,IANT) = FBLANK
                  CWT(IST,IIF,IANT) = 0.0
                  END IF
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
C                                       In case it doesn't converge.
               CREAL(IST,IIF,IANT) = FBLANK
               CIMAG(IST,IIF,IANT) = FBLANK
               CDELY(IST,IIF,IANT) = FBLANK
               CRATE(IST,IIF,IANT) = FBLANK
               CWT(IST,IIF,IANT) = 0.0
               END IF
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
     *         VREAL, VIMAG, TIMB, FREQX, MAXTIM, MAXFRQ, NUMANT, NFRQ,
     *         NUMTIM, WTB, WTT, WTF, PSTD, REFANT, PRTLV,SL, GWORK)
C                                       Accumulate (half) the gradient
C                                       of S and (half) the Hessian of
C                                       S:
C                                       Data valid?
            III = IS
            JJJ = JS
            II = IKANT(III)
            JJ = IKANT(JJJ)
C                                       if both ends are bad
C                                       then skip this baseline
            IF ((WTB.GT.0.0) .AND. (II.GE.0) .AND. (JJ.GE.0)) THEN
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
                     WT = WTB * WTF(J) * WTT(K)
C                                        Check for blanking.
                     IF ((ABS (VREAL(K,J)) + ABS (VIMAG(K,J)))
     *                  .LE.1.0E-20) WT = 0.0
                     X3 = (XPRM(III+NUMANT) - XPRM(JJJ+NUMANT)) *
     *                  TIMB(K)
                     X3R = COS (X3)
                     X3I = SIN (X3)
                     XTR = X3R*X12R - X3I*X12I
                     XTI = X3I*X12R + X3R*X12I
                     CX = WT * (VREAL(K,J) * XTR + VIMAG(K,J) * XTI)
                     SX = WT * (VIMAG(K,J) * XTR - VREAL(K,J) * XTI)
                     G1 = -SX
                     G2 = -TIMB(K) * SX
                     G3 = -FREQX(JX) * SX
                     D11 = CX
                     D12 = TIMB(K) * CX
                     D13 = FREQX(JX) * CX
                     D22 = TIMB(K) * D12
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
               END IF
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
     *            TIMB, FREQX, MAXTIM, MAXFRQ, NUMANT, NFRQ, NUMTIM,
     *            WTB, WTT, WTF, PSTD, REFANT, PRTLV, S, GWORK)
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
            IF (SWT(IANT).GT.0.0) THEN
               CREAL(IST,IIF,IANT) = COS (XPRM(IANT) - XPRM(REFANT))
               CIMAG(IST,IIF,IANT) = SIN (XPRM(IANT) - XPRM(REFANT))
               CDELY(IST,IIF,IANT) = ((XPRM(NANT2+IANT) -
     *            XPRM(NANT2+REFANT)) / TWOPI) * 1.0E-9
               CRATE(IST,IIF,IANT) = (XPRM(NUMANT+IANT) -
     *            XPRM(NUMANT+REFANT)) / (TWOPI * 86400.0)
C         IF (IANT.EQ.REFANT) CWT(IST,IIF,IANT) = SNRMIN + 1.0
               JJJ = IKANT(IANT)
               IF (JJJ.EQ.0) CWT(IST,IIF,IANT) = SNRMIN + 1.0
               IF ((JJJ.GT.0) .AND. (HESS(JJJ,JJJ).GT.0.0)) THEN
                  CWT(IST,IIF,IANT) = SQRT (SIGMA2 * HESS(JJJ,JJJ) *0.5)
                  IF (CWT(IST,IIF,IANT).GT.1.0E-20)
     *               CWT(IST,IIF,IANT) = 1.0 / CWT(IST,IIF,IANT)
                  END IF
C                                       Check min SNR
               IF (CWT(IST,IIF,IANT).LT.SNRMIN) THEN
                  CREAL(IST,IIF,IANT) = FBLANK
                  CIMAG(IST,IIF,IANT) = FBLANK
                  CDELY(IST,IIF,IANT) = FBLANK
                  CRATE(IST,IIF,IANT) = FBLANK
                  CWT(IST,IIF,IANT) = 0.0
                  END IF
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
C                                       End of IF loop
 790     IF (DOEVLA.GT.0) CALL DPCOPY (MAXPRM, XPRM, YPRM(1,IIF))
 800     CONTINUE
C                                       Copy solns. to all IFs
      IF ((NUMIF.GT.1) .AND. (DOEVLA.GT.0)) THEN
         DO 820 NJ = DOEVLA,1,-1
            NIF1 = NUMIF / DOEVLA
            NIF2 = NJ * NIF1
            NIF1 = NIF2 - NIF1 + 1
            DO 810 IIF = NIF1,NIF2
               DO 805 IANT = 1,NUMANT
                  IF (SAWT(IANT).GT.0.0) THEN
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
     *                  * (FREQIF(IIF) - FREQIF(NIF1))*1.0D-9)
                     CREAL(IST,IIF,IANT) = COS(PHASE)
                     CIMAG(IST,IIF,IANT) = SIN(PHASE)
                     CDELY(IST,IIF,IANT) = CDELY(IST,NJ,IANT)
                     CRATE(IST,IIF,IANT) = CRATE(IST,NJ,IANT)
                     CWT(IST,IIF,IANT) = CWT(IST,NJ,IANT)
                     END IF
 805              CONTINUE
               REFAN(IST,IIF) = REFAN(IST,NJ)
 810           CONTINUE
 820        CONTINUE
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
     *   FREQ, MAXTIM, MAXFRQ, NUMANT, NUMFRQ, NUMTIM, WTB, WTT, WTF,
     *   PSTD, REFANT, PRTLV, S, WORK)
C-----------------------------------------------------------------------
C   This routine takes a specified step in the model parameters used
C   by RLDDRP and compute the function being minimized, i.e. the chi-
C   squared sum plus penalty terms to constrain the delay and rate
C   solution.
C   Inputs:
C     IFP1           I    First frequency pointer
C     XPRM(NUMANT,3) D    Delay, rate and phase current model parms.
C                         In order, phase, rate, delay
C     W              D    Step size to take, fraction of STEP
C     STEP(NUMANT,3) D    Step for each of XPRM.
C     IS             I    First ant. of baseline numbers
C     JS             I    2nd ant. of baseline numbers
C     VREAL(MAXTIM,MAXFRQ)  R    Real part of visibility array
C     VIMAG(MAXTIM,MAXFRQ)  R    Imag part of visibility array
C     TIME(MAXTIM)          R    Time wrt center
C     FREQ(MAXFRQ)   R    Frequency array (only those in current IF)
C     MAXTIM         I    Maximum number of time integrations.
C     MAXFRQ         I    Maximum number of frequency channels.
C     NUMANT         I    Number of antennas
C     NUMFRQ         I    Number of frequencies
C     NUMTIM         I    Number of times
C     WTB            R    Baseline weight array
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
      INTEGER   IFP1, MAXTIM, MAXFRQ, IS, JS, NUMANT, NUMFRQ, NUMTIM,
     *   REFANT, PRTLV
      REAL      VREAL(MAXTIM,MAXFRQ), VIMAG(MAXTIM,MAXFRQ),
     *   TIME(MAXTIM), FREQ(MAXFRQ), WTB, WTT(MAXTIM),
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
      IF (WTB.GT.0.) THEN
         II = IS
         JJ = JS
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
               WT = WTB * WTF(J) * WTT(K)
               SUMWT = SUMWT + WT
C                                        Check for blanking.
               IF ((ABS (VREAL(K,J)) + ABS (VIMAG(K,J)))
     *           .LE.1.0E-20) WT = 0.0
               X3 = (WORK(II2) - WORK(JJ2)) * TIME(K)
               X3R = COS (X3)
               X3I = SIN (X3)
               XTR = X3R*X12R - X3I*X12I
               XTI = X3I*X12R + X3R*X12I
C                                       R = residual from model.
               R(1) = VREAL(K,J) - XTR
               R(2) = VIMAG(K,J) - XTI
               S1 = S1 + WT * (R(1)*R(1) + R(2)*R(2))
 20            CONTINUE
 30         CONTINUE
         END IF
C                                       Add constraints
      S2 = 0.0
      DO 60 I = 1,NUMANT
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
      SUBROUTINE CHKAP (SOLINT, INTTIM, NUMFRQ, NUMIF, FREQS, DOEVLA,
     *   RATWIN, DELWIN, IRET)
C-----------------------------------------------------------------------
C   adjsut parameters - AP checking now elsewhere
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
      IF (DOEVLA.LE.0) THEN
         FCOUNT = NUMFRQ / NUMIF
      ELSE
         FCOUNT = NUMFRQ / DOEVLA
         END IF
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
         ND = MF * DELWIN * DF
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
      DOUBLE PRECISION DPDF
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
 10         CONTINUE
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
      SUBROUTINE CALCOP (DISK, CNOSCR, REFANT, BUFFER, BUFSZ, IRET)
C-----------------------------------------------------------------------
C   Routine to copy selected data from one data file to another
C   optionally applying calibration and editing information.  The input
C   file should have been opened with UVGET.  Both files will be closed
C   on return from CALCOP.  Note: UVGET returns the information
C   necessary to catalog the output file.  The output file will be
C   compressed if necessary at completion of CALCOP.
C
C   Version to copy only baselines to REFANT
C
C   Inputs:
C      DISK     I       Disk number for cataloged output file.
C                       If .LE. 0 then the output file is a /CFILES/
C                       scratch file.
C      BUFSZ    I       Size of BUFFER in bytes.
C   Input via common:
C      LREC     I       (/UVHDR/) length of vis. record in R words.
C      NRPARM   I       (/UVHDR/) number of R random parameters.
C      REFANT   I       Copy only baselines with this antenna
C   In/out:
C      CNOSCR   I       Catalog slot number for if cataloged file;
C                       /CFILES/ scratch file number if a scratch file,
C                       IF DISK=CNOSCR=0 then the scratch is created.
C                       On output = Scratch file number if created.
C   In/out via common:
C      CATBLK   I(256)  Catalog header block from UVGET
C                       on output with actual no. records
C      NVIS     I       (/UVHDR/) Number of vis. records.
C   Output:
C      BUFFER   R(*)    Work buffer for writing.
C      IRET     I       Error code: 0 => OK,
C                          > 0 => failed, abort process.
C   Usage notes:
C   (1) UVGET with OPCODE='INIT' MUST be called before CALCOP to setup
C       for calibration, editing and data translation.  If an output
C       cataloged file is to be created this should be done after the
C       call to UVGET.
C   (2) Uses AIPS LUN 24
C-----------------------------------------------------------------------
      INTEGER   DISK, CNOSCR, REFANT, BUFSZ, IRET
      REAL      BUFFER(*)
C
      CHARACTER NAME*48
      INTEGER   VOL, LUN, FIND, BIND, LENBU, NIO, CATBLK(256), CNO, BO,
     *   VO, I, XCOUNT, ISIZE, ASIZE, IA1, IA2, IBASE
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN, BO, VO /24,1,0/
C-----------------------------------------------------------------------
      IRET = 0
      LENBU = 1
      XCOUNT = 0
C                                       Determine size.
      CALL UVSIZE (LREC, NVIS, ISIZE)
C                                       Create output file if necessary
      IF ((DISK.LE.0) .AND. (CNOSCR.LE.0)) THEN
C                                       Create scratch file.
         CALL SCREAT (ISIZE, BUFFER, IRET)
         CNOSCR = NSCR
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) THEN
               MSGTXT = 'CALCOP: TOO LITTLE DISK SPACE FOR SCRATCH FILE'
            ELSE
               WRITE (MSGTXT,1000) IRET, 'CREATING SCRATCH FILE'
               END IF
            GO TO 990
            END IF
C                                       Update CATBLK: ignore error
         CALL CATIO ('UPDT', SCRVOL(CNOSCR), SCRCNO(CNOSCR), CATBLK,
     *      'REST', BUFFER, IRET)
         END IF
C                                       Set output file name.
      IF (DISK.GT.0) THEN
         VOL = DISK
         CNO = CNOSCR
         CALL ZPHFIL ('UV', VOL, CNO, 1, NAME, IRET)
      ELSE
         VOL = SCRVOL(CNOSCR)
         CNO = SCRCNO(CNOSCR)
         CALL ZPHFIL ('SC', VOL, CNO, 1, NAME, IRET)
         END IF
C                                       Check file size
      CALL ZEXIST (VOL, NAME, ASIZE, IRET)
      IF (IRET.NE.0) THEN
         IF (IRET.EQ.1) THEN
            MSGTXT = 'CALCOP: FILE MISSING ' // NAME(:42)
         ELSE
            WRITE (MSGTXT,1000) IRET, 'SEEING IF FILE EXISTS'
            END IF
         GO TO 990
         END IF
      IF (ASIZE.LT.ISIZE) THEN
         MSGTXT = 'CALCOP: FILE TOO SMALL ' // NAME(:40)
         IRET = 1
         IF (DISK.GT.0) GO TO 990
         CALL MSGWRT (6)
         CALL ZDESTR (VOL, NAME, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'DESTROYING OLD SC FILE'
            GO TO 990
            END IF
         CALL ZCREAT (VOL, NAME, ISIZE, .TRUE., ASIZE, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CREATING NEW SC FILE'
            GO TO 990
            END IF
         MSGTXT = 'So - no worries - replaced it with a new SC file'
         CALL MSGWRT (6)
         END IF
C                                       Open output file.
      CALL ZOPEN (LUN, FIND, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT FILE'
         GO TO 990
         END IF
C                                       Init vis file for write
      CALL UVINIT ('WRIT', LUN, FIND, NVIS, VO, LREC, LENBU, BUFSZ,
     *   BUFFER, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT OUTPUT FILE'
         GO TO 990
         END IF
C                                       Copy file
      DO 100 I = 1,NVIS
C                                       Read old.
         CALL UVGET ('READ', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
         IF (IRET.LT.0) GO TO 110
         IF (IRET.NE.0) GO TO 999
C                                       Write new
         IF (ILOCB.GE.0) THEN
            IBASE = BUFFER(BIND+ILOCB) + 0.1
            IA1 = IBASE / 256
            IA2 = IBASE - 256*IA1
         ELSE
            IA1 = BUFFER(BIND+ILOCA1) + 0.1
            IA1 = BUFFER(BIND+ILOCA2) + 0.1
            END IF
         IF ((IA1.EQ.REFANT) .OR. (IA2.EQ.REFANT)) THEN
            XCOUNT = XCOUNT + 1
            NIO = 1
            CALL UVDISK ('WRIT', LUN, FIND, BUFFER, NIO, BIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT FILE'
               GO TO 990
               END IF
            END IF
 100     CONTINUE
C                                       Check if last call to UVGET
C                                       returned valid data.
 110  IF (IRET.LT.0) XCOUNT = XCOUNT - 1
C                                       Flush output
      NIO = 0
      CALL UVDISK ('FLSH', LUN, FIND, BUFFER, NIO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT FILE'
         GO TO 990
         END IF
C                                       Close input
      CALL UVGET ('CLOS', BUFFER(BIND), BUFFER(BIND+NRPARM), IRET)
C                                       Compress output file
      NVIS = XCOUNT
      IF (NVIS.GT.0) CALL UCMPRS (NVIS, VOL, CNO, LUN, CATBLK, IRET)
C                                       Update CATBLK: ignore error
      CALL CATIO ('UPDT', VOL, CNO, CATBLK, 'REST', BUFFER, IRET)
C                                       Close output
      CALL ZCLOSE (LUN, FIND, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CALCOP: ERROR ',I5,1X,A)
      END

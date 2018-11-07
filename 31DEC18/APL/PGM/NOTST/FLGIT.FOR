LOCAL INCLUDE 'FLGIT.INC'
C                                       Local include for FLGIT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   SEQIN, CNOIN, DISKIN, JBUFSZ, DISKOU, FITWTS(MAXCIF),
     *   NBDCOR, INCSI, INCFI, INCIFI, MAXWIN, AVGWIN, CATBO(256),
     *   SEQOUT, CNOOUT, NNCNT(10), NIF, CHNSEL(3,20,MAXIF), NCHAN,
     *   FGVERI, FGVERO, NFGWRI
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC, XCLAOU(2),
     *   XNAMEO(3), XOPCOD
      CHARACTER NAMEIN*12, CLAIN*6, XCALCO*4, XSOUR(30)*16, NAMEOU*12,
     *   CLAOUT*6, OPCODE*4
      REAL      XSIN, XDISIN, XSOUT, XDISOU, XQUAL, XFGOUT, XTIME(8),
     *   XBAND, XFREQ, XFQID, XBCHAN, XECHAN, XBIF, XEIF, XSUBA, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH(3), APARM(10), BPARM(10), XCHNS(4,20), XORD, XBADD(10),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), FINC(MAXIF)
      LOGICAL   ISCOMP, SELIF, DOOUT
      INTEGER   NSOU, ISBAND(MAXIF), NORDER, NANT, NPOLN
      DOUBLE PRECISION TIME1, TIME2, FQOFF, FOFF(MAXIF)
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMEO, XCLAOU,
     *   XSOUT, XDISOU, XFGOUT, XXSOUR, XQUAL, XXCALC, XTIME, XBAND,
     *   XFREQ, XFQID, XBCHAN, XECHAN, XBIF, XEIF, XSUBA, XDOCAL, XGUSE,
     *   XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XOPCOD,
     *   APARM, BPARM, XCHNS, XORD, XBADD
      COMMON /INFO/ CATBO, TIME1, TIME2, FQOFF, FOFF, FINC, ISBAND,
     *   NSOU, SEQIN, DISKIN, CNOIN, INCSI, INCFI, INCIFI, ISCOMP,
     *   NBDCOR, SEQOUT, CNOOUT, DISKOU, SELIF, NNCNT, MAXWIN, AVGWIN,
     *   NORDER, NIF, NCHAN, NANT, NPOLN, CHNSEL, DOOUT, FGVERI, FGVERO,
     *   NFGWRI
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMEOU, CLAOUT, XCALCO, XSOUR,
     *   OPCODE
      COMMON /WEIGHT/ FITWTS
LOCAL END
      PROGRAM FLGIT
C-----------------------------------------------------------------------
C! Applies calibration and/or editing and flags multisource uv data.
C# task editing UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-2000, 2002-2012, 2015-2016
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
C   It reads a UV data set in time order applying calibration and
C   editing.  It then fits a baseline to selected channels, examines the
C   rms in those channels after the fit and flags channels with
C   excessive signals.  The flagged data are then written to a new file.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      SOURCES        SOURCS        Source list.
C      QUAL           SELQUA
C      CALCODE        XCALCO
C      TIMERANG       XTIME         Time range of the data to check.
C      SELBAND        SELBAN
C      SELFREQ        SELFRQ
C      FREQID         FRQSEL
C      BCHAN          BCHAN         First channel in output
C      ECHAN          ECHAN         Last channel in output.
C      BIF            BIF           First IF to copy. 0=>all.
C      EIF            EIF           Highest IF to copy. 0=> highest.
C      SUBARRAY       SUBARR        Subarray number to copy. 0=> all.
C      DOCALIB        DOCAL         If true (>0), calibrate the data.
C      GAINUSE        CLUSE         Version of the Cal. table to use.
C      FLAGVER        FGVER         Version of the FG table to use.
C      DOBAND         DOBAND        If true correct data for bandpass
C      BPVER          BPVER         The BP table to apply
C      FLUX           RMS           Max deviation for unity weight
C      BADDISK        IBAD          Disks to avoid for scratch files.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, I, MALL(2), MBAD(2), MCBAD(2), NWORDS, MCHAN(2)
      LONGINT   OFFALL, OFFBAD, OFCBAD, OFFCHN
C     INTEGER   MCHAN(4*MAXCIF), MALL(MAXANT,MAXANT,4*MAXIF),
C    *   MBAD(MAXANT,MAXANT,4*MAXIF), MCBAD(MAXANT,MAXANT,4*MAXIF)
      INCLUDE 'FLGIT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'FLGIT '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL FLGIIN (PRGM, IRET)
C                                       Allocate memory
      NWORDS = (NANT * NANT * NPOLN * NIF - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MALL, OFFALL,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MBAD, OFFBAD,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MCBAD, OFCBAD,
     *   IRET)
C                                       Zero info matrices
      IF (IRET.EQ.0) THEN
         NWORDS = 1024 * NWORDS
         CALL FILL (NWORDS, 0, MALL(1+OFFALL))
         CALL FILL (NWORDS, 0, MBAD(1+OFFBAD))
         CALL FILL (NWORDS, 0, MCBAD(1+OFCBAD))
         END IF
C                                       Zero channel info array
      NWORDS = (NPOLN * NCHAN * NIF) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MCHAN, OFFCHN,
     *   IRET)
      IF (IRET.EQ.0) CALL FILL (1024*NWORDS, 0, MCHAN(1+OFFCHN))
C                                       Loop over sources.
      IF (IRET.EQ.0) CALL FLGIUV (NANT, NPOLN, NCHAN, NIF,
     *   MALL(1+OFFALL), MBAD(1+OFFBAD), MCBAD(1+OFCBAD),
     *   MCHAN(1+OFFCHN), IRET)
C                                       report results
      IF (IRET.EQ.0) CALL REPORT (NANT, NPOLN, NCHAN, NIF,
     *   MALL(1+OFFALL), MBAD(1+OFFBAD), MCBAD(1+OFCBAD),
     *   MCHAN(1+OFFCHN))
C                                       Report deeds to History file
      IRET = MAX (IRET, 0)
      IF (IRET.EQ.0) CALL FLGIHI
C                                       Done: Close down files, etc.
      CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, MCBAD, OFCBAD, I)
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE FLGIIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   FLGIIN gets input parameters for FLGIT, finds input file and
C   prepares the list of sources.  All selection criteria except
C   for the source name are filled into the commons in C/DSEL.INC.
C   Inputs:  PRGN    C*6   Program name
C   Output:
C     JERR         I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      HOLLERITH CATH(256)
      CHARACTER STAT*4, UTYPE*2
      INTEGER   NPARM, IROUND, IERR, I, LUN, J, JJ
      REAL      CATR(256)
      LOGICAL   MATCH
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FLGIT.INC'
      INTEGER   NW(MAXIF), K, K1, K2, IOFF, NUMAN(513), NVER
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR, CATBLK, CATH)
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      CALL FILL (10, 0, NNCNT)
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 276
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
C
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SEQOUT = IROUND (XSOUT)
      DISKOU = IROUND (XDISOU)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMEO, NAMEOU)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      DOOUT = XFGOUT.LT.0.0
      NORDER = XORD + 0.1
      NORDER = MAX (0, MIN (1, NORDER))
C                                       Sources
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), SOURCS(I))
         XSOUR(I) = SOURCS(I)
 20      CONTINUE
C                                       Save the time range
      CALL RCOPY (8, XTIME, TIMRNG)
C                                       BADDISK
      DO 30 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 30      CONTINUE
C                                       Get CATBLK for input file.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       OK, get the header now
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         GO TO 990
         END IF
C                                       OK, file available
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
C                                       Save input header
      CALL RCOPY (256, CATBLK, CATUV)
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Get uv header pointers.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      FQOFF = 0.0D0
      NPOLN = CATBLK(KINAX+JLOCS)
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
C                                       Set BCHAN and ECHAN
      JJ = CATBLK(KINAX+JLOCF)
      BCHAN = XBCHAN + 0.1
      BCHAN = MAX (1, BCHAN)
      ECHAN = XECHAN + 0.1
      IF (ECHAN.LT.BCHAN) ECHAN = JJ
      ECHAN = MIN (ECHAN, JJ)
      NCHAN = ECHAN - BCHAN + 1
      BPARM(1) = MAX (0.0, BPARM(1))
      IF (BPARM(2).LT.0.5) BPARM(2) = JJ
C                                       Check wanted IFs
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (BIF.GT.EIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         EIF = 1
         END IF
      SELIF = (BIF.GT.1) .OR. (EIF.LT.CATBLK(KINAX+JLOCIF)) .OR.
     *   (FRQSEL.GT.0)
      NIF = EIF - BIF + 1
C                                       ICHANSEL
      I = NCHAN * NIF
      CALL FILL (I, 0, FITWTS)
      CALL FILL (MAXIF, 0, NW)
      I = 60 * MAXIF
      CALL FILL (I, 0, CHNSEL)
      DO 40 J = 1,20
         K = IROUND (XCHNS(2,J))
         IF (K.LE.0) GO TO 45
         K = IROUND (XCHNS(4,J))
         IF ((K.LE.0) .OR. (K.GT.NIF)) THEN
            K1 = BIF
            K2 = EIF
         ELSE
            K1 = K
            K2 = K
            END IF
         DO 35 K = K1,K2
            NW(K) = NW(K) + 1
            CHNSEL(1,NW(K),K) = MAX (0, IROUND (XCHNS(1,J)))
            CHNSEL(2,NW(K),K) = MAX (0, IROUND (XCHNS(2,J)))
            CHNSEL(3,NW(K),K) = MAX (1, IROUND (XCHNS(3,J)))
 35         CONTINUE
 40      CONTINUE
C                                       If no channel selection
C                                       use 1 - NCHAN
 45    DO 60 K = BIF,EIF
          IOFF = (K - BIF) * NCHAN + 1 - BCHAN
          IF (NW(K).LE.0) THEN
             NW(K) = 1
             CHNSEL(1,1,K) = BCHAN
             CHNSEL(2,1,K) = ECHAN
             CHNSEL(3,1,K) = 1
             END IF
          DO 55 I = 1,NW(K)
             CHNSEL(1,I,K) = MAX (BCHAN, MIN (CHNSEL(1,I,K), ECHAN))
             IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K)) CHNSEL(2,I,K) = ECHAN
             CHNSEL(2,I,K) = MAX (BCHAN, MIN (CHNSEL(2,I,K), ECHAN))
             DO 50 J = CHNSEL(1,I,K),CHNSEL(2,I,K),CHNSEL(3,I,K)
                FITWTS(J+IOFF) = 1
 50            CONTINUE
 55         CONTINUE
 60      CONTINUE
C                                       Write weights to message file
      DO 70 K = BIF,EIF
         IOFF = (K-BIF)*NCHAN
         DO 65 I = BCHAN,ECHAN,20
            K1 = I + IOFF - BCHAN + 1
            K2 = MIN (I+19, ECHAN) - BCHAN + 1 + IOFF
            WRITE (MSGTXT,1060) K, I, (FITWTS(J), J = K1,K2)
            CALL MSGWRT (4)
 65         CONTINUE
 70      CONTINUE
C                                       Setup calibration choices
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      BLVER = IROUND (XBLVER)
      CALL FILL (50, 0, ANTENS)
      SUBARR = IROUND (XSUBA)
C                                       set flag versions
      CALL FNDEXT ('FG', CATBLK, I)
      FGVER = IROUND (XFLAG)
      IF ((FGVER.LT.0) .OR. (FGVER.GT.I)) FGVER = -1
      IF (FGVER.EQ.0) FGVER = I
      IF (DOOUT) THEN
         FGVERO = -1
         FGVERI = -1
      ELSE
         FGVERO = IROUND (XFGOUT)
         IF ((FGVERO.EQ.0) .OR. (FGVERO.GT.I)) FGVERO = I + 1
         FGVERI = FGVER
         IF (FGVERO.LE.I) FGVERI = -ABS (FGVERI)
         END IF
      NFGWRI = 0
C                                       cal parameters
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Do only cross-correlations
      DOACOR = .FALSE.
      DOXCOR = .TRUE.
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'No match to SELBAND/SELFREQ adverbs - check inputs'
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
C                                       Choice of qualifiers and calcode
      SELQUA = IROUND (XQUAL)
      SELCOD = XCALCO
C                                       Get source list
      IUDISK = FVOL(1)
      IUCNO = FCNO(1)
      IXLUN = 28
      CALL SOUFIL (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Max antenna number
      NANT = MAXANT
      CALL FNDEXT ('AN', CATBLK, NVER)
      IF (NVER.GT.0) THEN
         CALL GETNAN (DISKIN, CNOIN, CATBLK, LUN, BUFF1, NUMAN, IRET)
         IF ((NVER.GT.0) .AND. (IRET.EQ.0)) THEN
            JJ = NUMAN(1)
            NANT = 0
            DO 80 J = 1,JJ
               NANT = MAX (NANT, NUMAN(J+1))
 80            CONTINUE
            END IF
         END IF
C                                        Local parameters
      IF (APARM(1).LE.0.0) APARM(1) = 1.0E10
      IF (APARM(2).LE.0.0) APARM(2) = 1.0E10
      IF (APARM(3).LE.0.0) APARM(3) = 1.0E10
      IF (APARM(4).LE.0.0) APARM(4) = 6.0
      IF (APARM(5).LE.0.0) APARM(5) = 5.0
      MAXWIN = APARM(7) + 0.5
      IF (MAXWIN.LT.3) MAXWIN = 5
      AVGWIN = APARM(8) + 0.5
      AVGWIN = MAX (1, MIN (AVGWIN, MAXWIN-2))
      APARM(7) = MAXWIN
      APARM(8) = AVGWIN
      WRITE (MSGTXT,1100) APARM(1)
      CALL MSGWRT (4)
      WRITE (MSGTXT,1101) APARM(6)
      CALL MSGWRT (4)
      WRITE (MSGTXT,1102) APARM(2)
      CALL MSGWRT (4)
      WRITE (MSGTXT,1103) APARM(3)
      CALL MSGWRT (4)
      IF (OPCODE.NE.'MWFL') THEN
         WRITE (MSGTXT,1104) APARM(4)
         CALL MSGWRT (4)
         WRITE (MSGTXT,1105) APARM(5)
         CALL MSGWRT (4)
      ELSE
         WRITE (MSGTXT,1110) AVGWIN, MAXWIN
         CALL MSGWRT (4)
         END IF
      APARM(1) = APARM(1) * APARM(1)
      APARM(2) = APARM(2) * APARM(2)
      APARM(3) = APARM(3) * APARM(3)
      APARM(6) = APARM(6) * APARM(6)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLGIIN: error',I3,' obtaining input parameters')
 1030 FORMAT ('Error',I3,' finding ',A12,'.',A6,'.',I4,' disk =',
     *   I3,' user=',I5)
 1035 FORMAT ('Error',I3,' obtaining CATBLK ')
 1060 FORMAT ('Weights (',I3,'/',I5,') ',20I2)
 1100 FORMAT ('Clipping threshold for all channels = ',1PE12.3,' Jy')
 1101 FORMAT ('Clipping threshold for VPOL all chn = ',1PE12.3,' Jy')
 1102 FORMAT ('Flagging threshold for unit weight  = ',1PE12.3,' Jy')
 1103 FORMAT ('Flagging threshold for signal chans = ',1PE12.3,' Jy')
 1104 FORMAT ('Flagging residual flux              > ',F12.3,' *RMS')
 1105 FORMAT ('Flagging residual Real/Imaginary    > ',F12.3,' *RMS')
 1110 FORMAT ('Average',I3,' channels to form median in window of',I3,
     *   ' channels')
      END
      SUBROUTINE FLGIUV (NA, NP, NC, NI, MALL, MBAD, MCBAD, MCHAN, IRET)
C-----------------------------------------------------------------------
C   FLGIUV reads data through the calibration package, removes the
C   spectral baseline, flags bad channels, and writes out the data.
C   Inputs:
C      NA      I      Number of antennas
C      NP      I      Number of polarizations
C      NC      I      Number of channels
C      NI      I      Number of IFs
C   Output:
C      MALL    I(*)   Counts samples by baseline etc
C      MBAD    I(*)   Counts full bad spectra by baseline
C      MCBAD   I(*)   Counts bad channels by baseline
C      MCHAN   I(*)   Counts flags by channel
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NA, NP, NC, NI, MALL(NA,NA,NP,NI), MBAD(NA,NA,NP,NI),
     *   MCBAD(NA,NA,NP,NI), MCHAN(NC,NP,NI), IRET
C
      INCLUDE 'FLGIT.INC'
      CHARACTER PHNAME*48, CPARM*8, REASON*24, ATIME*8, ADATE*12
      HOLLERITH CATH(256)
      INTEGER   I, NRPRIN, VO, BO, INCX, NUMVIS, VCOUNT, KBIND, LUNO,
     *   LRECO, IA1, IA2, ILOCWT, INDO, OCOUNT, IPTRO, NCOPY,
     *   ILENBU, NIOUT, NIOLIM, ISUB, ISUB1,ISUB2, NXVER, NXLUN, NNIF,
     *   RNXRET, WASBAD(MAXCIF), LFGRNO, BUFFLG(512), FGKOLS(MAXFGC),
     *   FGNUMV(MAXFGC), LUN, DATE(3), TIME(3)
      REAL      RPARM(20), CATR(256), BASEN, UVWSC
      DOUBLE PRECISION  CATD(128)
      LOGICAL   T, F
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA LUNO, LUN /48, 49/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Set flagging reason
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      REASON = TSKNAM // ADATE // ' ' // ATIME(:5)
C                                       Initialize calibration
      NUMVIS = 0
      OCOUNT = 0
      CALL FNDEXT ('AN', CATBLK, ISUB)
      ISUB = MAX (1, ISUB)
      IF (ISUB.EQ.1) THEN
         ISUB1 = 1
         ISUB2 = 1
      ELSE IF ((SUBARR.LT.1) .OR. (SUBARR.GT.ISUB)) THEN
         ISUB1 = 1
         ISUB2 = ISUB
      ELSE
         ISUB1 = SUBARR
         ISUB2 = SUBARR
         END IF
      NXVER = 1
      NXLUN = 28
      CALL CHNDAT ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATUV, NXLUN,
     *   NNIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'PROBLEM FINDING FREQUENCIES'
         CALL MSGWRT (6)
         GO TO 999
         END IF
      DO 100 ISUB = ISUB1,ISUB2
         WRITE (MSGTXT,1010) ISUB
         CALL MSGWRT (2)
         SUBARR = ISUB
         CALL UVGET ('INIT', RPARM, BUFF1, IRET)
         IF (IRET.GT.0) GO TO 999
         UVWSC = FREQ / UVFREQ
C                                       Get values of data increments
C                                       from the calibration package
         IF (ISUB.EQ.ISUB1) THEN
            INCX = CATBLK(KINAX)
            INCSI = INCS / INCX
            INCFI = INCF / INCX
            INCIFI = INCIF / INCX
            NRPRIN = NRPARM
            END IF
C                                       create output file name
         IF ((ISUB.EQ.ISUB1) .AND. (DOOUT)) THEN
            CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMEOU, CLAOUT,
     *         SEQOUT)
            CALL CHR2H (12, NAMEOU, KHIMNO, CATH(KHIMN))
            CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
            CATBLK(KIIMS) = SEQOUT
C                                       fix header for compress
            IF (ISCOMP) THEN
               IA1 = 0
               IA2 = 0
               DO 10 I = 1,NRPARM
                  CALL H2CHR (8, 1, CATH(KHPTP+2*I-2), CPARM)
                  IF (CPARM.EQ.'REMOVED') THEN
                     IF (IA1.EQ.0) THEN
                        IA1 = I
                     ELSE IF (IA2.NE.IA1+1) THEN
                        IA2 = I
                        END IF
                     END IF
 10               CONTINUE
C                                       replace REMOVEDs
               IF (IA2.EQ.IA1+1) THEN
                  ILOCWT = IA1 - 1
C                                       Increase number
               ELSE
                  ILOCWT = NRPARM
                  CATBLK(KIPCN) = NRPARM + 2
                  END IF
               CALL CHR2H (8, 'WEIGHT  ', 1, CATH(KHPTP+2*ILOCWT))
               CALL CHR2H (8, 'SCALE   ', 1, CATH(KHPTP+2*ILOCWT+2))
               CATBLK(KINAX) = 1
               CALL UVPGET (IRET)
               END IF
C                                       no sort if >1 subarray
            IF (ISUB2.GT.ISUB1) CALL CHR2H (2, '  ', 1, CATH(KITYP))
C                                       Create output file.
            CNOOUT = 1
            FRW(NCFILE+1) = 3
            CALL UVCREA (DISKOU, CNOOUT, BUFF1, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'CREATE OUTPUT'
               GO TO 990
               END IF
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = DISKOU
            FCNO(NCFILE) = CNOOUT
            FRW(NCFILE) = FRW(NCFILE) - 1
            SEQOUT = CATBLK(KIIMS)
C                                       copy keywords
            CALL KEYCOP (DISKIN, CNOIN, DISKOU, CNOOUT, IRET)
C                                       Copy new CATBLK
            CALL COPY (256, CATBLK, CATBO)
            CALL ZPHFIL ('UV', DISKOU, CNOOUT, 1, PHNAME, IRET)
            CALL ZOPEN (LUNO, INDO, DISKOU, PHNAME, T, F, F, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT'
               GO TO 990
               END IF
C                                       Init vis file for write
            ILENBU = 0
            LRECO = LREC
            CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU,
     *         JBUFSZ,BUFF2, BO, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT OUTPUT'
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
            NIOLIM = ILENBU
C                                       Make sure there is data
            VCOUNT = 0
            NCOPY = LREC - NRPARM
C                                       Set lengths of input axes.
            NUMIF = 1
            IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
C                                       make an index table
            CALL RNXGET (DISKIN, CNOIN, CATUV)
            CALL RNXINI (DISKOU, CNOOUT, CATBLK, RNXRET)
            END IF
C                                       Get a record
 50      CONTINUE
            CALL UVGET ('READ', RPARM, BUFF1, IRET)
C                                       Escape loop if no more data
C                                       for this source (IRET = -1)
            IF (IRET.EQ.-1) GO TO 90
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ INPUT'
               GO TO 990
               END IF
            VCOUNT = VCOUNT + 1
C                                       Call FLAGIT now
            IF (ILOCB.GE.0) THEN
               BASEN = RPARM(1+ILOCB)
               IA1 = BASEN / 256. + 0.1
               IA2 = BASEN - IA1*256. + 0.1
            ELSE
               IA1 = RPARM(1+ILOCA1) + 0.1
               IA2 = RPARM(1+ILOCA2) + 0.1
               END IF
C                                      Working on visibility number
C                                      NUMVIS
            NUMVIS = NUMVIS + 1
C                                      Call filtering routine
            IF (OPCODE.EQ.'MWFL') THEN
               CALL FLAGMW (NUMVIS, IA1, IA2, NA, NP, NC, NI, MALL,
     *            MBAD, MCBAD, MCHAN, BUFF1, RPARM, WASBAD, IRET)
            ELSE
               CALL FLAGIT (NUMVIS, IA1, IA2, NA, NP, NC, NI, MALL,
     *            MBAD, MCBAD, MCHAN, BUFF1, RPARM, WASBAD, IRET)
               END IF
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'FILTER SPECTRUM'
               GO TO 990
            ELSE IF ((IRET.EQ.0) .AND. (DOOUT)) THEN
               OCOUNT = OCOUNT + 1
               RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVWSC
               RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVWSC
               RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVWSC
               CALL RCOPY (NRPRIN, RPARM, BUFF2(IPTRO))
C                                       update NX table
               CALL RNXUPD (RPARM, RNXRET)
C                                       Compressed
               IF (ISCOMP) THEN
                  CALL ZUVPAK (NCOPY, BUFF1, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPARM))
               ELSE
                  CALL RCOPY (NCOPY, BUFF1, BUFF2(IPTRO+NRPARM))
                  END IF
               IPTRO = IPTRO + LREC
               NIOUT = NIOUT + 1
C                                       Write vis record.
               IF (NIOUT.GE.NIOLIM) THEN
                  CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT'
                     GO TO 990
                     END IF
                  IPTRO = KBIND
                  NIOUT = 0
                  END IF
            ELSE
               IF (IRET.EQ.0) OCOUNT = OCOUNT + 1
               IF (.NOT.DOOUT) THEN
                  CALL MAKEFG (LUN, LFGRNO, FGKOLS, FGNUMV, IA1, IA2,
     *               ISUB, REASON, BUFF1, RPARM, WASBAD, BUFFLG, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITE FG TABLE'
                     GO TO 990
                     END IF
                  END IF
               END IF
            GO TO 50
C
 90      IF (DOOUT) THEN
            WRITE (MSGTXT,1110) VCOUNT, OCOUNT, 'written'
            CALL REFRMT (MSGTXT, '_', I)
            CALL MSGWRT (2)
         ELSE
            IRET = -99
            CALL MAKEFG (LUN, LFGRNO, FGKOLS, FGNUMV, IA1, IA2, ISUB,
     *         REASON, BUFF1, RPARM, WASBAD, BUFFLG, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'CLOSE FG TABLE'
               GO TO 990
               END IF
            WRITE (MSGTXT,1110) VCOUNT, OCOUNT, 'not completely flagged'
            CALL REFRMT (MSGTXT, '_', I)
            CALL MSGWRT (2)
            END IF
C                                       Close input file
         CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
C                                       Finish write
         IF (DOOUT) THEN
            NIOUT = - NIOUT
            CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'FLUSH OUTPUT'
               GO TO 990
               END IF
            END IF
 100     CONTINUE
C                                       Compress output file.
      IRET = 0
      IF (DOOUT) THEN
         NVIS = OCOUNT
         CALL UCMPRS (NVIS, DISKOU, CNOOUT, LUNO, CATBLK, IRET)
C                                       close NX table
         IRET = 0
         CALL RNXCLS (RNXRET)
         IF (RNXRET.NE.0) THEN
            MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
            GO TO 990
            END IF
         I = NNCNT(1) + NNCNT(2) + NNCNT(3) + NNCNT(4) + NNCNT(5) +
     *      NNCNT(6) + NNCNT(9) + NNCNT(10)
         IF (I.LE.0) THEN
            MSGTXT = 'NO NEW DATA FLAGS FOUNDP'
            CALL MSGWRT (7)
            IRET = -99
            END IF
      ELSE IF (NFGWRI.LE.0) THEN
         MSGTXT = 'NO NEW FLAGS FOUND: NO FG TABLE WRITTEN'
         CALL MSGWRT (7)
         IRET = -99
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLGIUV: ERROR',I5,' ON ',A,' FILE')
 1010 FORMAT ('Begin processing subarray',I4)
 1110 FORMAT (I10,' visibilities processed',I10,1X,A)
      END
      SUBROUTINE REPORT (NA, NP, NC, NI, MALL, MBAD, MCBAD, MCHAN)
C-----------------------------------------------------------------------
C   Print the reports
C   Inputs:
C      NA      I      Number of antennas
C      NP      I      Number of polarizations
C      NC      I      Number of channels
C      NI      I      Number of IFs
C      MALL    I(*)   Counts samples by baseline etc
C      MBAD    I(*)   Counts full bad spectra by baseline
C      MCBAD   I(*)   Counts bad channels by baseline
C      MCHAN   I(*)   Counts flags by channel
C-----------------------------------------------------------------------
      INTEGER   NA, NP, NC, NI, MALL(NA,NA,NP,NI), MBAD(NA,NA,NP,NI),
     *   MCBAD(NA,NA,NP,NI), MCHAN(NP,NC,NI)
C
      INTEGER   I, J, K, L, TOTALA, TOTALB, MMAX, MXA, I1, I2
      INCLUDE 'FLGIT.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   MAUX(MAXANT)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Report the news
      MSGTXT = 'Most of the following reports the sum of the incoming'
      CALL MSGWRT (2)
      MSGTXT = 'flags and those generated this time by FLGIT'
      CALL MSGWRT (2)
C                                       First add the totals
      DO 90 L = 1,NI
         DO 80 K = 1,NP
            TOTALA = 0
            TOTALB = 0
            MMAX = 0
            MXA = 0
            DO 20 I = 1,NA
               DO 10 J = 1,NA
                  IF (MALL(I,J,K,L).GT.0) THEN
                     MXA = MAX (MXA, I)
                     MXA = MAX (MXA, J)
                     TOTALA = TOTALA + MALL(I,J,K,L)
                     IF (MALL(I,J,K,L).GT.MMAX) MMAX = MALL(I,J,K,L)
                     END IF
                  TOTALB = TOTALB + MBAD(I,J,K,L)
 10               CONTINUE
 20            CONTINUE
C                                       Now report on all visibilities
            IF (TOTALA.GT.0) THEN
               I = L + BIF - 1
               WRITE (MSGTXT,1020) TOTALB, TOTALA, K, I
               CALL MSGWRT (4)
               WRITE(MSGTXT,1021) MMAX
               CALL MSGWRT (4)
               I1 = 1
 25            I2 = MIN (I1+27, MXA)
               IF (I2.GE.I1) THEN
                  WRITE (MSGTXT,1025) (J, J = I1,I2)
                  CALL MSGWRT (4)
                  DO 35 I= 1,MXA
                     DO 30 J= I1,I2
                        MAUX(J) = NINT ((10. * MALL(I,J,K,L)) / MMAX)
 30                     CONTINUE
                     WRITE (MSGTXT,1035) I, (MAUX(J), J = I1,I2)
                     CALL MSGWRT (4)
 35                  CONTINUE
                  I1 = I2 + 1
                  GO TO 25
                  END IF
C                                       Now report percentage flagged
               IF (TOTALB.GT.0) THEN
                  MSGTXT = 'Full spectra now flagged (percent):'
                  CALL MSGWRT (4)
                  I1 = 1
 40               I2 = MIN (I1+27, MXA)
                  IF (I2.GE.I1) THEN
                     WRITE (MSGTXT,1025) (J, J = I1,I2)
                     CALL MSGWRT (4)
                     DO 50 I = 1,MXA
                        DO 45 J = I1,I2
                           MAUX(J) = 0
                           IF (MALL(I,J,K,L).GT.0) THEN
                              MAUX(J) = NINT ((100. * MBAD(I,J,K,L)) /
     *                           MALL(I,J,K,L))
                           ELSE
                              MAUX(J) = 0
                              END IF
 45                        CONTINUE
                        WRITE (MSGTXT,1035) I, (MAUX(J), J = I1,I2)
                        CALL MSGWRT (4)
 50                     CONTINUE
                     I1 = I2 + 1
                     GO TO 40
                     END IF
                  END IF
C                                       Now report percentage flagged
               MSGTXT = 'Channels now flagged (percent):'
               CALL MSGWRT (4)
               I1 = 1
 55            I2 = MIN (I1+27, MXA)
               IF (I2.GE.I1) THEN
                  WRITE (MSGTXT,1025) (J, J = I1,I2)
                  CALL MSGWRT (4)
                  DO 65 I = 1,MXA
                     DO 60 J = I1,I2
                        MAUX(J) = 0
                        IF (MALL(I,J,K,L).GT.0) THEN
                           MAUX(J) = NINT ((100. * MCBAD(I,J,K,L)) /
     *                        (NCHAN * MALL(I,J,K,L)))
                        ELSE
                           MAUX(J) = 0
                           END IF
 60                     CONTINUE
                     WRITE (MSGTXT,1035) I, (MAUX(J), J = I1,I2)
                     CALL MSGWRT (4)
 65                  CONTINUE
                  I1 = I2 + 1
                  GO TO 55
                  END IF
C                                       Now report on channel triggers
               DO 70 I = 1,NCHAN
                  IF (MCHAN(I,K,L).GT.0) THEN
                     WRITE (MSGTXT,1060) I, MCHAN(I,K,L)
                     CALL MSGWRT (4)
                     END IF
 70               CONTINUE
               END IF
 80         CONTINUE
 90      CONTINUE
C                                       Report flagged correlators
      WRITE (MSGTXT,1090) NBDCOR
      CALL MSGWRT (4)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (I8,' of',I10,' full spectra are now flagged for corr,IF',
     *   I2,I3)
 1021 FORMAT ('Visibilities per baseline (tens of percent of',I9,'):')
 1025 FORMAT (7X,28(I2))
 1035 FORMAT ('Ant',I3,1X,28(I2))
 1060 FORMAT ('Channel',I5,':',I9,' new flags')
 1090 FORMAT (I10,' full spectra are now flagged')
      END
      SUBROUTINE FLGIHI
C-----------------------------------------------------------------------
C   FLGIHI appends to the history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, NNS, J, K
      REAL      FRAC
      INCLUDE 'FLGIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA LUN1, LUN2 /28, 29/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      IF (DOOUT) THEN
         CALL HISCOP (LUN1, LUN2, DISKIN, DISKOU, CNOIN, CNOOUT, CATBLK,
     *      BUFF1, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (6)
            GO TO 200
            END IF
C                                       FG table instead
      ELSE
         CALL HIOPEN (LUN2, DISKIN, CNOIN, BUFF2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (6)
            GO TO 200
            END IF
         END IF
C                                       cal adverbs
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Weights used
      DO 30 K = BIF,EIF
         DO 20 I = 1,20
            IF ((CHNSEL(1,I,K).GT.0) .AND.
     *         (CHNSEL(2,I,K).GE.CHNSEL(1,I,K))) THEN
               WRITE(HILINE,1120) TSKNAM, I, (CHNSEL(J,I,K), J = 1,3), K
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
               END IF
 20         CONTINUE
 30      CONTINUE
C                                       method
      IF (OPCODE.NE.'MWFL') THEN
         HILINE = TSKNAM // '/ linear baseline fit over above channels'
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,1124) TSKNAM, NORDER
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
      ELSE
         WRITE (HILINE,1125) TSKNAM, MAXWIN
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,1126) TSKNAM, AVGWIN
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       FG table written
      IF (.NOT.DOOUT) THEN
         WRITE (HILINE,1127) TSKNAM, FGVERO
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,1128) TSKNAM, NFGWRI
         MSGTXT = HILINE(7:)
         CALL MSGWRT (3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
C                                       fraction flagged
      NNS = NNCNT(9)
      NNCNT(6) = 2 * NNCNT(6)
      DO 125 I = 1,7
         NNS = NNS + NNCNT(I)
 125     CONTINUE
      NNCNT(8) = NNCNT(8) + NNS
      NNCNT(8) = MAX (1, NNCNT(8))
      FRAC = REAL (NNS) / REAL (NNCNT(8))
C                                       Threshold
      APARM(1) = SQRT (APARM(1))
      IF (APARM(1).LT.1.0E8) THEN
         WRITE (HILINE,1131) TSKNAM, NNCNT(1), APARM(1)
         MSGTXT = HILINE(9:)
         CALL MSGWRT (3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      APARM(6) = SQRT (APARM(6))
      IF (APARM(1).LT.1.0E8) THEN
         WRITE (HILINE,1132) TSKNAM, NNCNT(6), APARM(6)
         MSGTXT = HILINE(9:)
         CALL MSGWRT (3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      APARM(2) = SQRT (APARM(2))
      IF (APARM(2).LT.1.0E8) THEN
         WRITE (HILINE,1133) TSKNAM, NNCNT(2), APARM(2)
         MSGTXT = HILINE(9:)
         CALL MSGWRT (3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      APARM(3) = SQRT (APARM(3))
      IF (APARM(3).LT.1.0E8) THEN
         WRITE (HILINE,1134) TSKNAM, NNCNT(3), APARM(3)
         MSGTXT = HILINE(9:)
         CALL MSGWRT (3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      IF (OPCODE.NE.'MWFL') THEN
         WRITE (HILINE,1140) TSKNAM, NNCNT(4), APARM(4)
         MSGTXT = HILINE(9:)
         CALL MSGWRT (3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (HILINE,1141) TSKNAM, NNCNT(5), APARM(5)
         MSGTXT = HILINE(9:)
         CALL MSGWRT (3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      IF (APARM(9).GT.0.0) THEN
         WRITE (HILINE,1142) TSKNAM, NNCNT(9), APARM(9)
         MSGTXT = HILINE(9:)
         CALL MSGWRT (3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      WRITE (HILINE,1143) TSKNAM, NNCNT(7)
      MSGTXT = HILINE(9:)
      CALL MSGWRT (3)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
      IF (NNCNT(10).GT.0) THEN
         K = BPARM(2) + 0.1
         WRITE (HILINE,1145) TSKNAM, NNCNT(10), K
         MSGTXT = HILINE(9:)
         CALL MSGWRT (3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
         END IF
      WRITE (HILINE,1144) TSKNAM, FRAC
      MSGTXT = HILINE(9:)
      CALL MSGWRT (3)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                      Number of flagged correlators
      WRITE (HILINE,1146) TSKNAM, NBDCOR
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
C                                       Close HI file
 190  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                       Copy tables
 200  IF (DOOUT) CALL COPTAB (DISKIN, CNOIN, DISKOU, CNOOUT, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLGIHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1120 FORMAT (A6,'CHNSEL(',I2.2,') =',I5,':',I5,' BY',I3,
     *   '  / fit region IF=',I3)
 1124 FORMAT (A6,'ORDER =',I2,6X,'/ order of polynomial fit')
 1125 FORMAT (A6,'MAXWIN =',I5,'  / channels max in median window')
 1126 FORMAT (A6,'AVGWIN =',I5,'  / channels averaged about median')
 1127 FORMAT (A6,'OUTFGVER=',I4,'  / output flag table version')
 1128 FORMAT (A6,'NFGREC=',I10,'  / flag table records written')
 1131 FORMAT (A6,'/ Flagged',I10,
     *   ' channels for input flux > clip level',1PE10.3)
 1132 FORMAT (A6,'/ Flagged',I10,
     *   ' channels for VPOL flux > clip level ',1PE10.3)
 1133 FORMAT (A6,'/ Flagged',I10,
     *   ' channels for residual fit flux >    ',1PE10.3)
 1134 FORMAT (A6,'/ Flagged',I10,
     *   ' channels for residual signal flux > ',1PE10.3)
 1140 FORMAT (A6,'/ Flagged',I10,
     *   ' channels for residual flux >',F6.2,'*RMS(f)')
 1141 FORMAT (A6,'/ Flagged',I10,
     *   ' channels for residual Re/Im >',F6.2,'*RMS(R/I)')
 1142 FORMAT (A6,'/ Flagged',I10,
     *   ' channels for U baseline   >',F8.1,' lambda')
 1143 FORMAT (A6,'/ Flagged',I10,
     *   ' channels previously or in calibration')
 1144 FORMAT (A6,'/ ',F6.4,' of the channels are now flagged')
 1145 FORMAT (A6,'/ Flagged',I10,' full spectra because >=',I4,
     *   ' channels flagged')
 1146 FORMAT (A6,'/ Full spectra flagged:', I8)
      END
      SUBROUTINE FLAGIT (NUMVIS, IA1, IA2, NA, NP, NC, NI, MALL, MBAD,
     *   MCBAD, MCHAN, VIS, RPARM, WASBAD, IRET)
C-----------------------------------------------------------------------
C   Routine to fit straight line to chosen channels, subtract and check
C   residual values.  *** IT FITS TO REAL AND IMAGINARY COMPONENTS ***
C   Inputs:
C      NUMVIS   I        Visibility number
C      IA1      I        First antenna number
C      IA2      I        Second antenna number
C      NA       I        Number of antennas
C      NP       I        Number of polarizations
C      NC       I        Number of channels
C      NI       I        Number of IFs
C      VIS      R(3,*)   Visibilities in order real, imaginary, weight
C                        (Jy, Jy, unitless).  Weight <= 0 => flagged.
C      RPARM    R(*)     Random parameter array
C   Inputs from COMMON:
C      BCHAN    I        Lowest channel number.
C      ECHAN    I        Highest channel number.
C      INCSI    I        Input Stokes' increment in vis.
C      INCFI    I        Input frequency increment in vis.
C      INCIFI   I        Input IF increment in vis.
C      CATBLK   I        Catalog header block
C      FITWTS   I(*)     Weights for fitting
C   Output:
C      MALL     I(*)     Counts samples by baseline etc
C      MBAD     I(*)     Counts full bad spectra by baseline
C      MCBAD    I(*)     Counts bad channels by baseline
C      MCHAN    I(*)     Counts flags by channel
C      WASBAD   I(*)     =1 if bad on input, -1 if not, matches VIS
C      IRET     I        Return code   0 => OK
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2, NA, NP, NC, NI, MALL(NA,NA,NP,NI),
     *   MBAD(NA,NA,NP,NI), MCBAD(NA,NA,NP,NI), MCHAN(NC,NP,NI),
     *   WASBAD(*), IRET
      REAL      VIS(3,*), RPARM(*)
C
      INCLUDE 'FLGIT.INC'
      INTEGER   LOOP, INDEX, OFF, IS, IIF, FITNUM, FLOOP, NRMS, NGOOD,
     *   IOFF, BPARM1, LL1, LL2, LBAD, LL, LI
      REAL      RESULT(2), FITRE(MAXCHA), FITIM(MAXCHA), FITCHA(MAXCHA),
     *   AR, BR, AI, BI, AMPLIT, AVGR, AVGI, VR, VI, FMS, FMS2, RMS,
     *   RMS2, IMS, CATUVR(256)
      LOGICAL   GOODDT, WILFLG, PTEST
      DOUBLE PRECISION FZ, FI, FRQMUL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATUVR, CATUV)
C-----------------------------------------------------------------------
      BPARM1 = BPARM(1) + 0.1
C                                       Set up on first call
      IF (NUMVIS.EQ.1) THEN
         NBDCOR = 0
         CALL FILL (9, 0, NNCNT)
         END IF
C                                       process data
      IRET = -1
      IF (NUMVIS.GT.0) THEN
         IF (MOD(NUMVIS,10000).EQ.0) THEN
            WRITE (MSGTXT,1000) NUMVIS
            CALL MSGWRT (2)
            END IF
C                                       Loop over IF, Stokes
         DO 90 IIF = 1,NIF
            IOFF = (IIF - 1) * NCHAN
            FZ = FOFF(IIF) / UVFREQ + 1.0D0
            FI = FINC(IIF) / UVFREQ
            DO 80 IS = 1,NPOLN
               NGOOD = 0
               FLOOP = 0
               PTEST = (IS.EQ.1) .AND. (NPOLN.GT.1) .AND.
     *            (APARM(6).GT.0.0)
C                                       Offset in Stokes and IF
               OFF = (IS - 1) * INCSI + (IIF - 1) * INCIFI
C                                       Index for this channel, IF
               INDEX = OFF + 1
               GOODDT = .FALSE.
               DO 5 LOOP = 1,NCHAN
                  IF (VIS(3,INDEX).GT.0.0) THEN
                     WASBAD(INDEX) = -1
                     GOODDT = .TRUE.
                  ELSE
                     WASBAD(INDEX) = 1
                     NNCNT(7) = NNCNT(7) + 1
                     END IF
                  INDEX = INDEX + INCFI
 5                CONTINUE
               INDEX = OFF + 1
               DO 10 LOOP = 1,NCHAN
                  IF (VIS(3,INDEX).GT.0.0) THEN
                     FRQMUL = 1.0D0
                     IF (TYPUVD.LE.0) FRQMUL = FZ + FI *
     *                  (LOOP - 1 + BCHAN - CATUVR(KRCRP+KLOCFY))
                     VR = VIS(1,INDEX)
                     VI = VIS(2,INDEX)
                     AMPLIT = VR*VR + VI*VI
                     IF (ABS(RPARM(1+ILOCU)*FRQMUL).LT.APARM(9)) THEN
                        NNCNT(9) = NNCNT(9) + 1
                        VIS(3,INDEX) = -VIS(3,INDEX)
                        MCHAN(LOOP,IS,IIF) = MCHAN(LOOP,IS,IIF) + 1
                     ELSE IF (AMPLIT.GT.APARM(1)) THEN
                        NNCNT(1) = NNCNT(1) + 1
                        LL1 = MAX (1, LOOP-BPARM1)
                        LL2 = MIN (NCHAN, LOOP+BPARM1)
                        DO 8 LL = LL1,LL2
                           LI = INDEX + (LL-LOOP) * INCFI
                           VIS(3,LI) = -ABS (VIS(3,LI))
                           MCHAN(LL,IS,IIF) = MCHAN(LL,IS,IIF) + 1
 8                         CONTINUE
                     ELSE IF ((PTEST) .AND. (VIS(3,INDEX+INCSI).GT.0.0))
     *                  THEN
                        VR = VR - VIS(1,INDEX+INCSI)
                        VI = VI - VIS(2,INDEX+INCSI)
                        AMPLIT = VR*VR + VI*VI
                        IF (AMPLIT.GT.APARM(6)) THEN
                           NNCNT(6) = NNCNT(6) + 1
                           LL1 = MAX (1, LOOP-BPARM1)
                           LL2 = MIN (NCHAN, LOOP+BPARM1)
                           DO 9 LL = LL1,LL2
                              LI = INDEX + (LL-LOOP) * INCFI
                              VIS(3,LI) = -ABS (VIS(3,LI))
                              VIS(3,LI+INCSI) = -ABS (VIS(3,LI+INCSI))
                              MCHAN(LL,IS,IIF) = MCHAN(LL,IS,IIF) + 2
 9                            CONTINUE
                           END IF
                        END IF
                     END IF
                  INDEX = INDEX + INCFI
 10               CONTINUE
C                                       Get arrays for line fitting
 15            FITNUM = 0
               INDEX = OFF + 1
               DO 20 LOOP = 1,NCHAN
                  IF (VIS(3,INDEX).GT.0.0) THEN
                     IF (FITWTS(LOOP+IOFF).GT.0) THEN
                        FITNUM = FITNUM + 1
                        FITRE(FITNUM) = VIS(1,INDEX)
                        FITIM(FITNUM) = VIS(2,INDEX)
                        FITCHA(FITNUM) = LOOP
                        END IF
                     END IF
                  INDEX = INDEX + INCFI
 20               CONTINUE
C                                       Some data to fit
               FLOOP = FLOOP + 1
               IF (FITNUM.GT.0) THEN
                  CALL LINFIT (NORDER, FITCHA, FITRE, FITNUM, AR, BR)
                  CALL LINFIT (NORDER, FITCHA, FITIM, FITNUM, AI, BI)
C                                       Now subtract from data
                  IF (FLOOP.LT.5) THEN
                     INDEX = OFF + 1
                     RMS = 0.0
                     FMS = 0.0
                     IMS = 0.0
                     NRMS = 0
                     DO 30 LOOP = 1,NCHAN
                        IF ((VIS(3,INDEX).GT.0.0) .AND.
     *                     (FITWTS(LOOP+IOFF).GT.0)) THEN
C                                       Interpolated value (re, imag)
                           AVGR = AR + BR * LOOP
                           AVGI = AI + BI * LOOP
C                                       Subtract from vis.
                           RESULT(1) = (VIS(1,INDEX) - AVGR) ** 2
                           RESULT(2) = (VIS(2,INDEX) - AVGI) ** 2
                           RMS = RMS + RESULT(1)
                           IMS = IMS + RESULT(2)
                           NRMS = NRMS + 1
                           END IF
                        INDEX = INDEX + INCFI
 30                     CONTINUE
                     FMS = RMS + IMS
                     RMS = MAX (RMS, IMS)
                     FMS2 = FMS
                     RMS2 = RMS
                     IF (NRMS.GT.0) THEN
                        FMS2 = (FMS / NRMS) * (APARM(4)**2)
                        FMS  = (FMS / NRMS) * ((1.414*(4-FLOOP) +
     *                     APARM(4))**2)
                        RMS2 = (RMS / NRMS) * (APARM(5)**2)
                        RMS  = (RMS / NRMS) * ((4-FLOOP + APARM(5))**2)
                        END IF
                     INDEX = OFF + 1
                     WILFLG = .FALSE.
                     DO 35 LOOP = 1,NCHAN
                        IF ((VIS(3,INDEX).GT.0.0) .AND.
     *                     (FITWTS(LOOP+IOFF).GT.0)) THEN
C                                       Interpolated value (re, imag)
                           AVGR = AR + BR * LOOP
                           AVGI = AI + BI * LOOP
C                                       Subtract from vis.
                           RESULT(1) = (VIS(1,INDEX) - AVGR) ** 2
                           RESULT(2) = (VIS(2,INDEX) - AVGI) ** 2
                           AMPLIT = RESULT(1) + RESULT(2)
                           AVGR = MAX (RESULT(1), RESULT(2))
C                                       clip on local rms
                           IF (AMPLIT.GT.FMS2) WILFLG = .TRUE.
                           IF (AVGR.GT.RMS2) WILFLG = .TRUE.
                           IF (AMPLIT.GT.FMS) THEN
                              NNCNT(4) = NNCNT(4) + 1
                              LL1 = MAX (1, LOOP-BPARM1)
                              LL2 = MIN (NCHAN, LOOP+BPARM1)
                              DO 33 LL = LL1,LL2
                                 LI = INDEX + (LL-LOOP) * INCFI
                                 MCHAN(LL,IS,IIF) = MCHAN(LL,IS,IIF)+1
                                 VIS(3,LI) = -ABS (VIS(3,LI))
 33                              CONTINUE
                           ELSE IF (AVGR.GT.RMS) THEN
                              NNCNT(5) = NNCNT(5) + 1
                              LL1 = MAX (1, LOOP-BPARM1)
                              LL2 = MIN (NCHAN, LOOP+BPARM1)
                              DO 34 LL = LL1,LL2
                                 LI = INDEX + (LL-LOOP) * INCFI
                                 MCHAN(LL,IS,IIF) = MCHAN(LL,IS,IIF)+1
                                 VIS(3,LI) = -ABS (VIS(3,LI))
 34                              CONTINUE
                              END IF
                           END IF
                        INDEX = INDEX + INCFI
 35                     CONTINUE
                     IF (.NOT.WILFLG) FLOOP = 5
                     GO TO 15
                  ELSE
                     INDEX = OFF + 1
                     DO 40 LOOP = 1,NCHAN
                        IF (VIS(3,INDEX).GT.0.0) THEN
C                                       Interpolated value (re, imag)
                           AVGR = AR + BR * LOOP
                           AVGI = AI + BI * LOOP
C                                       Subtract from vis.
                           RESULT(1) = (VIS(1,INDEX) - AVGR) ** 2
                           RESULT(2) = (VIS(2,INDEX) - AVGI) ** 2
                           AMPLIT = RESULT(1) + RESULT(2)
C                                       Scale by integration time
                           IF (FITWTS(LOOP+IOFF).GT.0) THEN
                              IF (AMPLIT*VIS(3,INDEX).GT.APARM(2)) THEN
                                 NNCNT(2) = NNCNT(2) + 1
                                 LL1 = MAX (1, LOOP-BPARM1)
                                 LL2 = MIN (NCHAN, LOOP+BPARM1)
                                 DO 38 LL = LL1,LL2
                                    LI = INDEX + (LL-LOOP) * INCFI
                                    MCHAN(LL,IS,IIF) = MCHAN(LL,IS,IIF)
     *                                 + 1
                                    VIS(3,LI) = -ABS (VIS(3,LI))
 38                                 CONTINUE
                                 END IF
                           ELSE
                              IF (AMPLIT.GT.APARM(3)) THEN
                                 NNCNT(3) = NNCNT(3) + 1
                                 LL1 = MAX (1, LOOP-BPARM1)
                                 LL2 = MIN (NCHAN, LOOP+BPARM1)
                                 DO 39 LL = LL1,LL2
                                    LI = INDEX + (LL-LOOP) * INCFI
                                    MCHAN(LL,IS,IIF) = MCHAN(LL,IS,IIF)
     *                                 + 1
                                    VIS(3,LI) = -ABS (VIS(3,LI))
 39                                 CONTINUE
                                 END IF
                              END IF
                           END IF
                        INDEX = INDEX + INCFI
 40                     CONTINUE
                     END IF
                  END IF
               LBAD = 0
               INDEX = OFF + 1
               DO 45 LOOP = 1,NCHAN
                  IF (VIS(3,INDEX).LE.0) LBAD = LBAD + 1
                  INDEX = INDEX + INCFI
 45               CONTINUE
               IF (LBAD.GE.BPARM(2)) THEN
                  NNCNT(10) = NNCNT(10) + 1
                  INDEX = OFF + 1
                  DO 50 LOOP = 1,NCHAN
                     VIS(3,INDEX) = -ABS (VIS(3,INDEX))
                     INDEX = INDEX + INCFI
 50                  CONTINUE
               ELSE
                  INDEX = OFF + 1
                  DO 55 LOOP = 1,NCHAN
                     IF (VIS(3,INDEX).GT.0) THEN
                        NNCNT(8) = NNCNT(8) + 1
                        NGOOD = NGOOD + 1
                        END IF
                     INDEX = INDEX + INCFI
 55                  CONTINUE
                  END IF
C                                       Increase corresponding counter
               MALL(IA1,IA2,IS,IIF) = MALL(IA1,IA2,IS,IIF)+1
               MCBAD(IA1,IA2,IS,IIF) = MCBAD(IA1,IA2,IS,IIF) + NCHAN -
     *            NGOOD
C                                       Now flag everything if needed
               IF (NGOOD.EQ.0)  THEN
C                                       Increase corresponding counters
                  NBDCOR = NBDCOR + 1
                  MBAD(IA1,IA2,IS,IIF) = MBAD(IA1,IA2,IS,IIF)+1
               ELSE IF (GOODDT) THEN
                  IRET = 0
                  END IF
 80            CONTINUE
 90         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Working on visibility record ', I8)
      END
      SUBROUTINE LINFIT (N, X, Y, NDATA, A, B)
C-----------------------------------------------------------------------
C     Routine to fit straight line
C-----------------------------------------------------------------------
      REAL      X(*), Y(*), A, B
      INTEGER   N, NDATA
C
      DOUBLE PRECISION SX, SY, SXY, SYY, SXX
      INTEGER  I
      REAL     DELTA
C-----------------------------------------------------------------------
      SX = 0.D0
      SY = 0.D0
      SXX = 0.D0
      SXY = 0.D0
      SYY = 0.D0
C
      DO 100 I = 1,NDATA
         SY = SY + Y(I)
         IF (N.GT.0) THEN
            SX = SX + X(I)
            SXX = SXX + X(I) * X(I)
            SXY = SXY + X(I) * Y(I)
            SYY = SYY + Y(I) * Y(I)
            END IF
 100     CONTINUE
C
      A = 0.
      B = 0.
      IF ((NDATA.GT.2) .AND. (N.GT.0)) THEN
         DELTA = NDATA * SXX - SX * SX
         IF (DELTA.GT.0) THEN
            A = ( SXX * SY - SX * SXY ) / DELTA
            B = ( SXY * NDATA - SX * SY ) / DELTA
            END IF
      ELSE IF ((NDATA.GT.0) .AND. (N.EQ.0)) THEN
         A = SY / NDATA
         END IF
C
 999  RETURN
      END
      SUBROUTINE FLAGMW (NUMVIS, IA1, IA2, NA, NP, NC, NI, MALL, MBAD,
     *   MCBAD, MCHAN, VIS, RPARM, WASBAD, IRET)
C-----------------------------------------------------------------------
C   Routine to fit median window to chosen channels, subtract and check
C   residual values.  *** IT FITS TO REAL AND IMAGINARY COMPONENTS ***
C   Inputs:
C      NUMVIS   I        Visibility number
C      IA1      I        First antenna number
C      IA2      I        Second antenna number
C      NA       I        Number of antennas
C      NP       I        Number of polarizations
C      NC       I        Number of channels
C      NI       I        Number of IFs
C      VIS      R(3,*)   Visibilities in order real, imaginary, weight
C                        (Jy, Jy, unitless).  Weight <= 0 => flagged.
C      RPARM    R(*)     Random parameter array
C   Inputs from COMMON:
C      BCHAN    I        Lowest channel number.
C      ECHAN    I        Highest channel number.
C      INCSI    I        Input Stokes' increment in vis.
C      INCFI    I        Input frequency increment in vis.
C      INCIFI   I        Input IF increment in vis.
C      CATBLK   I        Catalog header block
C      FITWTS   I(*)     Weights for fitting
C   Output:
C      MALL     I(*)     Counts samples by baseline etc
C      MBAD     I(*)     Counts full bad spectra by baseline
C      MCBAD    I(*)     Counts bad channels by baseline
C      MCHAN    I(*)     Counts flags by channel
C      WASBAD   I(*)     =1 if bad on input, -1 if not, matches VIS
C      IRET     I        Return code   0 => OK
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2, NA, NP, NC, NI, MALL(NA,NA,NP,NI),
     *   MBAD(NA,NA,NP,NI), MCBAD(NA,NA,NP,NI), MCHAN(NC,NP,NI),
     *   WASBAD(*), IRET
      REAL      VIS(3,*), RPARM(*)
C
      INCLUDE 'FLGIT.INC'
      INTEGER   LOOP, INDEX, OFF, IS, IIF, FITNUM, NGOOD, IOFF, BPARM1,
     *   LL, LL1, LL2, LBAD, LI
      REAL      RESULT(2), FITRE(MAXCHA), FITIM(MAXCHA), AMPLIT, VR, VI,
     *   CATUVR(256)
      LOGICAL   GOODDT, PTEST
      DOUBLE PRECISION FZ, FI, FRQMUL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATUVR, CATUV)
C-----------------------------------------------------------------------
      BPARM1 = BPARM(1) + 0.1
C                                       Set up on first call
      IF (NUMVIS.EQ.1) THEN
         NBDCOR = 0
         CALL FILL (9, 0, NNCNT)
         END IF
C                                       process data
      IRET = -1
      IF (NUMVIS.GT.0) THEN
         IF (MOD(NUMVIS,10000).EQ.0) THEN
            WRITE (MSGTXT,1000) NUMVIS
            CALL MSGWRT (2)
            END IF
C                                       Loop over IF, Stokes
         DO 90 IIF = 1,NIF
            IOFF = (IIF - 1) * NCHAN
            FZ = FOFF(IIF) / UVFREQ + 1.0D0
            FI = FINC(IIF) / UVFREQ
            DO 80 IS = 1,NPOLN
               NGOOD = 0
               PTEST = (IS.EQ.1) .AND. (NPOLN.GT.1) .AND.
     *            (APARM(6).GT.0.0)
C                                       Offset in Stokes and IF
               OFF = (IS - 1) * INCSI + (IIF - 1) * INCIFI
C                                       Index for this channel, IF
               INDEX = OFF + 1
               GOODDT = .FALSE.
               DO 5 LOOP = 1,NCHAN
                  IF (VIS(3,INDEX).GT.0.0) THEN
                     WASBAD(INDEX) = -1
                     GOODDT = .TRUE.
                  ELSE
                     WASBAD(INDEX) = 1
                     NNCNT(7) = NNCNT(7) + 1
                     END IF
                  INDEX = INDEX + INCFI
 5                CONTINUE
               INDEX = OFF + 1
               DO 10 LOOP = 1,NCHAN
                  IF (VIS(3,INDEX).GT.0.0) THEN
                     FRQMUL = 1.0D0
                     IF (TYPUVD.LE.0) FRQMUL = FZ + FI *
     *                  (LOOP - 1 + BCHAN - CATUVR(KRCRP+KLOCFY))
                     VR = VIS(1,INDEX)
                     VI = VIS(2,INDEX)
                     AMPLIT = VR*VR + VI*VI
                     IF (ABS(RPARM(1+ILOCU)*FRQMUL).LT.APARM(9)) THEN
                        NNCNT(9) = NNCNT(9) + 1
                        VIS(3,INDEX) = -VIS(3,INDEX)
                        MCHAN(LOOP,IS,IIF) = MCHAN(LOOP,IS,IIF) + 1
                     ELSE IF (AMPLIT.GT.APARM(1)) THEN
                        LL1 = MAX (1, LOOP-BPARM1)
                        LL2 = MIN (NCHAN, LOOP+BPARM1)
                        NNCNT(1) = NNCNT(1) + 1
                        DO 8 LL = LL1,LL2
                           LI = INDEX + (LL-LOOP) * INCFI
                           VIS(3,LI) = -ABS (VIS(3,LI))
                           MCHAN(LL,IS,IIF) = MCHAN(LL,IS,IIF) + 1
 8                         CONTINUE
                        ELSE IF ((PTEST) .AND.
     *                     (VIS(3,INDEX+INCSI).GT.0.0)) THEN
                        VR = VR - VIS(1,INDEX+INCSI)
                        VI = VI - VIS(2,INDEX+INCSI)
                        AMPLIT = VR*VR + VI*VI
                        IF (AMPLIT.GT.APARM(6)) THEN
                           LL1 = MAX (1, LOOP-BPARM1)
                           LL2 = MIN (NCHAN, LOOP+BPARM1)
                           NNCNT(6) = NNCNT(6) + 1
                           DO 9 LL = LL1,LL2
                              LI = INDEX + (LL-LOOP) * INCFI
                              VIS(3,LI) = -ABS (VIS(3,LI))
                              VIS(3,LI+INCSI) = -ABS (VIS(3,LI+INCSI))
                              MCHAN(LL,IS,IIF) = MCHAN(LL,IS,IIF) + 2
 9                            CONTINUE
                           END IF
                        END IF
                     END IF
                  INDEX = INDEX + INCFI
 10               CONTINUE
C                                       Get arrays for line fitting
 15            FITNUM = 0
               INDEX = OFF + 1
               DO 20 LOOP = 1,NCHAN
                  IF (VIS(3,INDEX).LE.0.0) THEN
                     FITRE(LOOP) = FBLANK
                     FITIM(LOOP) = FBLANK
                  ELSE
                     FITNUM = FITNUM + 1
                     FITRE(LOOP) = VIS(1,INDEX)
                     FITIM(LOOP) = VIS(2,INDEX)
                     END IF
                  INDEX = INDEX + INCFI
 20               CONTINUE
C                                       Subt
               IF (FITNUM.GT.0) THEN
                  CALL MWFILT (NCHAN, MAXWIN, AVGWIN, FITRE)
                  CALL MWFILT (NCHAN, MAXWIN, AVGWIN, FITIM)
                  INDEX = OFF + 1
                  DO 40 LOOP = 1,NCHAN
                     IF (VIS(3,INDEX).GT.0.0) THEN
                        RESULT(1) = FITRE(LOOP) ** 2
                        RESULT(2) = FITIM(LOOP) ** 2
                        AMPLIT = RESULT(1) + RESULT(2)
C                                       Scale by integration time
                        IF (FITWTS(LOOP+IOFF).GT.0) THEN
                           IF (AMPLIT*VIS(3,INDEX).GT.APARM(2)) THEN
                              LL1 = MAX (1, LOOP-BPARM1)
                              LL2 = MIN (NCHAN, LOOP+BPARM1)
                              NNCNT(2) = NNCNT(2) + 1
                              DO 30 LL = LL1,LL2
                                 LI = INDEX + (LL-LOOP) * INCFI
                                 MCHAN(LL,IS,IIF) = MCHAN(LL,IS,IIF)+1
                                 VIS(3,LI) = -ABS (VIS(3,LI))
 30                              CONTINUE
                              END IF
                        ELSE
                           IF (AMPLIT.GT.APARM(3)) THEN
                              LL1 = MAX (1, LOOP-BPARM1)
                              LL2 = MIN (NCHAN, LOOP+BPARM1)
                              NNCNT(3) = NNCNT(3) + 1
                              DO 35 LL = LL1,LL2
                                 LI = INDEX + (LL-LOOP) * INCFI
                                 VIS(3,LI) = -ABS (VIS(3,LI))
                                 MCHAN(LL,IS,IIF) = MCHAN(LL,IS,IIF)+1
 35                              CONTINUE
                              END IF
                           END IF
                        END IF
                     INDEX = INDEX + INCFI
 40                  CONTINUE
                  END IF
               LBAD = 0
               INDEX = OFF + 1
               DO 45 LOOP = 1,NCHAN
                  IF (VIS(3,INDEX).LE.0) LBAD = LBAD + 1
                  INDEX = INDEX + INCFI
 45               CONTINUE
               IF (LBAD.GE.BPARM(2)) THEN
                  NNCNT(10) = NNCNT(10) + 1
                  INDEX = OFF + 1
                  DO 50 LOOP = 1,NCHAN
                     VIS(3,INDEX) = -ABS (VIS(3,INDEX))
                     INDEX = INDEX + INCFI
 50                  CONTINUE
               ELSE
                  INDEX = OFF + 1
                  DO 55 LOOP = 1,NCHAN
                     IF (VIS(3,INDEX).GT.0) THEN
                        NNCNT(8) = NNCNT(8) + 1
                        NGOOD = NGOOD + 1
                        END IF
                     INDEX = INDEX + INCFI
 55                  CONTINUE
                  END IF
C                                       Increase corresponding counter
               MALL(IA1,IA2,IS,IIF) = MALL(IA1,IA2,IS,IIF)+1
               MCBAD(IA1,IA2,IS,IIF) = MCBAD(IA1,IA2,IS,IIF) +
     *            NCHAN - NGOOD
C                                       Now flag everything if needed
               IF (NGOOD.EQ.0)  THEN
C                                       Increase corresponding counters
                  NBDCOR = NBDCOR + 1
                  MBAD(IA1,IA2,IS,IIF) = MBAD(IA1,IA2,IS,IIF)+1
               ELSE IF (GOODDT) THEN
                  IRET = 0
                  END IF
 80            CONTINUE
 90         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Working on visibility record ', I8)
      END
      SUBROUTINE MWFILT (NX, NW, NA, VALS)
C-----------------------------------------------------------------------
C   Runs a median window filter through a row of data subtracting the
C   median from the row.
C   Input:
C      NX     I      Number of points in VALS
C      NW     I      Width of filter
C      NA     I      Channels to average
C   In/out:
C      VALS   R(*)   Data row
C-----------------------------------------------------------------------
      INTEGER   NX, NW, NA
      REAL      VALS(*)
C
      INTEGER   MAXW
      PARAMETER (MAXW=127)
      INTEGER   IL(MAXW), I, NL, IH, LA
      REAL      DL(MAXW), RESULT
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      NL = 0
      NW = MIN (MAXW, NW)
      IF (NW.LT.3) NW = 5
      LA = MAX (1, MIN (NA, NW-2))
      IH = NW / 2
C                                       roll 1/2 window in
      DO 10 I = 1,IH
         IF (VALS(I).NE.FBLANK) THEN
            CALL MWMERG (NW, LA, -1, VALS(I), NL, DL, IL, RESULT)
         ELSE
            CALL MWMERG (NW, LA, -2, VALS(I), NL, DL, IL, RESULT)
            END IF
 10      CONTINUE
C                                       loop over row
      DO 20 I = 1,NX
         IF ((I.GT.NX-IH) .OR. (VALS(I+IH).EQ.FBLANK)) THEN
            CALL MWMERG (NW, LA, 0, VALS(I+IH), NL, DL, IL, RESULT)
         ELSE
            CALL MWMERG (NW, LA, 1, VALS(I+IH), NL, DL, IL, RESULT)
            END IF
         IF (VALS(I).NE.FBLANK) VALS(I) = VALS(I) - RESULT
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE MWMERG (NW, NV, NA, DA, NL, DL, IL, RESULT)
C-----------------------------------------------------------------------
C   merges a new value into a sorted list decrementing the
C   corresponding counter list and returning the median value
C   Inputs:
C      NW       I      Window full width
C      NV       I      Number central values (max) to average
C      NA       I      Number values to insert (0, or 1), -1 => insert
C                      one no result returned, -2 no result no insert
C      DA       R      Value to insert
C   In/out:
C      NL       I      Current list length
C      DL       R(*)   ordered list
C      IL       I      number times used so far from list
C   Output:
C      RESULT   R      median value
C-----------------------------------------------------------------------
      INTEGER   NW, NA, NL, NV, IL(*)
      REAL      DA, DL(*), RESULT
C
      INTEGER   I, J, K, J1, J2, LV
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       decrement entries
      IF (NL.GT.0) THEN
         J = 0
         DO 10 I = 1,NL
            IL(I) = IL(I) - 1
            IF (IL(I).EQ.0) J = I
 10         CONTINUE
C                                       squeeze it out
         IF (J.GT.0) THEN
            J = J + 1
            DO 20 I = J,NL
               DL(I-1) = DL(I)
               IL(I-1) = IL(I)
 20            CONTINUE
            NL = NL - 1
            END IF
         END IF
C                                       insert datum
      IF (ABS(NA).EQ.1) THEN
         J = 0
C                                       find place in list
         IF (NL.GT.0) THEN
            DO 30 I = 1,NL
               IF (DA.GT.DL(I)) J = I
 30            CONTINUE
C                                       make room
            DO 40 I = 1,NL-J
               K = NL + 1 - I
               DL(K+1) = DL(K)
               IL(K+1) = IL(K)
 40            CONTINUE
            END IF
         DL(J+1) = DA
         IL(J+1) = NW
         NL = NL + 1
         END IF
C                                       return value
      RESULT = FBLANK
      IF ((NA.GE.0) .AND. (NL.GT.0)) THEN
         J = (NL + 1) / 2
         LV = MAX (1, MIN (NL-1, NV))
         K = LV / 2
         RESULT = 0.0
         IF (MOD(NL,2).EQ.1) THEN
            IF (MOD(LV,2).EQ.1) THEN
               J1 = J - K
               J2 = J + K
            ELSE
               RESULT = (DL(J-K) + DL(J+K)) / 2.0
               J1 = J - K + 1
               J2 = J + K - 1
               END IF
         ELSE
            IF (MOD(LV,2).EQ.1) THEN
               RESULT = (DL(J-K) + DL(J+K+1)) / 2.0
               J1 = J - K + 1
               J2 = J + K
            ELSE
               J1 = J - K + 1
               J2 = J + K
               END IF
            END IF
         DO 50 I = J1,J2
            RESULT = RESULT + DL(I)
 50         CONTINUE
         RESULT = RESULT / LV
         END IF
C
 999  RETURN
      END
      SUBROUTINE MAKEFG (LUN, LFGRNO, FGKOLS, FGNUMV, IA1, IA2, ISUB,
     *   REASON, VIS, RPARM, WASBAD, BUFFLG, IRET)
C-----------------------------------------------------------------------
C   Writes one or more FG table entries for current record
C   Inputs:
C      LUN      I        LUN to use
C      IA1      I        Antenna 1
C      IA2      I        Antenna 2
C      ISUB     I        Subarray
C      REASON   C*24     Reason string
C      VIS      R(3,*)   Visibilities with output flagging
C      RPARM    R(*)     Random parms
C      WASBAD   I(*)     Which were bad before FLGIT
C   In/Out:
C      LFGRNO   I        Record number onto FG table
C      FGKOLS   I(*)     FG table column pointers
C      FGNUMV   I(*)     FG table number parms /column
C      BUFFLG   I(512)   FG table IO buffer
C      IRET     I        Input: -1 => full record to be flagged
C                              -99 => close FG table
C                        Output: FG I/O error ccode
C-----------------------------------------------------------------------
      INTEGER   LUN, LFGRNO, FGKOLS(*), FGNUMV(*), IA1, IA2, ISUB,
     *   WASBAD(*), BUFFLG(512), IRET
      CHARACTER REASON*(*)
      REAL      VIS(3,*), RPARM(*)
C
      INCLUDE 'FLGIT.INC'
      INTEGER   LOOP, INDEX, OFF, IS, IIF, IDS, LS, LBC, LEC, LIF, JS,
     *   JSM, JX
      REAL      BT, ET, EPS
      LOGICAL   PFLAGS(4), DOFL
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA EPS /1.11574E-6/
C-----------------------------------------------------------------------
      IDS = 0
      IF (ILOCSU.GE.0) IDS = RPARM(1+ILOCSU)
      BT = RPARM(1+ILOCT) - EPS
      ET = RPARM(1+ILOCT) + EPS
C                                       close
      IF (IRET.EQ.-99) THEN
         IF (NFGWRI.GT.0) THEN
            CALL FLAGFG ('CLOS', LUN, DISKIN, CNOIN, FGVERI, FGVERO,
     *         LFGRNO, FGKOLS, FGNUMV, IDS, ISUB, FRQSEL, IA1, IA2, BT,
     *         ET, 0, 0, 0, 0, PFLAGS, REASON, CATUV, BUFFLG, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'CLOS'
               CALL MSGWRT (8)
               END IF
         ELSE
            IRET = 0
            END IF
C                                       flag all
      ELSE IF (IRET.LT.0) THEN
         CALL LFILL (4, .TRUE., PFLAGS)
         CALL FLAGFG ('FLAG', LUN, DISKIN, CNOIN, FGVERI, FGVERO,
     *      LFGRNO, FGKOLS, FGNUMV, IDS, ISUB, FRQSEL, IA1, IA2, BT, ET,
     *      0, 0, 0, 0, PFLAGS, REASON, CATUV, BUFFLG, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'FLAG'
            CALL MSGWRT (8)
            END IF
         NFGWRI = NFGWRI + 1
C                                       flag some
      ELSE
C                                       Loop over IF, Stokes
         DO 90 IS = 1,NPOLN
            LS = ABS (ICOR0) + IS - 1
            CALL LFILL (4, .FALSE., PFLAGS)
            PFLAGS(LS) = .TRUE.
            JSM = 0
            IF (APARM(10).GT.0) THEN
               IF ((LS.EQ.1) .OR. ((LS.EQ.2) .AND. (ICOR0.LT.0))) THEN
                  PFLAGS(3) = .TRUE.
                  PFLAGS(4) = .TRUE.
                  IF (ICOR0.GT.0) PFLAGS(2) = .TRUE.
                  JSM = ABS (ICOR0) + NPOLN - 1
                  END IF
               END IF
            DO 80 IIF = 1,NIF
               LIF = IIF + BIF - 1
C                                       Offset in Stokes and IF
               OFF = (IS - 1) * INCSI + (IIF - 1) * INCIFI
C                                       Index for this channel, IF
               INDEX = OFF + 1
               DOFL = .FALSE.
               DO 70 LOOP = 1,NCHAN
                  IF (DOFL) THEN
                     IF (VIS(3,INDEX).LE.0.0) THEN
                        LEC = LOOP + BCHAN - 1
                        DO 10 JS = LS+1,JSM
                           JX = INDEX + (JS - LS) * INCSI
                           IF (PFLAGS(JS)) WASBAD(JX) = MAX (0,
     *                        WASBAD(JX))
 10                        CONTINUE
                     ELSE
                        CALL FLAGFG ('FLAG', LUN, DISKIN, CNOIN, FGVERI,
     *                     FGVERO, LFGRNO, FGKOLS, FGNUMV, IDS, ISUB,
     *                     FRQSEL, IA1, IA2, BT, ET, LIF, LIF, LBC, LEC,
     *                     PFLAGS, REASON, CATUV, BUFFLG, IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET, 'FLAG'
                           CALL MSGWRT (8)
                           GO TO 999
                           END IF
                        NFGWRI = NFGWRI + 1
                        DOFL = .FALSE.
                        END IF
                  ELSE IF ((VIS(3,INDEX).LE.0.0) .AND.
     *               (WASBAD(INDEX).LT.0)) THEN
                     DOFL = .TRUE.
                     LBC = LOOP + BCHAN - 1
                     LEC = LBC
                     DO 20 JS = LS+1,JSM
                        JX = INDEX + (JS - LS) * INCSI
                        IF (PFLAGS(JS)) WASBAD(JX) = MAX (0, WASBAD(JX))
 20                     CONTINUE
                     END IF
                  INDEX = INDEX + INCFI
 70               CONTINUE
 80            CONTINUE
 90         CONTINUE
         END IF
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAKEFG: FLAGFG RETURNS ERROR CODE',I5,' ON ',A)
      END
      SUBROUTINE FLAGFG (OPCODE, LUN, DISK, CNO, VERI, VER, LFGRNO,
     *   FGKOLS, FGNUMV, ID, SUBA, FQID, ANT1, ANT2, BTIME, ETIME, BIF,
     *   EIF, BCHAN, ECHAN, PFLAGS, REASON, CATUV, BUFF, IRET)
C-----------------------------------------------------------------------
C   Updates the Flag (FG) table. Adapted from FLAGUP
C   One entry is made indicating a visibility to be rejected.
C   The FLAG table will be opened on the first call but a final call
C   with OPCODE='CLOS' is required to close the file.
C   Inputs:
C      OPCODE   C*4      Operation desired, 'CLOS'=>close file
C                        Anything else = 'FLAG'
C      LUN      I        Logical unit number to use
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VERI     I        Input version number
C      VER      I        FG file version
C      ID       I        Source ID as defined in SOURCE table
C      SUBA     I        Subarray number.
C      FQID     I        Freqid number
C      ANT1     I        First antenna number in baseline
C      ANT2     I        Second antenna number in baseline
C      BTIME    R        Start time of data to be flagged (Days)
C      ETIME    R        End time of data to be flagged (Days)
C      BIF      I        First IF number to flag. 0=>all
C      EIF      I        Last IF number to flag. 0=>all higher than IFS(1)
C      BCHAN    I        First channel number to flag. 0=>all
C      ECHAN    I        Last channel number to flag. 0=>all higher.
C      PFLAGS   L(4)     Correlator flags
C      REASON   C*24     Reason for flagging blank => ignore for unflag.
C   Input/Output:
C      LFGRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      FGKOLS   I(*)     The column pointer array in order, SOURCE,
C                        SUBARRAY, ANTS, TIMERANG, IFS, CHANS, PFLAGS,
C                        REASON
C      FGNUMV   I(*)     Element count in each column.
C      CATUV    I(256)   Header for disk file to get FG table
C      BUFF     I(512)   I/O buffer and related storage, also defines
C                        file if open.
C   Output:
C      IRET     I        Error code, 0=>OK else TABIO error.
C                        Note: -1 => read, but record deselected.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, REASON*24
      INTEGER   LUN, DISK, CNO, VERI, VER, LFGRNO, FGKOLS(*), FGNUMV(*),
     *   ID, SUBA, FQID, ANT1, ANT2, BIF, EIF, BCHAN, ECHAN, CATUV(256),
     *   BUFF(*), IRET
      REAL      BTIME, ETIME
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TREAS*24, CTEMP*12
      INTEGER   IDT, SUBT, ANTS(2), IFS(2), CHANS(2), IDUM, FIND, I,
     *   BUFF2(512), LUN2, IFGKOL(MAXFGC), IFGNUM(MAXFGC), NROW, IFQ,
     *   IFGRNO
      LOGICAL   PFLAGS(4), TFLAGS(4), FIRST
      REAL      TIMER(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE FIRST
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
C                                       See if table open - check FTAB
      IF (OPCODE.NE.'CLOS') THEN
         FIND = BUFF(82)
C                                       Open file
         IF ((FIND.LT.0) .OR. (FIND.GT.10000) .OR. (LUN.NE.FTAB(FIND)))
     *      THEN
            CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
     *         IDUM, 'CLRD', BUFF, IRET)
            CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
     *         IDUM, 'WRIT', BUFF, IRET)
C
            CALL FLGINI ('WRIT', BUFF, DISK, CNO, VER, CATUV, LUN,
     *         LFGRNO, FGKOLS, FGNUMV, IRET)
C                                       Report on the need for flagging
            WRITE (MSGTXT,1000) VER
            IF (.NOT.FIRST) WRITE (MSGTXT,1001) VER
            CALL MSGWRT (2)
            IF (IRET.NE.0) GO TO 999
C                                       Copy the old file
            IF ((FIRST) .AND. (VERI.GT.0)) THEN
               LUN2 = LUN + 1
               CALL FLGINI ('READ', BUFF2, DISK, CNO, VERI, CATUV, LUN2,
     *            IFGRNO, IFGKOL, IFGNUM, IRET)
               IF (IRET.NE.0) GO TO 999
               NROW = BUFF2(5)
               WRITE (MSGTXT,1002) NROW, VERI, VER
               CALL MSGWRT (2)
               DO 20 I = 1,NROW
                  CALL TABFLG ('READ', BUFF2, IFGRNO, IFGKOL, IFGNUM,
     *               IDT, SUBT, IFQ, ANTS, TIMER, IFS, CHANS, TFLAGS,
     *               TREAS, IRET)
                  IF (IRET.GT.0) GO TO 999
                  IF (IRET.EQ.0) THEN
                     CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV,
     *                  IDT, SUBT, IFQ, ANTS, TIMER, IFS, CHANS, TFLAGS,
     *                  TREAS, IRET)
                     IF (IRET.NE.0) GO TO 999
                     END IF
 20               CONTINUE
               CALL TABIO ('CLOS', 0, IFGRNO, BUFF2, BUFF2, I)
               END IF
            FIRST = .FALSE.
C                                       Mark as unsorted
            BUFF(43) = 0
            BUFF(44) = 0
            END IF
C                                       Set up for flagging
         ANTS(1) = ANT1
         ANTS(2) = ANT2
         TIMER(1) = BTIME
         TIMER(2) = ETIME
         IFS(1) = BIF
         IFS(2) = EIF
         CHANS(1) = BCHAN
         CHANS(2) = ECHAN
C                                       Flag table entry.
         CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV, ID, SUBA,
     *      FQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
C                                       Close
      ELSE
         CALL TABFLG ('CLOS', BUFF, LFGRNO, FGKOLS, FGNUMV, IDT, SUBT,
     *      FQID, ANTS, TIMER, IFS, CHANS, TFLAGS, TREAS, IRET)
C                                       Clear write status
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV', IDUM,
     *      'CLWR', BUFF, IRET)
C                                       Reset status to read
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV', IDUM,
     *      'READ', BUFF, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Found some bad data, will write flags to table FG', I4)
 1001 FORMAT ('Found some bad data, will add   flags to table FG', I4)
 1002 FORMAT ('Copy',I8,' rows from FG vers',I3,' to',I3)
      END

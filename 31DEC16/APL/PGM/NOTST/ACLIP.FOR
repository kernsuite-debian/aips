LOCAL INCLUDE 'ACLIP.INC'
C                                       Local include for ACLIP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   SEQIN, CNOIN, DISKIN, JBUFSZ, LRECC, NBDCOR, NRPRMI,
     *   INCSI, INCFI, INCIFI, FGVERI, FGVERO, LBCHAN, LECHAN, CTYP(4),
     *   INTY, OUTY, NANT, NIF, NPOLN, NPOLI
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC, XXSTOK
      CHARACTER NAMEIN*12, CLAIN*6, XCALCO*4, XSOUR(300)*16, REASON*24
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XBCHAN, XECHAN, XBIF, XEIF, XSUBA, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XFGOUT, XDOBND, XBPVER, XSMOTH(3),
     *   XANT(50), APARM(10), XBADD(10),
     *   BUFF1(UVBFSS), CMIN(4), CMAX(4)
      LOGICAL   DOUVCM, TESTIT(4), FLAGXX, FLAGP(4,4), DOALL
      INTEGER   NSOU
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XQUAL,
     *   XXCALC, XXSTOK, XTIME, XBAND, XFREQ, XFQID, XBCHAN, XECHAN,
     *   XBIF, XEIF, XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XFGOUT, XDOBND, XBPVER, XSMOTH, XANT, APARM, XBADD
      COMMON /INFO/ FGVERO, NSOU, SEQIN, DISKIN, CNOIN, LRECC, DOUVCM,
     *   NRPRMI, INCSI, INCFI, INCIFI, NBDCOR, LBCHAN, LECHAN, TESTIT,
     *   CMIN, CMAX, CTYP, FLAGXX, INTY, OUTY, FLAGP, NANT, NIF, NPOLN,
     *   DOALL, NPOLI, FGVERI
      COMMON /CHRCOM/ NAMEIN, CLAIN, XCALCO, XSOUR, REASON
      INTEGER   FLGIT(4,4)
      LOGICAL   LFLAG(4,4)
      COMMON /RICKIT/ FLGIT, LFLAG
LOCAL END
      PROGRAM ACLIP
C-----------------------------------------------------------------------
C! Applies calibration and/or editing and flags multisource uv data.
C# task editing UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1999-2007, 2009-2012, 2014-2016
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
C   Loops over sources in a multi-source line uv data set applying
C   calibration and editing.  It then compares amplitudes to allowed
C   ranges and makes entries in an FG table for excess values.
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
C      BCHAN          LBCHAN        Examine LBCHAN-LECHAN
C      ECHAN          LECHAN        Examine LBCHAN-LECHAN
C      BIF            BIF           First IF to copy. 0=>all.
C      EIF            EIF           Highest IF to copy. 0=> highest.
C      SUBARRAY       SUBARR        Subarray number to copy. 0=> all.
C      DOCALIB        DOCAL         If true (>0), calibrate the data.
C      GAINUSE        CLUSE         Version of the Cal. table to use.
C      FLAGVER        FGVER         Version of the FG table to use.
C      DOBAND         DOBAND        If true correct data for bandpass
C      BPVER          BPVER         The BP table to apply
C      BADDISK        IBAD          Disks to avoid for scratch files.
C-----------------------------------------------------------------------
      INCLUDE 'ACLIP.INC'
      INCLUDE 'INCS:DSEL.INC'
C
      CHARACTER PRGM*6
      LOGICAL   DOWANT
      INTEGER   NUMSOU, IRET, I, SULIST(XSTBSZ), MALL(2), MBAD(2),
     *   MSOM(2), NWORDS
      LONGINT   OFFALL, OFFBAD, OFFSOM
C     INTEGER   MALL(MAXANT,4*MAXIF),
C    *   MBAD(MAXANT,4*MAXIF), MSOM(MAXANT,4*MAXIF)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'ACLIP '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL CLPMIN (PRGM, DOWANT, NUMSOU, SULIST, IRET)
C                                       Allocate memory
      NWORDS = NANT * 4 * NIF
      NWORDS = (NWORDS - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MALL, OFFALL,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MBAD, OFFBAD,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MSOM, OFFSOM,
     *   IRET)
      IF (IRET.EQ.0) THEN
         NWORDS = 1024 * NWORDS
         CALL FILL (NWORDS, 0, MALL(1+OFFALL))
         CALL FILL (NWORDS, 0, MBAD(1+OFFBAD))
         CALL FILL (NWORDS, 0, MSOM(1+OFFSOM))
         END IF
C                                       Loop over sources.
      IF (IRET.EQ.0) CALL CLPMUV (NANT, 4, NIF, MALL(1+OFFALL),
     *   MBAD(1+OFFBAD), MSOM(1+OFFSOM), DOWANT, NUMSOU, SULIST, IRET)
C                                       report results
      IF (IRET.EQ.0) CALL REPORT (NANT, 4, NIF, MALL(1+OFFALL),
     *   MBAD(1+OFFBAD), MSOM(1+OFFSOM))
C                                       Report deeds to History file
      IF (IRET.EQ.0) CALL CLPMHI
C                                       Done: Close down files, etc.
      CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, MSOM, OFFSOM, I)
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE CLPMIN (PRGN, DOWANT, NUMSOU, SULIST, JERR)
C-----------------------------------------------------------------------
C   CLPMIN gets input parameters for ACLIP, finds input file and
C   prepares the list of sources.  All selection criteria except
C   for the source name are filled into the commons in C/DSEL.INC.
C   Inputs:  PRGN    C*6   Program name
C   Output:
C     DOWANT       L    If true sources listed are selected
C     NUMSOU       I    Number of sources to process, 0=>all
C     SULIST(*)    I    Source number list.
C     JERR         I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      LOGICAL   DOWANT
      INTEGER   NUMSOU, SULIST(*), JERR
C
      HOLLERITH CATH(256)
      CHARACTER STAT*4, UTYPE*2, ATIME*8, ADATE*12
      INTEGER   NPARM, IROUND, IERR, I, LUN, TIME(3), DATE(3), NVER,
     *   NUMAN(513), J, JJ
      REAL      CATR(256)
      LOGICAL   MATCH
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'ACLIP.INC'
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
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 228
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
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
C                                       Sources
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), SOURCS(I))
 20      CONTINUE
C                                       Save the time range
      CALL RCOPY (8, XTIME, TIMRNG)
C                                       BADDISK
      DO 30 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 30      CONTINUE
C                                       Set flagging reason
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      REASON = TSKNAM // ADATE // ' ' // ATIME(:5)
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
C                                       Get uv header pointers.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      INTY = ICOR0
      NPOLI = CATBLK(KINAX+JLOCS)
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
C                                       Set BCHAN and ECHAN to all
      BCHAN = 1
      ECHAN = CATBLK(KINAX+JLOCF)
C                                       Set examine range differently
      LBCHAN = IROUND (XBCHAN)
      LBCHAN = MAX (BCHAN, MIN (ECHAN, LBCHAN))
      LECHAN = IROUND (XECHAN)
      IF ((LECHAN.LT.LBCHAN) .OR. (LECHAN.GT.ECHAN)) LECHAN = ECHAN
C                                       Check wanted IFs
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (BIF.GT.EIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
         DOALL = (BIF.EQ.1) .AND. (EIF.EQ.CATBLK(KINAX+JLOCIF))
      ELSE
         BIF = 1
         EIF = 1
         DOALL = .TRUE.
         END IF
      NIF = EIF - BIF + 1
C                                       Setup calibration choices
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      BLVER = IROUND (XBLVER)
      DOUVCM = .FALSE.
      DO 40 I = 1,50
         ANTENS(I) = IROUND (XANT(I))
 40      CONTINUE
      IF (STOKES.EQ.' ') THEN
         IF (NCOR.EQ.4) THEN
            STOKES = 'FULL'
         ELSE IF (NCOR.EQ.3) THEN
            STOKES = 'HALF'
         ELSE IF (NCOR.EQ.2) THEN
            STOKES = 'HALF'
         ELSE
            IF (ICOR0.EQ.-1) STOKES = 'RR'
            IF (ICOR0.EQ.-2) STOKES = 'LL'
            IF (ICOR0.EQ.-5) STOKES = 'XX'
            IF (ICOR0.EQ.-6) STOKES = 'YY'
            END IF
      ELSE
         DOALL = .FALSE.
         END IF
      SUBARR = IROUND (XSUBA)
C                                       set flag versions
      CALL FNDEXT ('FG', CATBLK, I)
      FGVER = IROUND (XFLAG)
      IF ((FGVER.EQ.0) .OR. (FGVER.GT.I)) FGVER = I
      FGVERO = IROUND (XFGOUT)
      IF ((FGVERO.LE.0) .OR. (FGVERO.GT.I)) FGVERO = I + 1
      FGVERI = FGVER
      IF (FGVERO.LE.I) FGVERI = -ABS (FGVERI)
C                                       cal parameters
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Do only auto-correlations
      DOXCOR = .FALSE.
      DOACOR = .TRUE.
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'No match to SELBAND/SELFREQ adverbs - check inputs'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       Choice of qualifiers and calcode
      SELQUA = IROUND (XQUAL)
      SELCOD = XCALCO
C                                       Get source list
      IUDISK = FVOL(1)
      IUCNO = FCNO(1)
      IXLUN = 28
      CALL SOUFIL (JERR)
      IF (JERR.NE.0) GO TO 999
      DOWANT = DOSWNT
      NUMSOU = NSOUWD
      CALL COPY (30, SOUWAN, SULIST)
C                                       Reset values in /SELCAL/
C                                       Empty names of sources done
      DO 90 I = 1,30
         SOURCS(I) = ' '
         XSOUR(I) = ' '
 90      CONTINUE
      NSOUWD = 0
C                                       Max antenna number
      NANT = MAXANT
      CALL FNDEXT ('AN', CATBLK, NVER)
      IF (NVER.GT.0) THEN
         CALL GETNAN (DISKIN, CNOIN, CATBLK, LUN, BUFF1, NUMAN, JERR)
         IF ((NVER.GT.0) .AND. (JERR.EQ.0)) THEN
            JJ = NUMAN(1)
            NANT = 0
            DO 80 J = 1,JJ
               NANT = MAX (NANT, NUMAN(J+1))
 80            CONTINUE
            END IF
         END IF
C                                        Default clip levels
      IF (APARM(1).EQ.0.0) APARM(1) = 1.0E6
      IF (APARM(2).EQ.0.0) APARM(2) = 1.0E6
      IF (APARM(3).EQ.0.0) APARM(3) = -1.0E6
      IF (APARM(4).EQ.0.0) APARM(4) = -1.0E6
      IF (APARM(5).LE.0.0) APARM(5) = -1.0E12
      IF (APARM(6).LE.0.0) APARM(6) = 1.0E12
      IF (APARM(7).LT.0.0) APARM(7) = 0.0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLPMIN: error',I3,' obtaining input parameters')
 1030 FORMAT ('Error',I3,' finding ',A12,'.',A6,'.',I4,' disk =',
     *   I3,' user=',I5)
 1035 FORMAT ('Error',I3,' obtaining CATBLK ')
      END
      SUBROUTINE CLPMUV (NA, NP, NI, MALL, MBAD, MSOM, DOWANT, NUMSOU,
     *   SULIST, IRET)
C-----------------------------------------------------------------------
C   CLPMUV uses UVGET to obtain data as single source files and CLPMOP
C   to loop through a source at a time to check for bad data
C   Input:
C      NA       I      Number of antennas
C      NP       I      Number of polarizations
C      NI       I      Number of IFs
C      DOWANT   L      If true sources listed are selected
C      NUMSOU   I      Number of sources to process, 0=>all
C      SULIST   I(*)   Source number list.
C   Output:
C      MALL     I(*)   Counts samples by baseline etc
C      MBAD     I(*)   Counts full bad spectra by baseline
C      MSOM     I(*)   Counts partly bad spectra by baseline
C      IRET     I      Return code, 0 => OK, otherwise abort.
C   Auxiliary:
C     FGVERO    I      Version number of new flag table (reset later)
C-----------------------------------------------------------------------
      LOGICAL   DOWANT
      INTEGER   NA, NP, NI, MALL(NA,NP,NI), MBAD(NA,NP,NI),
     *   MSOM(NA,NP,NI), NUMSOU, SULIST(*), IRET
C
      INCLUDE 'ACLIP.INC'
      CHARACTER VELTYP*8, VELDEF*8, CALCOD*4
      LOGICAL   DOAVG, DOAPPT, NOSUB, SINGLE, TABLE, EXIST, FITASC,
     *   FIRST
      INTEGER   NUMVIS, SOUCUR, MAXSOU, SLOOP, DPOSAV, LRECU, NRPRIN,
     *   IERR, SUKOLS(MAXSUC), SUNUMV(MAXSUC), SBUFF(512), I, SLUN,
     *   IDSOU, QUAL, INOGRP, SUB, NUMSUB, LIMS1, LIMS2, SUBTMP, SAVBND,
     *   SUFQID, INCX, ICOR, IROUND, J
      REAL      RPARM, VIS, CATR(256), OLDRP, TEMP
      DOUBLE PRECISION  BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   RAOBS, DECOBS, PMRA, PMDEC, CATD(128), OLDFRQ
      DOUBLE PRECISION   LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      REAL     FLUX(4,MAXIF)
      HOLLERITH CATH(256)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
C-----------------------------------------------------------------------
      DOAPPT = DOAPPL
      DOAVG = .FALSE.
      NOSUB = .FALSE.
      OLDRP = CATR(KRCRP+JLOCF)
      FIRST = .TRUE.
C                                       Zero visibility count
      NUMVIS = 0
C                                       Zero number of processed sources
      NSOU = 0
C                                       Get input reference frequency
      OLDFRQ = CATD(KDCRV+JLOCF)
C                                       Find number of subarrays
      CALL FNDEXT ('AN', CATUV, NUMSUB)
      NUMSUB = MAX (1, NUMSUB)
      SUBARR = MIN (SUBARR, NUMSUB)
      IF (SUBARR.GT.0) THEN
         LIMS1 = SUBARR
         LIMS2 = SUBARR
      ELSE
         LIMS1 = 1
         LIMS2 = NUMSUB
         END IF
      SUBTMP = SUBARR
C                                       Check if single source file
      SLUN = 27
      CALL MULSDB (CATUV, SINGLE)
      IF (SINGLE) THEN
         CALL ISTAB ('SU', FVOL(1), FCNO(1), 1, SLUN, SBUFF, TABLE,
     *      EXIST, FITASC, IERR)
         SINGLE = EXIST .AND. TABLE .AND. (IERR.EQ.0)
         END IF
      SINGLE = .NOT.SINGLE
C                                       Allow single-source now
      IF (SINGLE) THEN
         IDSOU = 1
         CALL H2CHR (8, 1, CATH(KHOBJ), SOURCS(1))
         MAXSOU = 1
C                                       Open source table
      ELSE
         CALL SOUINI ('READ', SBUFF, FVOL(1), FCNO(1), 1, CATUV, SLUN,
     *      INOGRP, VELTYP, VELDEF, SUFQID, SOUCUR, SUKOLS, SUNUMV,
     *      IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'Error opening the source table'
            GO TO 990
            END IF
C                                       Report number of sources
         MAXSOU = SBUFF(5)
         WRITE (MSGTXT,1000) MAXSOU
         CALL MSGWRT (2)
         END IF
C                                       Loop here over sources
      DO 200 SLOOP = 1,MAXSOU
C                                       Save SLOOP from TABSOU
         SOUCUR = SLOOP
C                                       Read source table
         IF (.NOT.SINGLE) THEN
            CALL TABSOU ('READ', SBUFF, SOUCUR, SUKOLS, SUNUMV, IDSOU,
     *         SOURCS, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'Error reading the source table'
               GO TO 990
               END IF
C                                       Restore SOUCUR
            SOUCUR = SLOOP
C                                       See if this source is wanted.
            IF (NUMSOU.GT.0) THEN
               DO 110 I = 1,NUMSOU
                  IF (IDSOU.EQ.SULIST(I)) THEN
                     IF (DOWANT) GO TO 120
                     GO TO 200
                     END IF
 110              CONTINUE
               IF (DOWANT) GO TO 200
               END IF
            END IF
C                                       Setup header for calibrated
C                                       single-source file, save input
C                                       header
 120     SUBARR = SUBTMP
         DPOSAV = DOPOL
         DOPOL = 0
         SAVBND = DOBAND
         DOBAND = 0
C                                       is there any subarray with
C                                       data for this source?
         DO 125 SUB = LIMS1,LIMS2
            SUBARR = SUB
            CALL UVGET ('INIT', RPARM, VIS, IERR)
            IF (IERR.GT.0) GO TO 190
            IF (IERR.EQ.0) GO TO 126
            CALL UVGET ('CLOS', RPARM, VIS, IERR)
 125        CONTINUE
         WRITE (MSGTXT,1125) IDSOU, SOURCS(1)
         CALL MSGWRT (6)
         IERR = 0
         GO TO 200
C                                       Initialize reading data
C                                       Report source it is working on
 126     WRITE (MSGTXT,1126) IDSOU, SOURCS(1)
         CALL MSGWRT (2)
C                                       Save this source's name
         NSOU = NSOU + 1
         XSOUR(NSOU) = SOURCS(1)
C                                       Initialize single-source header
         OUTY = ICOR0
         DOPOL = DPOSAV
         DOAPPL = .FALSE.
         DOBAND = SAVBND
         CLVER = CLUSE
C                                       For all stokes correlators
         IF (FIRST) THEN
            FLAGXX = .TRUE.
            IF (APARM(8).GT.0.0) FLAGXX = .FALSE.
            NPOLN = NCOR
            CALL FILL (16, 0, FLGIT)
            DO 130 I = 1,NPOLN
               TEMP = CATD(KDCRV+JLOCS) + (I-CATR(KRCRP+JLOCS)) *
     *            CATR(KRCIC+JLOCS)
               ICOR = IROUND (TEMP)
               CTYP(I) = 0
C                                       Stokes type, I
               IF (ICOR.EQ.1) THEN
                  CTYP(I) = 1
                  FLGIT(1,I) = 1
                  IF (INTY.LT.0) FLGIT(2,I) = 1
                  IF (FLAGXX) CALL FILL (3, 1, FLGIT(2,I))
C                                       Stokes type, Q, U
               ELSE IF ((ICOR.GE.2) .AND. (ICOR.LE.3)) THEN
                  CTYP(I) = 2
                  IF (INTY.GT.0) THEN
                     FLGIT(ICOR,I) = 1
                  ELSE
                     FLGIT(3,I) = 1
                     FLGIT(4,I) = 1
                     END IF
C                                       Stokes V
               ELSE IF (ICOR.EQ.4) THEN
                  CTYP(2) = 2
                  IF (INTY.GT.0) THEN
                     FLGIT(ICOR,I) = 1
                  ELSE
                     FLGIT(1,I) = 1
                     FLGIT(2,I) = 1
                     END IF
C                                       Stokes type  RR or LL
               ELSE IF ((ICOR.EQ.-1) .OR. (ICOR.EQ.-2)) THEN
                  CTYP(I) = 1
                  IF (INTY.GT.0) THEN
                     FLGIT(1,I) = 1
                     FLGIT(4,I) = 1
                  ELSE
                     FLGIT(-ICOR,I) = 1
                     IF (FLAGXX) CALL COPY (2, 1, FLGIT(3,I))
                     END IF
C                                       Stokes type  RL or LR
               ELSE IF ((ICOR.EQ.-3) .OR. (ICOR.EQ.-4)) THEN
                  CTYP(I) = 2
                  IF (INTY.GT.0) THEN
                     FLGIT(2,I) = 1
                     FLGIT(3,I) = 1
                  ELSE
                     FLGIT(-ICOR,I) = 1
                     END IF
C                                       Stokes type  XX or YY
               ELSE IF ((ICOR.EQ.-5) .OR. (ICOR.EQ.-6)) THEN
                  CTYP(I) = 1
                  IF (INTY.GT.0) THEN
                     FLGIT(1,I) = 1
                     FLGIT(4,I) = 1
                  ELSE
                     FLGIT(-ICOR-4,I) = 1
                     IF (FLAGXX) CALL FILL (2, 1, FLGIT(3,I))
                     END IF
C                                       Stokes type  XY or YX
               ELSE IF ((ICOR.EQ.-7) .OR. (ICOR.EQ.-8)) THEN
                  CTYP(I) = 2
                  IF (INTY.GT.0) THEN
                     FLGIT(2,I) = 1
                     FLGIT(3,I) = 1
                  ELSE
                     FLGIT(-ICOR-4,I) = 1
                     END IF
                  END IF
               CMAX(I) = 1.E6
               IF (CTYP(I).NE.0) CMAX(I) = APARM(CTYP(I))
               CMIN(I) = -1.E6
               IF (CTYP(I).NE.0) CMIN(I) = APARM(CTYP(I)+2)
               TESTIT(I) = (CMIN(I).GT.-1.E6) .OR. (CMAX(I).LT.1.E6)
     *            .OR. (APARM(5).GT.0.0) .OR. (APARM(6).LT.1.E12)
 130           CONTINUE
            DO 136 J = 1,4
               DO 135 I = 1,4
                  LFLAG(I,J) = FLGIT(I,J).NE.0
 135              CONTINUE
 136           CONTINUE
            FIRST = .FALSE.
            END IF
C                                       Close again - only need header
C                                       for sum of subarrays.
         CALL UVGET ('CLOS', RPARM, VIS, IERR)
         IF (IERR.GT.0) GO TO 190
C                                       Get values of data increments
         INCX = CATBLK(KINAX)
         NRPRMI = NRPARM
         INCSI = INCS / INCX
         INCFI = INCF / INCX
         INCIFI = INCIF / INCX
C                                       Make sure there is data
         IF (CATBLK(KIGCN).LE.0) GO TO 190
C                                       Loop over subarrays.
         DO 150 SUB = LIMS1, LIMS2
            SUBARR = SUB
C                                       Initialize reading data
            CALL UVGET ('INIT', RPARM, VIS, IERR)
            IF (IERR.GT.0) GO TO 190
            IF (IERR.EQ.0) THEN
               LRECU = LREC
               NRPRIN = NRPARM
C                                       Loop through data
               CALL CLPMOP (NUMVIS, NA, NP, NI, MALL, MBAD, MSOM, LRECU,
     *            NRPRIN, FVOL(1), FCNO(1), IDSOU, IERR)
               IF (IERR.NE.0) GO TO 190
            ELSE
               CALL UVGET ('CLOS', RPARM, VIS, IERR)
               END IF
            IERR = 0
 150        CONTINUE
         GO TO 200
C                                       Error, close input then die
 190     CALL UVGET ('CLOS', RPARM, VIS, IERR)
         MSGTXT = 'PROBLEM WITH SOURCE: ' // SOURCS(1)
         IRET = 1
         GO TO 990
C
 200     CONTINUE
C                                       Last call to CLPMOP
      CALL CLPMOP (-1, NA, NP, NI, MALL, MBAD, MSOM, LRECU, NRPRIN,
     *   FVOL(1), FCNO(1), IDSOU, IERR)
C                                       Close source table
      IF (.NOT.SINGLE) CALL TABIO ('CLOS', 1, SOUCUR, SBUFF, SBUFF,
     *   IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X, I3, ' sources in this file')
 1125 FORMAT ('No data for source number', I3,' name ',A)
 1126 FORMAT ('Working on source number', I3,' name ',A)
      END
      SUBROUTINE CLPMOP (NUMVIS, NA, NP, NI, MALL, MBAD, MSOM, LRECU,
     *   NRPRIN, INDISK, INNUM, IDSOUR, IRET)
C-----------------------------------------------------------------------
C   CLPMOP calls CLIPF to do clipping and raise the flags which are
C   entered upon return from CLIPF
C   Input:
C      NUMVIS   I      Number of visibilities previously processed
C      NA       I      Number of antennas
C      NP       I      Number of polarizations
C      NI       I      Number of IFs
C      LRECU    I      length of uncompressed vis record
C      NRPRIN   I      # random parms in uncompressed vis record
C   Input from common:
C      INCF     I      Increment in freq. of data from UVGET
C      INCIF    I      Increment in IF of data from UVGET
C      INCS     I      Increment in Stokes' of data from UVGET
C      JLOCF    I      Offset of freq. of data from UVGET
C      JLOCIF   I      Offset of IF of data from UVGET
C      JLOCS    I      Offset of Stokes' of data from UVGET
C   Output:
C      MALL     I(*)   Counts samples by baseline etc
C      MBAD     I(*)   Counts full bad spectra by baseline
C      MSOM     I(*)   Counts partly bad spectra by baseline
C      IRET     I      Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, NA, NP, NI, MALL(NA,NP,NI), MBAD(NA,NP,NI),
     *   MSOM(NA,NP,NI), LRECU, NRPRIN, INDISK, INNUM, IDSOUR, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   FLGKEY(MAXCIF), VCOUNT, LUN, NOPOL, I, JNCIF, JNCS, J,
     *   K, NUMFRQ, LRECO, LENBU, IA1, IA2, INCX, INNUMS, FGBUFL(512),
     *   LFGRNO, JJ, L, JB, JE, IB0, IB1, JX
      LOGICAL   PFLAGS(4), GOODDT, FLAGED, FLAGDO, GOODC(4), BADC(4),
     *   DOSOME
      REAL      CATR(256), BASEN, BTIME, ETIME
      DOUBLE PRECISION CATD(128)
      INCLUDE 'ACLIP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      COMMON /FLAGS/ FLGKEY
      DATA LUN /48/
C-----------------------------------------------------------------------
      IRET = 0
      VCOUNT = 0
      FLAGED = .FALSE.
      INNUMS = INNUM
C                                       If done, just return
      IF (NUMVIS.EQ.-1) GO TO 999
C                                       Set output increments
      JNCIF = INCIF
      JNCS = INCS
C                                       Set lengths of input axes.
      NUMFRQ = ECHAN - BCHAN + 1
      NUMIF = 1
      IF (JLOCIF.GT.0) NUMIF = CATBLK(KINAX+JLOCIF)
      NOPOL = CATBLK(KINAX+JLOCS)
      LRECO = LRECU
      LENBU = 1
      INCX = CATBLK(KINAX)
C                                       Loop through data
      DO 100 I = 1,NVIS
C                                       Get a record
         CALL UVGET ('READ', BUFF1, BUFF1(1+NRPRIN), IRET)
C                                       Escape loop if no more data
C                                       for this source (IRET = -1)
         IF (IRET.EQ.-1) GO TO 110
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 1000) IRET, I
            GO TO 990
            END IF
         VCOUNT = VCOUNT + 1
C                                       Call CLIPF now
         IF (ILOCB.GE.0) THEN
            BASEN = BUFF1(1+ILOCB)
            IA1 = BASEN / 256. + 0.1
            IA2 = BASEN - IA1*256. + 0.1
         ELSE
            IA1 = BUFF1(1+ILOCA1) + 0.1
            IA2 = BUFF1(1+ILOCA2) + 0.1
            END IF
C                                      Working on visibility number
C                                      NUMVIS
         NUMVIS = NUMVIS + 1
C                                      Call filtering routine
         CALL CLIPF (NUMVIS, IA1, NA, NP, NI, MALL, MBAD, MSOM,
     *      BUFF1(1+NRPRIN), INCX, IRET)
C                                       See if flagging is necessary
         FLAGDO = .FALSE.
         JJ = ECHAN * NUMIF * NOPOL
         DO 10 J = 1,JJ
            IF (FLGKEY(J).LT.0) FLAGDO = .TRUE.
 10         CONTINUE
C                                       Need to flag, set it as true
         IF (FLAGDO) THEN
            FLAGED = .TRUE.
C                                       Get the time, set interval
            BTIME = BUFF1(1+ILOCT) - 1.E-6
            ETIME = BUFF1(1+ILOCT) + 1.E-6
C                                       Now set flagging
            GOODDT = .FALSE.
            DO 20 J = 1,JJ
               IF (FLGKEY(J).GT.0) GOODDT = .TRUE.
 20            CONTINUE
C                                       No good data, flag whole record
            IF ((.NOT.GOODDT) .AND. (DOALL)) THEN
               CALL LFILL (4, .TRUE., PFLAGS)
               CALL FLAGIT ('FLAG', LUN, INDISK, INNUM, FGVERI, FGVERO,
     *            LFGRNO, FGKOLS, FGNUMV, IDSOUR, SUBARR, FRQSEL, IA1,
     *            IA2, BTIME, ETIME, 1, 0, 1, 0, PFLAGS, REASON,
     *            APARM(10), CATUV, FGBUFL, IRET)
               IF (IRET.NE.0) GO TO 80
C                                       There are some good ones
            ELSE
               DO 70 J = 1,NUMIF
                  JX = J + BIF - 1
                  FLAGDO = .FALSE.
                  GOODDT = .FALSE.
                  DOSOME = .FALSE.
                  DO 35 K = 1,NOPOL
                     GOODC(K) = .FALSE.
                     BADC(K) = .FALSE.
                     JJ = ECHAN * NOPOL * (J-1) + ECHAN * (K-1)
                     DO 25 L = 1,ECHAN
                        IF (FLGKEY(JJ+L).GT.0) GOODC(K) = .TRUE.
                        IF (FLGKEY(JJ+L).LT.0) BADC(K) = .TRUE.
                        IF ((GOODC(K)) .AND. (BADC(K))) GO TO 30
 25                     CONTINUE
 30                  IF (BADC(K)) FLAGDO = .TRUE.
                     IF (GOODC(K)) GOODDT = .TRUE.
                     PFLAGS(K) = .NOT.GOODC(K)
                     DOSOME = DOSOME .OR. PFLAGS(K)
 35                  CONTINUE
C                                       SOME bad this IF
                  IF (FLAGDO) THEN
                     DO 60 K = 1,NOPOL
C                                       all bad corr
                        IF ((BADC(K)) .AND. (.NOT.GOODC(K))) THEN
                           CALL LCOPY (4, LFLAG(1,K), PFLAGS)
                           CALL FLAGIT ('FLAG', LUN, INDISK, INNUM,
     *                        FGVERI, FGVERO, LFGRNO, FGKOLS, FGNUMV,
     *                        IDSOUR, SUBARR, FRQSEL, IA1, IA2, BTIME,
     *                        ETIME, JX, JX, 1, 0, PFLAGS, REASON,
     *                        APARM(10), CATUV, FGBUFL, IRET)
                           IF (IRET.NE.0) GO TO 80
C                                       some were bad
                        ELSE IF ((GOODC(K)) .AND. (BADC(K))) THEN
                           CALL LCOPY (4, LFLAG(1,K), PFLAGS)
                           JE = 0
 40                        JB = JE + 1
                           IF (JB.LE.ECHAN) THEN
                              IB0 = 1000000
                              IB1 = 1000000
                              JE = 0
                              JJ = ECHAN * (NOPOL * (J-1) + (K-1))
                              DO 45 L = JB,ECHAN
                                 IF (FLGKEY(JJ+L).EQ.0) THEN
                                    IB0 = MIN (IB0, L)
                                    JE = L
                                 ELSE IF (FLGKEY(JJ+L).LT.0) THEN
                                    IB0 = MIN (IB0, L)
                                    IB1 = MIN (IB1, L)
                                    JE = L
                                 ELSE
                                    IF (IB1.LT.ECHAN) GO TO 50
                                    IB0 = 1000000
                                    END IF
 45                              CONTINUE
 50                           IF (IB1.LE.JE) THEN
                                 JB = IB0
                                 CALL FLAGIT ('FLAG', LUN, INDISK,
     *                              INNUM, FGVERI, FGVERO, LFGRNO,
     *                              FGKOLS, FGNUMV, IDSOUR, SUBARR,
     *                              FRQSEL, IA1, IA2, BTIME, ETIME, JX,
     *                              JX, JB, JE, PFLAGS, REASON,
     *                              APARM(10), CATUV, FGBUFL, IRET)
                                 IF (IRET.NE.0) GO TO 80
                                 GO TO 40
                                 END IF
                              END IF
                           END IF
 60                     CONTINUE
                     END IF
 70               CONTINUE
               END IF
C                                       Report flagging problems
 80         IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1080) IRET
               GO TO 990
               END IF
            END IF
 100     CONTINUE
C
 110  WRITE (MSGTXT,1110) VCOUNT
      CALL MSGWRT (2)
      IRET = 0
C                                       Close the flagging table if used
      IF (FLAGED) CALL FLAGIT ('CLOS', LUN, INDISK, INNUM, FGVERI,
     *   FGVERO, LFGRNO, FGKOLS, FGNUMV, IDSOUR, SUBARR, FRQSEL, IA1,
     *   IA2, BTIME, ETIME, J, J, 1, 0, PFLAGS, REASON, APARM(10),
     *   CATUV, FGBUFL, IRET)
C                                       Close input file
      CALL UVGET ('CLOS', BUFF1, BUFF1(1+NRPRIN), IRET)
C                                       Restore INNUM
      INNUM = INNUMS
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLPMOP: Error obtaining data',I3,' VIS NUM',I10)
 1080 FORMAT ('CLPMOP: Error flagging', I3)
 1110 FORMAT (I10,' visibilities processed for this source')
      END
      SUBROUTINE REPORT (NA, NP, NI, MALL, MBAD, MSOM)
C-----------------------------------------------------------------------
C   Print the reports
C   Inputs:
C      NA      I      Number of antennas
C      NP      I      Number of polarizations
C      NI      I      Number of IFs
C      MALL    I(*)   Counts samples by baseline etc
C      MBAD    I(*)   Counts full bad spectra by baseline
C      MSOM    I(*)   Counts partly bad spectra by baseline
C-----------------------------------------------------------------------
      INTEGER   NA, NP, NI, MALL(NA,NP,NI), MBAD(NA,NP,NI),
     *   MSOM(NA,NP,NI)
C
      INTEGER   I, J, K, L, TOTALA, TOTALB, MMAX, MXA, I1, I2, TOTALS,
     *   KK
      INCLUDE 'ACLIP.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   MAUX(MAXANT)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       First add the totals
      DO 90 L = 1,NI
         DO 80 KK = 1,NPOLI
            IF (INTY.GT.0) THEN
               K = KK + INTY - 1
            ELSE
               K = KK - INTY - 1
               END IF
            TOTALA = 0
            TOTALB = 0
            TOTALS = 0
            MMAX = 0
            MXA = 0
            DO 20 I = 1,NA
               IF (MALL(I,K,L).GT.0) THEN
                  MXA = MAX (MXA, I)
                  TOTALA = TOTALA + MALL(I,K,L)
                  IF (MALL(I,K,L).GT.MMAX) MMAX = MALL(I,K,L)
                  END IF
               TOTALB = TOTALB + MBAD(I,K,L)
               TOTALS = TOTALS + MSOM(I,K,L)
 20            CONTINUE
C                                       Now report on all visibilities
            IF ((TOTALA.GT.0) .AND. (TOTALB+TOTALS.GT.0)) THEN
               I = L + BIF - 1
               IF (ECHAN.LE.0) THEN
                  WRITE (MSGTXT,1020) TOTALB, TOTALA, KK, I
                  CALL MSGWRT (4)
               ELSE
                  WRITE (MSGTXT,1021) TOTALB, TOTALA, KK, I
                  CALL MSGWRT (4)
                  WRITE (MSGTXT,1022) TOTALS, TOTALA, KK, I
                  CALL MSGWRT (4)
                  END IF
               WRITE (MSGTXT,1025) MMAX
               CALL MSGWRT (4)
               I1 = 1
 25            I2 = MIN (I1+30, MXA)
               IF (I2.GE.I1) THEN
                  DO 30 J= I1,I2
                     MAUX(J) = NINT ((10. * MALL(J,K,L)) / MMAX)
 30                  CONTINUE
                  WRITE (MSGTXT,1035) (MAUX(J), J = I1,I2)
                  CALL MSGWRT (4)
                  I1 = I2 + 1
                  GO TO 25
                  END IF
               END IF
C                                       Now report percentage flagged
            IF ((TOTALA.GT.0) .AND. (TOTALB.GT.0)) THEN
               MSGTXT = 'Visibilities flagged (percent):'
               IF (ECHAN.GT.1) MSGTXT = 'Visibility spectra fully'
     *            // ' flagged (percent):'
               CALL MSGWRT (4)
               I1 = 1
 45            I2 = MIN (I1+30, MXA)
               IF (I2.GE.I1) THEN
                  DO 50 J = I1,I2
                     MAUX(J) = 0
                     IF (MALL(J,K,L).GT.0) THEN
                        MAUX(J) = NINT ((100. * MBAD(J,K,L)) /
     *                     MALL(J,K,L))
                     ELSE
                        MAUX(J) = 0
                        END IF
 50                  CONTINUE
                  WRITE (MSGTXT,1035) (MAUX(J), J = I1,I2)
                  CALL MSGWRT (4)
                  I1 = I2 + 1
                  GO TO 45
                  END IF
               END IF
            IF ((TOTALA.GT.0) .AND. (TOTALS.GT.0)) THEN
               MSGTXT = 'Visibility spectra partly flagged (percent):'
               CALL MSGWRT (4)
               I1 = 1
 65            I2 = MIN (I1+30, MXA)
               IF (I2.GE.I1) THEN
                  DO 70 J = I1,I2
                     MAUX(J) = 0
                     IF (MALL(J,K,L).GT.0) THEN
                        MAUX(J) = NINT ((100. * MSOM(J,K,L)) /
     *                     MALL(J,K,L))
                     ELSE
                        MAUX(J) = 0
                        END IF
 70                  CONTINUE
                  WRITE (MSGTXT,1035) (MAUX(J), J = I1,I2)
                  CALL MSGWRT (4)
                  I1 = I2 + 1
                  GO TO 65
                  END IF
               END IF
 80         CONTINUE
 90      CONTINUE
C                                       Report flagged correlators
      WRITE (MSGTXT,1090) NBDCOR
      CALL MSGWRT (4)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Flagged',I8,' of',I10,' correlators for corr,IF',I2,I3)
 1021 FORMAT ('Fully flagged ',I8,' of',I10,' spectra for corr,IF',
     *   I2,I3)
 1022 FORMAT ('Partly flagged',I8,' of',I10,' spectra for corr,IF',
     *   I2,I3)
 1025 FORMAT ('Visibilities per baseline (tens of percent of', I9 ,'):')
 1035 FORMAT (1X,31(I2))
 1090 FORMAT ('Flagged',I10,' correlators')
      END
      SUBROUTINE CLPMHI
C-----------------------------------------------------------------------
C   CLPMHI appends to the history file.
C-----------------------------------------------------------------------
      CHARACTER ATIME*8, ADATE*12, HILINE*72
      INTEGER   LUN, IERR, TIME(3), DATE(3), HBUFF(256), I
      INCLUDE 'ACLIP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA LUN /28/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN, DISKIN, CNOIN, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      WRITE (HILINE,1000) TSKNAM, ADATE, ATIME
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       cal adverbs
      CALL CALHIS (LUN, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Flag table out
      WRITE (HILINE,1113) TSKNAM, FGVERO
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Range of parallel
      IF ((APARM(1).LT.1.E6) .OR. (APARM(3).GT.-1.E6)) THEN
         WRITE (HILINE,1120) TSKNAM, 'P', APARM(3), APARM(1), 'parallel'
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      IF ((APARM(2).LT.1.E6) .OR. (APARM(4).GT.-1.E6)) THEN
         WRITE (HILINE,1120) TSKNAM, 'C', APARM(4), APARM(2), 'crossed'
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      IF (APARM(5).GT.0.0) THEN
         WRITE (HILINE,1121) TSKNAM, APARM(5)
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      IF (APARM(6).LT.1.E12) THEN
         WRITE (HILINE,1122) TSKNAM, APARM(6)
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      I = APARM(7) + 0.1
      IF (I.GT.0) THEN
         WRITE (HILINE,1123) TSKNAM, I
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                      Number of flagged correlators
      WRITE (HILINE,1125) TSKNAM, NBDCOR
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close HI file
 900  CALL HICLOS (LUN, .TRUE., HBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'/********* Start ',A12,2X,A8)
 1113 FORMAT (A6,'FLAGVERO =',I4,' / Flagging table written')
 1120 FORMAT (A6,A1,'RANGE=',2(1PE14.6),' / range of okay',A,'-hand')
 1121 FORMAT (A6,'WTMIN=',1PE14.6,' / weights below WTMIN were flagged')
 1122 FORMAT (A6,'WTMAX=',1PE14.6,' / weights above WTMAX were flagged')
 1123 FORMAT (A6,'EXTEND=',I3,'   / flags extended +- in channels')
 1125 FORMAT (A6,'/ Correlators flagged:', I8)
      END
      SUBROUTINE CLIPF (NUMVIS, IA1, NA, NP, NI, MALL, MBAD, MSOM, VIS,
     *   INCX, IRET)
C-----------------------------------------------------------------------
C   Routine to examine data for bad amplitudes and weights.
C   Inputs:
C      NUMVIS   I        Visibility number
C      IA1      I        First antenna number
C      NA       I        Number of antennas
C      NP       I        Number of polarizations
C      NI       I        Number of IFs
C      VIS      R(INCX,*)  Visibilities in order real, imaginary, weight
C                        (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                        NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
C      BCHAN    I        Lowest channel number.
C      ECHAN    I        Highest channel number.
C      INCSI    I        Input Stokes' increment in vis.
C      INCFI    I        Input frequency increment in vis.
C      INCIFI   I        Input IF increment in vis.
C      CATBLK   I        Catalog header block
C   Output:
C      MALL     I(*)     Counts samples by baseline etc
C      MBAD     I(*)     Counts full bad spectra by baseline
C      MSOM     I(*)     Counts partly bad spectra by baseline
C      FLGKEY   I(*)     Flagging decisions
C      IRET     I        Return code   0 => OK
C   Auxiliary variables for info
C      MALL     I        Nant x Nant x 4*Nif matrix for IF1 with all
C                        visibilities
C      MBAD     I        Nant x Nant x 4*Nif matrix for IF1 with all
C                        visibilities
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, NA, NP, NI, MALL(NA,NP,NI), MBAD(NA,NP,NI),
     *   MSOM(NA,NP,NI), INCX, IRET
      REAL      VIS(INCX,*)
C
      INCLUDE 'ACLIP.INC'
      INTEGER   CINDEX, FLGKEY(MAXCIF), LOOP, INDEX, OFF, IS, IIF, I,
     *   NFLAG, NCL, NGOOD, NBAD, JS, LS, APARM7, LL1, LL2
      REAL      AMP
      LOGICAL   GOODDT(4,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      COMMON /FLAGS/ FLGKEY
      SAVE NCL
C-----------------------------------------------------------------------
C                                       Set up on first call
      APARM7 = APARM(7) + 0.1
      IF (NUMVIS.EQ.1) THEN
         NBDCOR = 0
C                                       Zero channel info array
         IF (APARM(9).GT.0.0) THEN
            NCL = APARM(9) + 0.5
            NCL = MAX (1, NCL)
         ELSE
            NCL = -1
            END IF
         END IF
C                                       process data
      IRET = 0
      IF (NUMVIS.GT.0) THEN
         IF (MOD(NUMVIS,10000).EQ.0) THEN
            WRITE (MSGTXT,1000) NUMVIS
            CALL MSGWRT (2)
            END IF
         I = NPOLN * NIF * ECHAN
         CALL FILL (I, 0, FLGKEY)
C                                       Mark good correlators
         DO 20 IIF = 1,NIF
            DO 15 IS = 1,NPOLN
               GOODDT(IS,IIF) = .FALSE.
C                                       Offset in Stokes and IF
               OFF = (IS - 1) * INCSI + (IIF - 1) * INCIFI
C                                       Set index for info counters
               CINDEX = ECHAN * NPOLN * (IIF - 1) + ECHAN * (IS - 1)
C                                       Index for this channel, IF
               INDEX = OFF + 1
C                                       Check data
               DO 10 LOOP = BCHAN,ECHAN
                  IF (VIS(3,INDEX).GT.0.0) THEN
                     FLGKEY(CINDEX+LOOP) = 1
                     GOODDT(IS,IIF) = .TRUE.
                     END IF
                  INDEX = INDEX + INCFI
 10               CONTINUE
 15            CONTINUE
 20         CONTINUE
C                                       Now check amplitudes
         DO 40 IIF = 1,NIF
            DO 35 IS = 1,NPOLN
               NFLAG = 0
               IF (TESTIT(IS)) THEN
C                                       Offset in Stokes and IF
                  OFF = (IS - 1) * INCSI + (IIF - 1) * INCIFI
C                                       Set index for info counters
                  CINDEX = ECHAN * NPOLN * (IIF - 1) + ECHAN * (IS - 1)
C                                       Index for this channel, IF
                  INDEX = OFF + 1 + INCFI * (LBCHAN-BCHAN)
C                                       Check data
                  DO 30 LOOP = LBCHAN,LECHAN
                     IF (VIS(3,INDEX).GT.0.0) THEN
                        IF (CTYP(IS).EQ.1) THEN
                           AMP = VIS(1,INDEX)
                        ELSE
                           AMP = SQRT (VIS(1,INDEX)**2 +
     *                        VIS(2,INDEX)**2)
                           END IF
                        IF ((AMP.LT.CMIN(IS)) .OR. (AMP.GT.CMAX(IS))
     *                     .OR. (VIS(3,INDEX).LT.APARM(5)) .OR.
     *                     (VIS(3,INDEX).GT.APARM(6))) THEN
                           LL1 = MAX (LOOP-APARM7, BCHAN)
                           LL2 = MIN (LOOP+APARM7, ECHAN)
                           LL2 = LL2 - LL1 + 1
                           CALL FILL (LL2, -1, FLGKEY(CINDEX+LL1))
                           NFLAG = NFLAG + 1
                           END IF
                        END IF
                     INDEX = INDEX + INCFI
 30                  CONTINUE
                  IF ((NCL.GT.0) .AND. (NFLAG.GE.NCL)) CALL FILL (ECHAN,
     *               -1, FLGKEY(CINDEX+1))
                  END IF
 35            CONTINUE
 40         CONTINUE
C                                       Now cross check
         IF (APARM(8).LE.0.0) THEN
            DO 60 IIF = 1,NIF
               DO 55 IS = 1,NPOLN
                  IF ((TESTIT(IS)) .AND. (CTYP(IS).EQ.1)) THEN
C                                       Set index for info counters
                     CINDEX = ECHAN * NPOLN * (IIF - 1) +
     *                  ECHAN * (IS - 1)
                     DO 50 JS = 1,NPOLN
                        IF (CTYP(JS).EQ.2) THEN
C                                       Check data
                           INDEX = ECHAN * NPOLN * (IIF - 1) +
     *                        ECHAN * (JS - 1)
                           NFLAG = 0
                           DO 45 LOOP = BCHAN,ECHAN
                              IF (FLGKEY(CINDEX+LOOP).EQ.-1)
     *                           FLGKEY(INDEX+LOOP) = -1
                              IF (FLGKEY(INDEX+LOOP).EQ.-1)
     *                           NFLAG = NFLAG + 1
 45                           CONTINUE
                           IF ((NCL.GT.0) .AND. (NFLAG.GE.NCL))
     *                        CALL FILL (ECHAN, -1, FLGKEY(INDEX+1))
                           END IF
 50                     CONTINUE
                     END IF
 55               CONTINUE
 60            CONTINUE
            END IF
C                                       Now COUNT
         DO 80 IIF = 1,NIF
            DO 75 IS = 1,NPOLN
               NGOOD = 0
               NBAD = 0
               NFLAG = 0
               IF (GOODDT(IS,IIF)) THEN
C                                       Set index for info counters
                  CINDEX = ECHAN * NPOLN * (IIF - 1) + ECHAN * (IS - 1)
C                                       Check flags
                  DO 70 LOOP = BCHAN,ECHAN
                     IF (FLGKEY(CINDEX+LOOP).EQ.1) THEN
                        NGOOD = NGOOD + 1
                     ELSE IF (FLGKEY(CINDEX+LOOP).EQ.-1) THEN
                        NFLAG = NFLAG + 1
                        NBDCOR = NBDCOR + 1
                     ELSE
                        NBAD = NBAD + 1
                        END IF
 70                  CONTINUE
                  DO 71 LS = 1,4
                     MALL(IA1,LS,IIF) = MALL(IA1,LS,IIF) + FLGIT(LS,IS)
                     IF (NGOOD.LE.0) MBAD(IA1,LS,IIF) = MBAD(IA1,LS,IIF)
     *                  + FLGIT(LS,IS)
                     IF ((NGOOD.GT.0) .AND. (NFLAG.GT.0))
     *                  MSOM(IA1,LS,IIF) = MSOM(IA1,LS,IIF)
     *                  + FLGIT(LS,IS)
 71                  CONTINUE
                  END IF
 75            CONTINUE
 80         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Working on visibility record ', I8)
      END
      SUBROUTINE FLAGIT (OPCODE, LUN, DISK, CNO, VERI, VER, LFGRNO,
     *   FGKOLS, FGNUMV, ID, SUBA, FQID, ANT1, ANT2, BTIME, ETIME, BIF,
     *   EIF, BCHAN, ECHAN, PFLAGS, REASON, DOIFS, CATUV, BUFF, IRET)
C-----------------------------------------------------------------------
C   Updates the Flag (FG) table. Adapted from FLAGUP
C   One entry is made indicating a visibility to be rejected.
C   The FLAG table will be opened on the first call but a final call
C   with OPCODE='CLOS' is required to close the file.
C   Inputs:
C      OPCODE   C*4      Operation desired, 'CLOS'=>close file
C                        Anything else = 'FLAG'
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VERI     I        Input version number
C      VER      I        FG file version
C      LUN      I        Logical unit number to use
C      ID       I(NID)   List of source ID as defined in SOURCE table
C      NID      I        Number of elements in ID
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
C      DOIFS    R        > 0 flag all IFs
C   Input/Output:
C      CATUV    I(256)   Header for disk file to get FG table
C      BUFF     I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      LFGRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      FGKOLS   I(*)     The column pointer array in order, SOURCE,
C                        SUBARRAY, ANTS, TIMERANG, IFS, CHANS, PFLAGS,
C                        REASON
C      FGNUMV   I(*)     Element count in each column.
C   Output:
C      IRET     I        Error code, 0=>OK else TABIO error.
C                        Note: -1 => read, but record deselected.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, REASON*24
      INTEGER   LUN, DISK, CNO, VERI, VER, LFGRNO, FGKOLS(*), FGNUMV(*),
     *   ID, SUBA, FQID, ANT1, ANT2, BIF, EIF, BCHAN, ECHAN, CATUV(256),
     *   BUFF(*), IRET
      REAL      BTIME, ETIME, DOIFS
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
         IF (DOIFS.LE.0.0) THEN
            IFS(1) = BIF
            IFS(2) = EIF
         ELSE
            IFS(1) = 1
            IFS(2) = 0
            END IF
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

LOCAL INCLUDE 'DEFLG.INC'
C                                       Local include for DEFLG
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   SEQIN, CNOIN, DISKIN, JBUFSZ, LRECC, NBDCOR, NRPRMI,
     *   INCSI, INCFI, INCIFI, FGVERO, LBCHAN, LECHAN, IANT(50),
     *   IBAS(50), NANTS, NBASS, CTYP(4), INTY, OUTY, NANT, NIF, NPOLN,
     *   CHNSEL(3,20,MAXIF), FITWTS(MAXCIF), NCHAN, CATS(256), DISKX,
     *   CNOX, LRECO, FGVERI
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC
      CHARACTER NAMEIN*12, CLAIN*6, XCALCO*4, XSOUR(30)*16, REASON*24
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XBIF, XEIF, XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XFGOUT, XDOBND, XBPVER, XSMOTH(3), XANT(50), XBAS(50),
     *   XUVR(2), XCHNS(4,20), DOSTOK, DOALLS, ICUT, SOLINT, DETIME,
     *   XBADD(10),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), CMIN(4), CMAX(4)
      LOGICAL   DOUVCM, DESEL, DOBLIN, TESTIT(4), FLAGP(4,4)
      INTEGER   NSOU
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XQUAL,
     *   XXCALC, XTIME, XBAND, XFREQ, XFQID, XBIF, XEIF, XSUBA, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XFGOUT, XDOBND, XBPVER,
     *   XSMOTH, XANT, XBAS, XUVR, XCHNS, DOSTOK, DOALLS, ICUT, SOLINT,
     *   DETIME, XBADD
      COMMON /INFO/ CATS, FGVERO, NSOU, SEQIN, DISKIN, CNOIN, LRECC,
     *   DOUVCM, NRPRMI, INCSI, INCFI, INCIFI, NBDCOR, LBCHAN, LECHAN,
     *   IANT, IBAS, NANTS, NBASS, DESEL, DOBLIN, TESTIT, CMIN, CMAX,
     *   CTYP, INTY, OUTY, FLAGP, NANT, NIF, NPOLN, CHNSEL,
     *   FITWTS, NCHAN, DISKX, CNOX, LRECO, FGVERI
      COMMON /CHRCOM/ NAMEIN, CLAIN, XCALCO, XSOUR, REASON
LOCAL END
      PROGRAM DEFLG
C-----------------------------------------------------------------------
C! Applies calibration and/or editing and flags based on decorrelation
C# task editing UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2001-2003, 2005-2007, 2010-2012, 2015-2016
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
      INCLUDE 'DEFLG.INC'
      INCLUDE 'INCS:DSEL.INC'
C
      CHARACTER PRGM*6
      LOGICAL   DOWANT
      INTEGER   NUMSOU, IRET, I, SULIST(XSTBSZ), MALL(2), MBAD(2),
     *   NWORDS
      LONGINT   OFFALL, OFFBAD
C     INTEGER   MALL(MAXANT,MAXANT,4*MAXIF),
C    *   MBAD(MAXANT,MAXANT,4*MAXIF)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'DEFLG '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL DEFLIN (PRGM, DOWANT, NUMSOU, SULIST, IRET)
C                                       Allocate memory
      NWORDS = NANT * NANT * NPOLN * NIF
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MALL, OFFALL,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MBAD, OFFBAD,
     *   IRET)
      IF (IRET.NE.0) THEN
         CALL FILL (NWORDS, 0, MALL(1+OFFALL))
         CALL FILL (NWORDS, 0, MBAD(1+OFFBAD))
         END IF
      NBDCOR = 0
C                                       Loop over sources.
      IF (IRET.EQ.0) CALL DEFLUV (NANT, NPOLN, NIF, MALL(1+OFFALL),
     *   MBAD(1+OFFBAD), DOWANT, NUMSOU, SULIST, IRET)
C                                       report results
      IF (IRET.EQ.0) CALL REPORT (NANT, NPOLN, NIF, MALL(1+OFFALL),
     *   MBAD(1+OFFBAD))
C                                       Report deeds to History file
      IF (IRET.EQ.0) CALL DEFLHI
C                                       Done: Close down files, etc.
      CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, MALL, OFFALL, I)
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE DEFLIN (PRGN, DOWANT, NUMSOU, SULIST, JERR)
C-----------------------------------------------------------------------
C   DEFLIN gets input parameters for DEFLG, finds input file and
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
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH CATH(256)
      CHARACTER STAT*4, UTYPE*2, ATIME*8, ADATE*12
      INTEGER   NPARM, IROUND, IERR, I, LUN, TIME(3), DATE(3), NVER,
     *   NUMAN(513), J, JJ, K1, K2, K, IOFF, NW(MAXIF)
      REAL      CATR(256)
      LOGICAL   MATCH
      INCLUDE 'DEFLG.INC'
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
      NPARM = 352
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
      NPOLN = CATBLK(KINAX+JLOCS)
      NPOLN = MIN (2, NPOLN)
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
C                                       Set BCHAN and ECHAN to all
      BCHAN = 1
      ECHAN = CATBLK(KINAX+JLOCF)
      NCHAN = ECHAN - BCHAN + 1
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
      NIF = EIF - BIF + 1
C                                       Setup calibration choices
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      BLVER = IROUND (XBLVER)
      DOUVCM = .FALSE.
      UVRNG(1) = XUVR(1)
      UVRNG(2) = XUVR(2)
      CALL FILL (50, 0, ANTENS)
      SUBARR = IROUND (XSUBA)
      IF ((ICUT.LE.0.0) .OR. (ICUT.GT.1.0)) ICUT = 0.5
      IF (SOLINT.LE.0.0) SOLINT = 60.
      SOLINT = SOLINT / 86400.0
      DETIME = DETIME / 86400.0
C                                       Crunch Antenna adverbs
      CALL SETANT (50, XANT, XBAS, NANTS, NBASS, IANT, IBAS, DESEL)
      DOBLIN = (NANTS.GT.0) .OR. (NBASS.GT.0)
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
C                                       use inner 3/4
 45    DO 60 K = BIF,EIF
          IOFF = (K - BIF) * NCHAN + 1 - BCHAN
          IF (NW(K).LE.0) THEN
             NW(K) = 1
             CHNSEL(1,1,K) = (NCHAN+7) / 8
             CHNSEL(2,1,K) = NCHAN - (NCHAN/8)
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
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DEFLIN: error',I3,' obtaining input parameters')
 1030 FORMAT ('Error',I3,' finding ',A12,'.',A6,'.',I4,' disk =',
     *   I3,' user=',I5)
 1035 FORMAT ('Error',I3,' obtaining CATBLK ')
      END
      SUBROUTINE DEFLUV (NA, NP, NI, MALL, MBAD, DOWANT, NUMSOU, SULIST,
     *   IRET)
C-----------------------------------------------------------------------
C   DEFLUV uses UVGET to obtain data as single source files and DEFLOP
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
C      IRET     I      Return code, 0 => OK, otherwise abort.
C   Auxiliary:
C     FGVERO    I      Version number of new flag table (reset later)
C-----------------------------------------------------------------------
      LOGICAL   DOWANT
      INTEGER   NA, NP, NI, MALL(NA,NA,NP,NI), MBAD(NA,NA,NP,NI),
     *   NUMSOU, SULIST(*), IRET
C
      INCLUDE 'DEFLG.INC'
      CHARACTER VELTYP*8, VELDEF*8, CALCOD*4
      LOGICAL   DOAVG, DOAPPT, NOSUB, SINGLE, TABLE, EXIST, FITASC,
     *   FIRST
      INTEGER   NUMVIS, SOUCUR, MAXSOU, SLOOP, DPOSAV, LRECU,
     *   IERR, SUKOLS(MAXSUC), SUNUMV(MAXSUC), SBUFF(512), I, SLUN,
     *   IDSOU, QUAL, INOGRP, SUB, NUMSUB, LIMS1, LIMS2, SUBTMP, SAVBND,
     *   INCX, ICOR, IROUND, ISIZE, DISKY, CNOY, NWAY, LUNST(2,2),
     *   PRNVIS, TFRW(16), PDVSAV, SUFQID
      REAL      RPARM, VIS, CATR(256), OLDRP, TEMP
      DOUBLE PRECISION  BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, CATD(128)
      DOUBLE PRECISION   LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      REAL     FLUX(4,MAXIF)
      SAVE PRNVIS, CNOY
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA NWAY, LUNST /2, 40,41,42,43/
C-----------------------------------------------------------------------
      PRNVIS = 0
      DOAPPT = DOAPPL
      DOAVG = .FALSE.
      NOSUB = .FALSE.
      OLDRP = CATR(KRCRP+JLOCF)
C                                       Zero visibility count
      NUMVIS = 0
C                                       Zero number of processed sources
      NSOU = 0
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
C                                       Quit if single source file
      IF (SINGLE) THEN
         MSGTXT = 'Single-source file, use UVLIN'
         IRET = 1
         GO TO 990
         END IF
C                                       Open source table
      CALL SOUINI ('READ', SBUFF, FVOL(1), FCNO(1), 1, CATUV, SLUN,
     *   INOGRP, VELTYP, VELDEF, SUFQID, SOUCUR, SUKOLS, SUNUMV, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'Error opening the source table'
         GO TO 990
         END IF
C                                       Report number of sources
      MAXSOU = SBUFF(5)
      WRITE (MSGTXT,1000) MAXSOU
      CALL MSGWRT (2)
C                                       Loop here over sources
      DISKY = 0
      CNOY = 0
      DO 200 SLOOP = 1,MAXSOU
C                                       Save SLOOP from TABSOU
         SOUCUR = SLOOP
C                                       Read source table
         CALL TABSOU ('READ', SBUFF, SOUCUR, SUKOLS, SUNUMV, IDSOU,
     *      SOURCS, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, LSRVEL, RESTFQ, PMRA, PMDEC, IRET)
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
 110           CONTINUE
            IF (DOWANT) GO TO 200
            END IF
C                                       Setup header for calibrated
C                                       single-source file, save input
C                                       header
 120     SUBARR = SUBTMP
         DPOSAV = DOPOL
         PDVSAV = PDVER
         DOPOL = 0
         SAVBND = DOBAND
         DOBAND = 0
C                                       Report source it is working on
         IF (SLOOP.GT.1) THEN
            WRITE (MSGTXT,1119) NBDCOR
            CALL MSGWRT (2)
            END IF
         WRITE (MSGTXT,1120) SOUCUR
         CALL MSGWRT (2)
C                                       Save this source's name
         NSOU = NSOU + 1
         XSOUR(NSOU) = SOURCS(1)
C                                       Initialize single-source header
         CALL UVGET ('INIT', RPARM, VIS, IERR)
         IF (IERR.NE.0) GO TO 190
         OUTY = ICOR0
         DOPOL = DPOSAV
         PDVER = PDVSAV
         DOAPPL = .FALSE.
         DOBAND = SAVBND
         CLVER = CLUSE
C                                        For all stokes correlators
         DO 130 I = 1,NCOR
            TEMP = CATD(KDCRV+JLOCS) + (I-CATR(KRCRP+JLOCS)) *
     *         CATR(KRCIC+JLOCS)
            ICOR = IROUND (TEMP)
            CTYP(I) = 0
C                                        Stokes type, I
            IF (ICOR.EQ.1) CTYP(I) = 1
C                                        Stokes type, Q (?)
            IF ((ICOR.GT.1) .AND. (ICOR.LE.4)) CTYP(I) = 2
C                                        Stokes type  RR or LL
            IF ((ICOR.EQ.-1) .OR. (ICOR.EQ.-2)) CTYP(I) = 1
C                                        Stokes type  RL or LR
            IF ((ICOR.EQ.-3) .OR. (ICOR.EQ.-4)) CTYP(I) = 2
 130        CONTINUE
C                                        flag pattern
         CALL LFILL (16, .FALSE., FLAGP)
         IF (OUTY.GT.0) THEN
            CALL LFILL (4, .TRUE., FLAGP(1,1))
            IF (STOKES.EQ.'IQUV') STOKES = 'I'
         ELSE IF (ICOR0.EQ.-2) THEN
            CALL LFILL (3, .TRUE., FLAGP(2,1))
            CALL LFILL (3, .TRUE., FLAGP(2,2))
         ELSE
            CALL LFILL (4, .TRUE., FLAGP(1,1))
            FLAGP(2,1) = .FALSE.
            CALL LFILL (3, .TRUE., FLAGP(2,2))
            END IF
C                                       Close again - only need header
C                                       for sum of subarrays.
         CALL UVGET ('CLOS', RPARM, VIS, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       Get values of data increments
         INCX = CATBLK(KINAX)
         NRPRMI = NRPARM
         INCSI = INCS / INCX
         INCFI = INCF / INCX
         INCIFI = INCIF / INCX
C                                       Make sure there is data
         IF (CATBLK(KIGCN).LE.0) GO TO 190
C                                       Delete old too small scratch
         IF ((PRNVIS.GT.0) .AND. (NVIS.GT.PRNVIS)) THEN
            CALL FILL (NSCR, 2, TFRW)
            CALL MAPCLR (NSCR, SCRVOL, SCRCNO, TFRW, BUFF2)
            PRNVIS = 0
            NSCR = 0
            END IF
C                                       Create scratch
         FIRST = PRNVIS.LT.NVIS
         IF (FIRST) THEN
            CALL COPY (256, CATBLK, CATS)
            CATS(KINAX) = 3
            CATS(KINAX+JLOCF) = 2
            CATS(KINAX+JLOCS) = NPOLN
            LRECO = NPOLN * 6 * NIF + NRPARM
            CALL UVSIZE (LRECO, NVIS+100, ISIZE)
            CALL SCREAT (ISIZE, BUFF2, IRET)
            IF (IRET.NE.0) GO TO 999
            DISKX = 0
            CNOX = NSCR
            CNOY = 0
            PRNVIS = NVIS
            END IF
C                                       Loop over subarrays.
         DO 150 SUB = LIMS1, LIMS2
            SUBARR = SUB
C                                       Initialize reading data
            CALL UVGET ('INIT', RPARM, VIS, IERR)
            IF (IERR.NE.0) GO TO 190
C
            LRECU = LREC
C                                       Read in and pre-average to SC
            CALL DEFLCO (NUMVIS, IERR)
C                                       Sort
            IF (NUMVIS.GT.0) THEN
               CATS(KIGCN) = NUMVIS
               CALL UVSORT (DISKX, CNOX, DISKY, CNOY, 'BT', 0.0, CATS,
     *            NWAY, LUNST, JBUFSZ, BUFF1, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 190
C                                       flag sorted file
C                                       UVSORT gets pseudo-AP
C                                       DEFLOP uses that memory
               CALL DEFLOP (NUMVIS, NA, NP, NI, MALL, MBAD, CNOY,
     *            SOUCUR, IERR)
               IF (IERR.NE.0) GO TO 190
               END IF
 150        CONTINUE
         GO TO 200
C                                       Error, close input then die
 190     CALL UVGET ('CLOS', RPARM, VIS, IERR)
         MSGTXT = 'PROBLEM WITH SOURCE: ' // SOURCS(1)
         IRET = 1
         GO TO 990
C
 200     CONTINUE
C                                       Close source table
      CALL TABIO ('CLOS', 1, SOUCUR, SBUFF, SBUFF, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X, I3, ' sources in this file')
 1119 FORMAT ('   Total correlators flagged so far',I12)
 1120 FORMAT ('Working on source number', I3)
      END
      SUBROUTINE DEFLCO (NUMVIS, IRET)
C-----------------------------------------------------------------------
C   DEFCLO reads the input file with UVGET, averages spectral channels
C   in CHNSEL, writes SC file
C   Output:
C      NUMVIS   I   Number of vis copied
C      IRET     I   Error
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
C
      INCLUDE 'INCS:DSEL.INC'
      REAL      RPARM(20)
      INTEGER   VOL, CNO, LUN, IND, LENBU, VO ,BO, BIND, BASEN, IA1,
     *   IA2, JNDX, INDX, IOFF, IPTRO, NIOUT, NIOLIM, IIS, IIF, IC
      LOGICAL   REQBAS
      CHARACTER PHNAME*48
      INCLUDE 'DEFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA LUN /40/
C-----------------------------------------------------------------------
      NUMVIS = 0
C                                       open output
      VOL = SCRVOL(CNOX)
      CNO = SCRCNO(CNOX)
      CALL ZPHFIL ('SC', VOL, CNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUN, IND, VOL, PHNAME, .TRUE., .TRUE., .TRUE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'SCRATCH'
         GO TO 990
         END IF
C                                       init output
      LENBU = 0
      VO = 0
      BO = 1
      CALL UVINIT ('WRIT', LUN, IND, NVIS, VO, LRECO, LENBU, JBUFSZ,
     *   BUFF2, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'SCRATCH'
         GO TO 990
         END IF
      NIOUT = 0
      NIOLIM = LENBU
      IPTRO = BIND
C                                       read loop
 10   CALL UVGET ('READ', RPARM, BUFF1, IRET)
      IF (IRET.GE.0) THEN
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ', 'INPUT'
            GO TO 990
            END IF
C                                       Call DEFLGF now
         IF (ILOCB.GE.0) THEN
            BASEN = RPARM(1+ILOCB) + 0.01
            IA1 = BASEN / 256
            IA2 = BASEN - IA1*256
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.01
            IA2 = RPARM(1+ILOCA2) + 0.01
            END IF
C                                       If selecting by baseline
         IF (DOBLIN) THEN
            IF (.NOT.REQBAS (IA1, IA2, DESEL, IANT, NANTS, IBAS, NBASS))
     *         GO TO 10
            END IF
C                                      Working on visibility number
C                                      NUMVIS
         NUMVIS = NUMVIS + 1
         CALL RCOPY (NRPARM, RPARM, BUFF2(IPTRO))
C                                       sum over channels
         DO 50 IIF = 1,NIF
            IOFF = (IIF - 1) * NCHAN
            DO 40 IIS = 1,NPOLN
               JNDX = 6 * ((IIS - 1) + (IIF - 1) * NPOLN) + IPTRO +
     *            NRPARM
               CALL RFILL (6, 0.0, BUFF2(JNDX))
               INDX = (IIS - 1) * INCS + (IIF - 1) * INCIF + 1
               DO 30 IC = 1,NCHAN
                  IF ((BUFF1(INDX+2).GT.0.0) .AND.
     *               (FITWTS(IC+IOFF).GT.0)) THEN
                     BUFF2(JNDX) = BUFF2(JNDX) +
     *                  BUFF1(INDX+2)*BUFF1(INDX)
                     BUFF2(JNDX+1) = BUFF2(JNDX+1) +
     *                  BUFF1(INDX+2)*BUFF1(INDX+1)
                     BUFF2(JNDX+2) = BUFF2(JNDX+2) + BUFF1(INDX+2)
                     BUFF2(JNDX+3) = BUFF2(JNDX+3) + BUFF1(INDX+2) *
     *                  SQRT (BUFF1(INDX)**2 + BUFF1(INDX+1)**2)
                     END IF
 30               CONTINUE
               IF (BUFF2(JNDX+2).GT.0.0) THEN
                  BUFF2(JNDX) = BUFF2(JNDX)/BUFF2(JNDX+2)
                  BUFF2(JNDX+1) = BUFF2(JNDX+1)/BUFF2(JNDX+2)
                  BUFF2(JNDX+3) = BUFF2(JNDX+3)/BUFF2(JNDX+2)
                  BUFF2(JNDX+4) = 0.0
                  BUFF2(JNDX+5) = BUFF2(JNDX+2)
                  END IF
 40            CONTINUE
 50         CONTINUE
C                                       time to write ?
         IPTRO = IPTRO + LRECO
         NIOUT = NIOUT + 1
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUN, IND, BUFF2, NIOLIM, BIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRIT', 'SCRATCH'
               GO TO 990
               END IF
            IPTRO = BIND
            NIOUT = 0
            END IF
         GO TO  10
         END IF
C                                       All read
      CALL UVGET ('CLOS', RPARM, BUFF1, IRET)
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUN, IND, BUFF2, NIOUT, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FLUSH', 'SCRATCH'
         GO TO 990
         END IF
      CALL ZCLOSE (LUN, IND, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DEFLCO: ERROR',I5,1X,A,'ING ',A,' FILE')
      END
      SUBROUTINE DEFLOP (NUMVIS, NA, NP, NI, MALL, MBAD, INNUM, IDSOUR,
     *   IRET)
C-----------------------------------------------------------------------
C   DEFLOP calls DEFLGF to do fitting and raise the flags which are
C   entered upon return from DEFLGF
C   Input:
C      NUMVIS   I      Number of visibilities previously processed
C      NA       I      Number of antennas
C      NP       I      Number of polarizations
C      NI       I      Number of IFs
C      INNUM    I      DFIL SC file pointer for sorted input
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
C      IRET     I      Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, NA, NP, NI, MALL(NA,NA,NP,NI),
     *   MBAD(NA,NA,NP,NI), INNUM, IDSOUR, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   K, LENBU, IA1, IA2, J1, J2, I1, II, VOL, CNO, LUN, IND,
     *   NC, APSIZ, NS, VO, BO, NAP, INIO, BIND, IPTR, JA1, JA2, JM, K1,
     *   TYPE, BASEN, IAP, JAP
      LONGINT   APP
      LOGICAL   GETDAT
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128)
      CHARACTER PHNAME*48
      INCLUDE 'DEFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DAPC.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA LUN /40/
C-----------------------------------------------------------------------
      IRET = 0
C                                       If done, just return
      IF (NUMVIS.LE.0) GO TO 999
      APSIZ = 1024 * PSAPNW
C                                       open output
      VOL = SCRVOL(INNUM)
      CNO = SCRCNO(INNUM)
      CALL ZPHFIL ('SC', VOL, CNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUN, IND, VOL, PHNAME, .TRUE., .TRUE., .TRUE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', 'SORTED'
         GO TO 990
         END IF
C                                       init output
      LENBU = 0
      VO = 0
      BO = 1
      CALL UVINIT ('READ', LUN, IND, NUMVIS, VO, LRECO, LENBU, JBUFSZ,
     *   BUFF1, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', 'SORTED'
         GO TO 990
         END IF
      I1 = 1
      INIO = 1
      IA1 = 0
      APP = PSAPOF
      NC = NPOLN * 6 * NIF
      NS = NC + 1
      NAP = 0
      GETDAT = .TRUE.
C                                       Read loop
 100  CONTINUE
C                                       Read vis. record.
         IF (GETDAT) THEN
            CALL UVDISK ('READ', LUN, IND, BUFF1, INIO, BIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ', 'SORTED'
               GO TO 990
               END IF
C                                       Out of data?
            IF (INIO.LE.0) GO TO 120
            I1 = 1
            IPTR = BIND
            END IF
C                                       Loop through buffer
         DO 110 II = I1,INIO
            IF (ILOCB.GE.0) THEN
               BASEN = BUFF1(IPTR+ILOCB) + 0.01
               JA1 = BASEN / 256
               JA2 = BASEN - JA1*256
            ELSE
               JA1 = BUFF1(IPTR+ILOCA1) + 0.01
               JA2 = BUFF1(IPTR+ILOCA2) + 0.01
               END IF
            IF ((IA1.EQ.0) .OR. (APP.EQ.PSAPOF)) THEN
               IA1 = JA1
               IA2 = JA2
               APP = PSAPOF
               END IF
            IF ((IA1.EQ.JA1) .AND. (IA2.EQ.JA2)) THEN
               NAP = NAP + 1
               APCORE(APP) = BUFF1(IPTR+ILOCT)
               JAP = IPTR + NRPARM - 1
               DO 105 IAP = 1,NC
                  APCORE(APP+IAP) = BUFF1(JAP+IAP)
 105              CONTINUE
               APP = APP + NS
               IPTR = IPTR + LRECO
               IF (APP.GE.APSIZ-NS) THEN
                  I1 = II
                  GETDAT = II.LT.INIO
                  GO TO 120
                  END IF
            ELSE
               I1 = II
               GETDAT = .FALSE.
               GO TO 120
               END IF
 110        CONTINUE
         GETDAT = .TRUE.
         GO TO 100
C                                       Baseline now fully loaded
 120  JM = 0
C                                       Find next scan break
 150  J1 = JM + 1
      IF (DETIME.LE.0.0) THEN
         JM = NAP
      ELSE
         K1 = JM + 1
         APP = (K1-1)*NS + PSAPOF
         DO 160 K = K1,NAP-1
            IF (APCORE(APP+NS)-APCORE(APP).GT.DETIME) THEN
               JM = K
               GO TO 161
               END IF
            APP = APP + NS
 160        CONTINUE
         JM = NAP
         END IF
 161  TYPE = 1
C                                       Find end of solint
 165  J2 = J1
      APP = J1 * NS + PSAPOF
      II = APP - NS
      DO 170 K = J1+1,JM
         IF (APCORE(APP)-APCORE(II).GT.SOLINT) GO TO 175
         J2 = K
         APP = APP + NS
 170     CONTINUE
C                                       flag set
 175  IF (J2.EQ.JM) TYPE = 2 + TYPE
      CALL DEFLDO (TYPE, NA, NP, NI, MALL, MBAD, IDSOUR, IA1, IA2, J1,
     *   J2, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       loop in scan
      TYPE = 0
      J1 = J1 + 1
      IF (J2.LT.JM) GO TO 165
C                                       loop for new scan
      IF (JM.LT.NAP) GO TO 150
C                                       loop for new data
      NAP = 0
      APP = PSAPOF
      IA1 = 0
      IF (INIO.GT.0) GO TO 100
C                                       DONE!
      CALL ZCLOSE (LUN, IND, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DEFLOP: ERROR',I5,1X,A,'ING ',A,' FILE')
      END
      SUBROUTINE DEFLDO (TYPE, NA, NP, NI, MALL, MBAD, IDSOUR, IA1, IA2,
     *   J1, J2, IRET)
C-----------------------------------------------------------------------
C   DEFLDO does the averaging and writing of flags
C   Input:
C      TYPE     I      0 flag central, 1 flag 1st half, 2 flag 2nd half,
C                      3 flag full
C      NA       I      Number of antennas
C      NP       I      Number of polarizations
C      NI       I      Number of IFs
C      IDSOUR   I      Source number
C      IA1      I      Antenna 1 number
C      IA2      I      Antenna 2 number
C      J1       I      First vis number in AP this interval
C      J2       I      Last vis number in AP
C   Output:
C      MALL     I(*)   Counts samples by baseline etc
C      MBAD     I(*)   Counts full bad spectra by baseline
C      IRET     I      Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   TYPE, NA, NP, NI, MALL(NA,NA,NP,NI), MBAD(NA,NA,NP,NI),
     *   IDSOUR, IA1, IA2, J1, J2, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LUNF, J, FGBUFL(512), LFGRNO, IIF, IIS, NS, NF, LDSOUR
      LONGINT   APP
      LOGICAL   PFLAGS(4), FLAGED, FLAGDO, FLGIT(2,MAXIF), FLAGAL, OKAY
      REAL      CATR(256), BTIME, ETIME
      DOUBLE PRECISION CATD(128), RS, IS, AS, WS
      INCLUDE 'DEFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INCLUDE 'INCS:DAPC.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA LUNF /48/
C-----------------------------------------------------------------------
      LDSOUR = IDSOUR
      IF (DOALLS.GT.0.0) LDSOUR = 0
      NS = 6 * NPOLN * NIF + 1
      FLAGDO = .FALSE.
      FLAGAL = .TRUE.
C                                       Counts
      J = (J2 - J1) / 2 + J1
      IF (TYPE.EQ.0) THEN
         NF = 1
      ELSE IF (TYPE.EQ.1) THEN
         NF = J - J1 + 1
      ELSE IF (TYPE.EQ.2) THEN
         NF = J2 - J + 1
      ELSE IF (TYPE.EQ.3) THEN
         NF = J2 - J1 + 1
         END IF
C                                       Do sums
      DO 30 IIF = 1,NIF
         FLGIT(1,IIF) = .TRUE.
         FLGIT(2,IIF) = .TRUE.
         DO 20 IIS = 1,NPOLN
            RS = 0.0D0
            IS = 0.0D0
            AS = 0.0D0
            WS = 0.0D0
            APP = (J1 - 1) * NS + 6 * ((IIS-1) + (IIF-1) * NPOLN) + 1
     *         + PSAPOF
            DO 10 J = J1,J2
               IF (APCORE(APP+2).GT.0.0) THEN
                  RS = RS + APCORE(APP) * APCORE(APP+2)
                  IS = IS + APCORE(APP+1) * APCORE(APP+2)
                  AS = AS + APCORE(APP+3) * APCORE(APP+2)
                  WS = WS + APCORE(APP+2)
                  END IF
               APP = APP + NS
 10            CONTINUE
            IF ((AS.GT.0.0D0) .AND. (WS.GT.0.0D0)) THEN
               AS = SQRT (RS*RS + IS*IS) / AS
               MALL(IA1,IA2,IIS,IIF) = MALL(IA1,IA2,IIS,IIF) + NF
            ELSE
               AS = 0.0D0
               END IF
            FLGIT (IIS,IIF) = (AS.LT.ICUT)
            IF (FLGIT (IIS,IIF)) THEN
               FLAGDO = .TRUE.
            ELSE
               FLAGAL = .FALSE.
               END IF
 20         CONTINUE
 30      CONTINUE
      FLAGED = .FALSE.
C                                       Need to flag, set it as true
      IF (FLAGDO) THEN
         DO 40 IIF = 1,NIF
            IF (DOSTOK.GT.0.0) THEN
               IF (FLGIT(1,IIF)) FLGIT(2,IIF) = .TRUE.
               IF ((NPOLN.GT.1) .AND. (FLGIT(2,IIF))) FLGIT(1,IIF) =
     *            .TRUE.
               END IF
            DO 35 IIS = 1,NPOLN
               IF (FLGIT(IIS,IIF)) THEN
                  MBAD(IA1,IA2,IIS,IIF) = MBAD(IA1,IA2,IIS,IIF) + NF
                  NBDCOR = NBDCOR + NF * NCHAN
                  END IF
 35            CONTINUE
 40         CONTINUE
         FLAGED = .TRUE.
         J = (J2 - J1 + 2) / 2 + J1
         APP = (J - 1) * NS + PSAPOF
C                                       Get the time, set interval
         IF (TYPE.EQ.0) THEN
            BTIME = APCORE(APP) - 1.D-6
            ETIME = APCORE(APP) + 1.D-6
            NF = 1
         ELSE IF (TYPE.EQ.1) THEN
            NF = J - J1 + 1
            ETIME = APCORE(APP) + 1.D-6
            APP = (J1 - 1) * NS + 1
            BTIME = APCORE(APP) - 1.D-6
         ELSE IF (TYPE.EQ.2) THEN
            NF = J2 - J + 1
            BTIME = APCORE(APP) - 1.D-6
            APP = (J2 - 1) * NS + 1
            ETIME = APCORE(APP) + 1.D-6
         ELSE
            NF = J2 - J1 + 1
            APP = (J1 - 1) * NS + 1
            BTIME = APCORE(APP) - 1.D-6
            APP = (J2 - 1) * NS + 1
            ETIME = APCORE(APP) + 1.D-6
            END IF
C                                       Now set flagging
C                                       No good data, flag whole record
         IF (FLAGAL) THEN
            CALL LFILL (4, .TRUE., PFLAGS)
            CALL FLAGIT ('FLAG', LUNF, DISKIN, CNOIN, FGVERI, FGVERO,
     *         LFGRNO, FGKOLS, FGNUMV, LDSOUR, SUBARR, FRQSEL, IA1, IA2,
     *         BTIME, ETIME, 1, 0, 1, 0, PFLAGS, REASON, CATUV,
     *         FGBUFL, IRET)
            IF (IRET.NE.0) GO TO 80
C                                       There are some good ones
         ELSE
            DO 50 IIF = 1,NUMIF
               OKAY = .TRUE.
               IF ((FLGIT(1,IIF)) .AND. (FLGIT(2,IIF))) THEN
                  CALL LFILL (4, .TRUE., PFLAGS)
                  OKAY = .FALSE.
               ELSE IF (FLGIT(1,IIF)) THEN
                  CALL LCOPY (4, FLAGP(1,1), PFLAGS)
                  IF (DOSTOK.GT.0.0) CALL LFILL (4, .TRUE., PFLAGS)
                  OKAY = .FALSE.
               ELSE IF ((NPOLN.GT.1) .AND. (FLGIT(2,IIF))) THEN
                  CALL LCOPY (4, FLAGP(1,2), PFLAGS)
                  IF (DOSTOK.GT.0.0) CALL LFILL (4, .TRUE., PFLAGS)
                  OKAY = .FALSE.
                  END IF
               IF (.NOT.OKAY) THEN
                  CALL FLAGIT ('FLAG', LUNF, DISKIN, CNOIN, FGVERI,
     *               FGVERO, LFGRNO, FGKOLS, FGNUMV, LDSOUR, SUBARR,
     *               FRQSEL, IA1, IA2, BTIME, ETIME, IIF, IIF, 1, 0,
     *               PFLAGS, REASON, CATUV, FGBUFL, IRET)
                  IF (IRET.NE.0) GO TO 80
                  END IF
 50            CONTINUE
            END IF
         END IF
C                                       Report flagging problems
 80   IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1080) IRET
         GO TO 990
         END IF
      IRET = 0
C                                       Close the flagging table if used
      IF (FLAGED) CALL FLAGIT ('CLOS', LUNF, DISKIN, CNOIN, FGVERI,
     *   FGVERO, LFGRNO, FGKOLS, FGNUMV, LDSOUR, SUBARR, FRQSEL, IA1,
     *   IA2, BTIME, ETIME, J, J, 1, 0, PFLAGS, REASON, CATUV, FGBUFL,
     *   IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1080 FORMAT ('DEFLDO: Error flagging', I3)
      END
      SUBROUTINE REPORT (NA, NP, NI, MALL, MBAD)
C-----------------------------------------------------------------------
C   Print the reports
C   Inputs:
C      NA      I      Number of antennas
C      NP      I      Number of polarizations
C      NI      I      Number of IFs
C      MALL    I(*)   Counts samples by baseline etc
C      MBAD    I(*)   Counts full bad spectra by baseline
C-----------------------------------------------------------------------
      INTEGER   NA, NP, NI, MALL(NA,NA,NP,NI), MBAD(NA,NA,NP,NI)
C
      INTEGER   I, J, K, L, TOTALA, TOTALB, MMAX, MXA, I1, I2
      INCLUDE 'DEFLG.INC'
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
               IF (ECHAN.LE.0) THEN
                  WRITE (MSGTXT,1020) TOTALB, TOTALA, K, I
                  CALL MSGWRT (4)
               ELSE
                  WRITE (MSGTXT,1021) TOTALB, TOTALA, K, I
                  CALL MSGWRT (4)
                  END IF
               WRITE (MSGTXT,1025) MMAX
               CALL MSGWRT (4)
               I1 = 1
 25            I2 = MIN (I1+27, MXA)
               IF (I2.GE.I1) THEN
                  DO 40 I= 1,MXA
                     DO 30 J= I1,I2
                        MAUX(J) = NINT ((10. * MALL(I,J,K,L)) / MMAX)
 30                     CONTINUE
                     WRITE (MSGTXT,1035) I, (MAUX(J), J = I1,I2)
                     CALL MSGWRT (4)
 40                  CONTINUE
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
 45            I2 = MIN (I1+27, MXA)
               IF (I2.GE.I1) THEN
                  DO 60 I = 1,MXA
                     DO 50 J = I1,I2
                        MAUX(J) = 0
                        IF (MALL(I,J,K,L).GT.0) THEN
                           MAUX(J) = NINT ((100. * MBAD(I,J,K,L)) /
     *                        MALL(I,J,K,L))
                        ELSE
                           MAUX(J) = 0
                           END IF
 50                     CONTINUE
                     WRITE (MSGTXT,1035) I, (MAUX(J), J = I1,I2)
                     CALL MSGWRT (4)
 60                  CONTINUE
                  I1 = I2 + 1
                  GO TO 45
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
 1025 FORMAT ('Visibilities per baseline (tens of percent of', I9 ,'):')
 1035 FORMAT ('Ant',I3,1X,28(I2))
 1090 FORMAT ('Flagged',I10,' correlators')
      END
      SUBROUTINE DEFLHI
C-----------------------------------------------------------------------
C   DEFLHI appends to the history file.
C-----------------------------------------------------------------------
      CHARACTER ATIME*8, ADATE*12, HILINE*72
      INTEGER   LUN, IERR, I, TIME(3), DATE(3), HBUFF(256)
      REAL      TEMP
      INCLUDE 'DEFLG.INC'
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
C                                       No Sources
      IF (NSOUWD.LE.0) THEN
         WRITE (HILINE,1100) TSKNAM
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Sources by name
      ELSE
C                                       Included or excluded?
         WRITE (HILINE,1101) TSKNAM
         IF (DOSWNT) WRITE (HILINE,1102) TSKNAM
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       First two and label.
         WRITE (HILINE,1103) TSKNAM, XSOUR(1), XSOUR(2)
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Rest of sources
         DO 110 I = 3,NSOUWD,2
            WRITE (HILINE,1104) TSKNAM, XSOUR(I), XSOUR(I+1)
            CALL HIADD (LUN, HILINE, HBUFF, IERR)
            IF (IERR.NE.0) GO TO 900
 110        CONTINUE
         END IF
C                                       QUAL, CALCODE
      WRITE (HILINE,1110) TSKNAM, SELQUA, SELCOD
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Subarray
      WRITE (HILINE,1111) TSKNAM, SUBARR
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Flag table
      WRITE (HILINE,1112) TSKNAM, FGVER
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (HILINE,1113) TSKNAM, FGVERO
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       TIMERANG
      CALL HITIME (TSTART, TEND, LUN, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       IF range
      WRITE (HILINE,1114) TSKNAM, BIF, EIF
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Chan range
      WRITE (HILINE,1115) TSKNAM, LBCHAN, LECHAN
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Subarray
      WRITE (HILINE,1116) TSKNAM, SUBARR
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Calibration
C                                       Table
      IF (DOCAL) THEN
         WRITE (HILINE,1117) TSKNAM, CLUSE
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       BP table
      IF (XDOBND.GT.0.0) THEN
         WRITE (HILINE,1118) TSKNAM, DOBAND
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (HILINE,1119) TSKNAM, BPVER
         CALL HIADD (LUN, HILINE, HBUFF, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       cut level
      WRITE (HILINE,1120) TSKNAM, ICUT
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
      TEMP = SOLINT * 86400.0
      WRITE (HILINE,1121) TSKNAM, TEMP
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
      TEMP = DETIME * 86400.0
      WRITE (HILINE,1122) TSKNAM, TEMP
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (DOSTOK.GT.0.0) THEN
         HILINE = TSKNAM // '/ All Stokes flagged when 1 bad'
      ELSE
         HILINE = TSKNAM // '/ Stokes flagged individually'
         END IF
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (DOALLS.GT.0.0) THEN
         HILINE = TSKNAM // '/ All Sources flagged when 1 bad'
      ELSE
         HILINE = TSKNAM // '/ Sources flagged individually'
         END IF
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                      Number of flagged correlators
      WRITE (HILINE,1123) TSKNAM, NBDCOR
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Close HI file
 900  CALL HICLOS (LUN, .TRUE., HBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'/********* Start ',A12,2X,A8)
 1100 FORMAT (A6,'SOURCES = ''''     /All sources selected')
 1101 FORMAT (A6,'/Sources excluded:')
 1102 FORMAT (A6,'/Sources included:')
 1103 FORMAT (A6,'SOURCES = ''',A16,''',''',A16,'''')
 1104 FORMAT (A6,'         ,''',A16,''',''',A16,'''')
 1110 FORMAT (A6,'QUAL = ',I4,' CALCODE = ',A4)
 1111 FORMAT (A6,'SUBARRAY =',I4)
 1112 FORMAT (A6,'FLAGVER  =',I4,' /Flagging table applied')
 1113 FORMAT (A6,'FLAGVERO =',I4,' /Flagging table written')
 1114 FORMAT (A6,'BIF =',I4,', EIF =',I4,'/ IF range')
 1115 FORMAT (A6,'BCHAN = ',I4,' ECHAN = ',I4,
     *   ' /Start and stop channels')
 1116 FORMAT (A6,'SUBARRAY =',I4)
 1117 FORMAT (A6,'GAINUSE =',I3,' / CL table')
 1118 FORMAT (A6,'DOBAND =',I2,'  /BP correction done')
 1119 FORMAT (A6,'BPVER =',I3,' / BP correction used BP table')
 1120 FORMAT (A6,'ICUT  =',F7.4,' / Decorrelations flagged below')
 1121 FORMAT (A6,'SOLINT =',F7.2,' / Averaging time (sec)')
 1122 FORMAT (A6,'DETIME =',F7.2,' / Scan break (sec)')
 1123 FORMAT (A6,'/ Correlators flagged:', I8)
      END
      SUBROUTINE FLAGIT (OPCODE, LUN, DISK, CNO, VERI, VER, LFGRNO,
     *   FGKOLS, FGNUMV, ID, SUBA, FQID, ANT1, ANT2, BTIME, ETIME, BIF,
     *   EIF, BCHAN, ECHAN, PFLAGS, REASON, CATUV, BUFF, IRET)
C-----------------------------------------------------------------------
C   Updates the Flag (FG) table. Adapted from FLAGUP
C   One entry is made indicating a visibility to be rejected.
C   The FLAG table will be opened on the first call but a final call
C   with OPCODE='CLOS' is required to close the file.
C   Inputs:
C      OPCODE   C*4      Operation desired, 'CLOS'=>close file
C                        'OPEN' create and copy,  else = 'FLAG'
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VERI     I        Input FG table version
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
            IF (FIRST) CALL MSGWRT (2)
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
     *      IFQ, ANTS, TIMER, IFS, CHANS, TFLAGS, TREAS, IRET)
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
 1002 FORMAT ('Copy',I8,' rows from FG vers',I3,' to',I3)
      END

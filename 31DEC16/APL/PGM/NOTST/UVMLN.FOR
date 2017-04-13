LOCAL INCLUDE 'UVMLN.INC'
C                                       Local include for UVMLN
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   SEQIN, CNOIN, DISKIN, JBUFSZ, LRECC, FITWTS(MAXCIF),
     *   NBDCOR, NRPRMI, INCSI, INCFI, INCIFI, FGVERO, NORDER, NIF,
     *   CHNSEL(3,20,MAXIF), NSOU, NCHAN, NPOLN, NANT, FGVERC
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC
      CHARACTER NAMEIN*12, CLAIN*6, XCALCO*4, XSTOK*4, XSOUR(30)*16
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XBIF, XEIF, XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XFGOUT, XDOBND, XBPVER, XSMOTH(3), XBADD(10),
     *   XCHNS(4,20), XORDER, BUFF1(UVBFSS), RMS
      LOGICAL   DOUVCM
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XQUAL,
     *   XXCALC, XTIME, XBAND, XFREQ, XFQID, XBIF, XEIF, XSUBA, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XFGOUT, XDOBND, XBPVER,
     *   XSMOTH, RMS, XCHNS, XORDER, XBADD
      COMMON /INFO/ FGVERO, NSOU, SEQIN, DISKIN, CNOIN, LRECC, DOUVCM,
     *   NRPRMI, INCSI, INCFI, INCIFI, NBDCOR, NORDER, NIF, CHNSEL,
     *   NCHAN, NPOLN, NANT, FGVERC
      COMMON /CHRCOM/ NAMEIN, CLAIN, XCALCO, XSOUR, XSTOK
      COMMON /WEIGHT/ FITWTS
LOCAL END
      PROGRAM UVMLN
C-----------------------------------------------------------------------
C! Applies calibration and/or editing and flags multisource uv data.
C# task editing UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998, 2000-2001, 2003-2007, 2010-2012, 2015-2016
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
C   calibration and editing.  It then fits a baseline to selected
C   channels, examines the rms in those channels after the fit and
C   makes entries in an FG table for excess rms.
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
C      FLUX           RMS           Max deviation for unity weight
C      NBOXES         NB            Number of boxes
C      BOX            BOXES         Boxes (start-stop pairs)
C      BADDISK        IBAD          Disks to avoid for scratch files.
C   Programmer: J. M. Uson (based on calibration tasks and UVLIN)
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   MALL(2), MBAD(2), MCHAN(2), NWORDS, NUMSOU, IRET, I
      LONGINT   OFFALL, OFFBAD, OFFCHN
C      INTEGER   MALL(MAXANT,MAXANT,4*MAXIF), MCHAN(4*MAXCIF),
C     *   MBAD(MAXANT,MAXANT,4*MAXIF)
      LOGICAL   DOWANT
      INCLUDE 'UVMLN.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   SULIST(XSTBSZ)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'UVMLN '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL UVMLIN (PRGM, DOWANT, NUMSOU, SULIST, IRET)
C                                       Allocate memory
      NWORDS = (NANT * NANT * NPOLN * NIF - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MALL, OFFALL,
     *   IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MBAD, OFFBAD,
     *   IRET)
      IF (IRET.EQ.0) THEN
         NWORDS = 1024 * NWORDS
         CALL FILL (NWORDS, 0, MALL(1+OFFALL))
         CALL FILL (NWORDS, 0, MBAD(1+OFFBAD))
         END IF
      NWORDS = (NPOLN * NCHAN * NIF - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, MCHAN, OFFCHN,
     *   IRET)
      IF (IRET.EQ.0) CALL FILL (1024*NWORDS, 0, MCHAN(1+OFFCHN))
C                                       Loop over sources.
      IF (IRET.EQ.0) CALL UVMLUV (DOWANT, NUMSOU, SULIST, NANT, NPOLN,
     *   NCHAN, NIF, MALL(1+OFFALL), MBAD(1+OFFBAD), MCHAN(1+OFFCHN),
     *   IRET)
C                                       report results
      IF (IRET.EQ.0) CALL REPORT (NANT, NPOLN, NCHAN, NIF,
     *   MALL(1+OFFALL), MBAD(1+OFFBAD), MCHAN(1+OFFCHN))
C                                       Report deeds to History file
      IF (IRET.EQ.0) CALL UVMLHI
C                                       Done: Close down files, etc.
      CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, MCHAN, OFFCHN, I)
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UVMLIN (PRGN, DOWANT, NUMSOU, SULIST, JERR)
C-----------------------------------------------------------------------
C   UVMLIN gets input parameters for UVMLN, finds input file and
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
      CHARACTER STAT*4, UTYPE*2
      INTEGER   NPARM, IROUND, IERR, I, LUN, J, K, K1, K2, IOFF, JJ,
     *   NUMAN(513), NVER
      REAL      CATR(256)
      LOGICAL   MATCH
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVMLN.INC'
      INTEGER   NW(MAXIF)
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
      NPARM = 237
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
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
C                                       Set BCHAN and ECHAN to all
      BCHAN = 1
      ECHAN = CATBLK(KINAX+JLOCF)
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
      NCHAN = ECHAN
      NPOLN = CATBLK(KINAX+JLOCS)
C                                       Max antenna number
      NANT = MAXANT
      CALL FNDEXT ('AN', CATBLK, NVER)
      IF (NVER.GT.0) THEN
         LUN = 27
         CALL GETNAN (DISKIN, CNOIN, CATBLK, LUN, BUFF1, NUMAN, JERR)
         IF ((NVER.GT.0) .AND. (JERR.EQ.0)) THEN
            JJ = NUMAN(1)
            NANT = 0
            DO 32 J = 1,JJ
               NANT = MAX (NANT, NUMAN(J+1))
 32            CONTINUE
            END IF
         END IF
C                                       ICHANSEL
      I = ECHAN * NIF
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
C                                       use 1 - ECHAN
 45    DO 60 K = BIF,EIF
          IOFF = (K - BIF) * ECHAN
          IF (NW(K).LE.0) THEN
             NW(K) = 1
             CHNSEL(1,1,K) = 1
             CHNSEL(2,1,K) = ECHAN
             CHNSEL(3,1,K) = 1
             END IF
          DO 55 I = 1,NW(K)
             CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), ECHAN))
             IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K)) CHNSEL(2,I,K) = ECHAN
             CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), ECHAN))
             DO 50 J = CHNSEL(1,I,K),CHNSEL(2,I,K),CHNSEL(3,I,K)
                FITWTS(J+IOFF) = 1
 50            CONTINUE
 55         CONTINUE
 60      CONTINUE
C                                       Write weights to message file
      DO 70 K = BIF,EIF
         IOFF = (K-BIF)*ECHAN
         DO 65 I = 1,ECHAN,20
            K1 = I
            K2 = MIN (I+19, ECHAN)
            WRITE (MSGTXT,1060) K, I, (FITWTS(J+IOFF), J = K1,K2)
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
      DOUVCM = .FALSE.
      CALL FILL (50, 0, ANTENS)
      SUBARR = IROUND (XSUBA)
      NORDER = XORDER + 0.1
      NORDER = MAX (0, MIN (1, NORDER))
C                                       set flag versions
      CALL FNDEXT ('FG', CATBLK, I)
      FGVER = IROUND (XFLAG)
      IF ((FGVER.EQ.0) .OR. (FGVER.GT.I)) FGVER = I
      FGVERO = IROUND (XFGOUT)
      IF ((FGVERO.LE.0) .OR. (FGVER.GT.I)) FGVERO = I + 1
      FGVERC = FGVER
      IF (FGVERO.LE.I) FGVERC = -ABS (FGVER)
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
C                                        Local parameters
      IF (RMS.LE.0.0) RMS = 1.0E20
      WRITE (MSGTXT,1100) RMS
      CALL MSGWRT(4)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVMLIN: error',I3,' obtaining input parameters')
 1030 FORMAT ('Error',I3,' finding ',A12,'.',A6,'.',I4,' disk =',
     *   I3,' user=',I5)
 1035 FORMAT ('Error',I3,' obtaining CATBLK ')
 1060 FORMAT ('Weights (',I3,'/',I5,') ',20I2)
 1100 FORMAT ('Flagging threshold for unit weight = ',1PE12.3,' Jy')
      END
      SUBROUTINE UVMLUV (DOWANT, NUMSOU, SULIST, NA, NP, NC, NI, MALL,
     *   MBAD, MCHAN, IRET)
C-----------------------------------------------------------------------
C   UVMLUV uses UVGET to obtain data as single source files and UVMCOP
C   to loop through a source at a time to check for bad data
C   Input:
C      DOWANT   L      If true sources listed are selected
C      NUMSOU   I      Number of sources to process, 0=>all
C      SULIST   I(*)   Source number list.
C      NA       I      Number of antennas
C      NP       I      Number of polarizations
C      NC       I      Number of channels
C      NI       I      Number of IFs
C   Output:
C      MALL    I(*)   Counts samples by baseline etc
C      MBAD    I(*)   Counts full bad spectra by baseline
C      MCHAN   I(*)   Counts flags by channel
C      IRET     I      Return code, 0 => OK, otherwise abort.
C   Auxiliary:
C      FGVERO   I      Version number of new flag table (reset later)
C-----------------------------------------------------------------------
      LOGICAL   DOWANT
      INTEGER   NUMSOU, SULIST(*), NA, NP, NC, NI, MALL(NA,NA,NP,NI),
     *   MBAD(NA,NA,NP,NI), MCHAN(NP,NC,NI), IRET
C
      INCLUDE 'UVMLN.INC'
      CHARACTER VELTYP*8, VELDEF*8, CALCOD*4
      LOGICAL   DOAVG, DOAPPT, NOSUB, SINGLE, TABLE, EXIST, FITASC
      INTEGER   NUMVIS, SOUCUR, MAXSOU, SLOOP, DPOSAV, LRECU, NRPRIN,
     *   IERR, SUKOLS(MAXSUC), SUNUMV(MAXSUC), SBUFF(512), I, SLUN,
     *   INOGRP, SUB, NUMSUB, LIMS1, LIMS2, SUBTMP, SAVBND, SUFQID,
     *   INCX, IDSOU, QUAL
      REAL      RPARM, VIS, CATR(256), OLDRP
      DOUBLE PRECISION  BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, CATD(128), OLDFRQ, RAOBS, DECOBS
      DOUBLE PRECISION   LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      REAL     FLUX(4,MAXIF)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
C-----------------------------------------------------------------------
      DOAPPT = DOAPPL
      DOAVG = .FALSE.
      NOSUB = .FALSE.
      OLDRP = CATR(KRCRP+JLOCF)
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
      DO 200 SLOOP = 1,MAXSOU
C                                       Save SLOOP from TABSOU
         SOUCUR = SLOOP
C                                       Read source table
         CALL TABSOU ('READ', SBUFF, SOUCUR, SUKOLS, SUNUMV, IDSOU,
     *      SOURCS, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IRET)
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
         DOPOL = 0
         SAVBND = DOBAND
         DOBAND = 0
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
         DOPOL = DPOSAV
         DOAPPL = .FALSE.
         DOBAND = SAVBND
         CLVER = CLUSE
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
               CALL UVMCOP (NUMVIS, NA, NP, NC, NI, MALL, MBAD, MCHAN,
     *            BUFF1, LRECU, NRPRIN, FGVERC, FGVERO, FVOL(1),
     *            FCNO(1), IDSOU, IERR)
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
C                                       Last call to UVMCOP
      CALL UVMCOP (-1, NA, NP, NC, NI, MALL, MBAD, MCHAN, BUFF1,
     *   LRECU, NRPRIN, FGVERC, FGVERO, FVOL(1), FCNO(1), IDSOU, IERR)
C                                       Close source table
      CALL TABIO ('CLOS', 1, SOUCUR, SBUFF, SBUFF, IERR)
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
      SUBROUTINE REPORT (NA, NP, NC, NI, MALL, MBAD, MCHAN)
C-----------------------------------------------------------------------
C   Print the reports
C   Inputs:
C      NA      I      Number of antennas
C      NP      I      Number of polarizations
C      NC      I      Number of channels
C      NI      I      Number of IFs
C      MALL    I(*)   Counts samples by baseline etc
C      MBAD    I(*)   Counts full bad spectra by baseline
C      MCHAN   I(*)   Counts flags by channel
C-----------------------------------------------------------------------
      INTEGER   NA, NP, NC, NI, MALL(NA,NA,NP,NI), MBAD(NA,NA,NP,NI),
     *   MCHAN(NP,NC,NI)
C
      INTEGER   I, J, K, L, TOTALA, TOTALB, MMAX, MXA, I1, I2
      INCLUDE 'UVMLN.INC'
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
               WRITE (MSGTXT,1020) TOTALB, TOTALA, K, I
               CALL MSGWRT (4)
               WRITE(MSGTXT,1021) MMAX
               CALL MSGWRT (4)
               I1 = 1
 25            I2 = MIN (I1+27, MXA)
               IF (I2.GE.I1) THEN
                  WRITE (MSGTXT,1036) (J, J = I1,I2)
                  CALL MSGWRT (4)
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
C                                       Now report percentage flagged
               MSGTXT = 'Visibilities flagged (percent):'
               CALL MSGWRT (4)
               I1 = 1
 45            I2 = MIN (I1+27, MXA)
               IF (I2.GE.I1) THEN
                  WRITE (MSGTXT,1036) (J, J = I1,I2)
                  CALL MSGWRT (4)
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
C                                       Now report on channel triggers
               DO 70 I = BCHAN,ECHAN
                  IF (MCHAN(K,I,L).GT.0) THEN
                     WRITE (MSGTXT,1060) I, MCHAN(K,I,L)
                     CALL MSGWRT (4)
                     END IF
 70               CONTINUE
               END IF
 80         CONTINUE
 90      CONTINUE
C                                       Report flagged correlators
      WRITE (MSGTXT,1080) NBDCOR
      CALL MSGWRT (4)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Flagged',I8,' of',I10,' correlators for corr,IF',I2,I3)
 1021 FORMAT ('Visibilities per baseline (tens of percent of', I9 ,'):')
 1035 FORMAT ('Ant',I3,1X,28(I2))
 1036 FORMAT (7X,28(I2))
 1060 FORMAT ('Channel:',I4,'; ',I8,' flags')
 1080 FORMAT ('Flagged ',I8,' correlators')
      END
      SUBROUTINE UVMCOP (NUMVIS, NA, NP, NC, NI, MALL, MBAD, MCHAN,
     *   BUFF1, LRECU, NRPRIN, FGVERC, FGVERO, INDISK, INNUM, IDSOUR,
     *   IRET)
C-----------------------------------------------------------------------
C   UVMCOP calls UVLINF to do fitting and raise the flags which are
C   entered upon return from UVLINF
C   Input:
C      NUMVIS   I      Number of visibilities previously processed
C      NA       I      Number of antennas
C      NP       I      Number of polarizations
C      NC       I      Number of channels
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
C      MALL    I(*)   Counts samples by baseline etc
C      MBAD    I(*)   Counts full bad spectra by baseline
C      MCHAN   I(*)   Counts flags by channel
C      BUFF1    R(*)   I/O buffer.
C      IRET     I      Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, NA, NP, NC, NI, MALL(NA,NA,NP,NI),
     *   MBAD(NA,NA,NP,NI), MCHAN(NP,NC,NI), LRECU, NRPRIN,
     *   FGVERC, FGVERO, INDISK, INNUM, IDSOUR, IRET
      REAL      BUFF1(*)
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER REASON*24
      INTEGER   FLGKEY(4*MAXIF), VCOUNT, LUN, NOPOL, I, JNCIF, JNCS, J,
     *   K, NUMFRQ, LRECO, LENBU, IA1, IA2, INCX, INNUMS, FGBUFL(512),
     *   LFGRNO
      LOGICAL   PFLAGS(4), GOODDT, FLAGED, FLAGDO
      REAL      CATR(256), BTIME, ETIME
      DOUBLE PRECISION CATD(128)
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
C                                       Set flagging reason
      REASON = 'UVMLN '
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
C                                       Call UVLINF now
         IF (ILOCB.GE.0) THEN
            IA2 = BUFF1(1+ILOCB) + 0.1
            IA1 = IA2 / 256
            IA2 = IA2 - IA1*256
         ELSE
            IA1 = BUFF1(1+ILOCA1) + 0.1
            IA2 = BUFF1(1+ILOCA2) + 0.1
            END IF
C                                      Working on visibility number
C                                      NUMVIS
         NUMVIS = NUMVIS + 1
C                                      Call filtering routine
         CALL UVLINF (NUMVIS, IA1, IA2, NA, NP, NC, NI, MALL, MBAD,
     *      MCHAN, BUFF1(1+NRPRIN), INCX, IRET)
C                                       See if flagging is necessary
         FLAGDO = .FALSE.
         DO 10 J = 1,4*NUMIF
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
            DO 20 J = 1,4*NUMIF
               IF (FLGKEY(J).GT.0) GOODDT = .TRUE.
 20            CONTINUE
C                                       Loop through IF's to do flagging
            IF (GOODDT) THEN
               DO 70 J = 1,NUMIF
                  FLAGDO = .FALSE.
                  DO 60 K = 1,4
                     PFLAGS(K) = .FALSE.
                     IF (FLGKEY(4*(J-1)+K).LT.0) THEN
                        FLAGDO = .TRUE.
                        PFLAGS(K) = .TRUE.
                        END IF
 60                  CONTINUE
C                                       Flag [IF = (j)] if necessary
                  IF (FLAGDO) CALL FLAGIT ('FLAG', LUN,  INDISK, INNUM,
     *               FGVERC, FGVERO, LFGRNO, FGKOLS, FGNUMV, IDSOUR,
     *               SUBARR, FRQSEL, IA1, IA2, BTIME, ETIME, J, J, 1, 0,
     *               PFLAGS, REASON, CATUV, FGBUFL, IRET)
 70               CONTINUE
C                                       No good data, flag whole record
            ELSE
               DO 80 K = 1,4
                  PFLAGS(K) = .TRUE.
 80               CONTINUE
               CALL FLAGIT ('FLAG', LUN, INDISK, INNUM, FGVERC, FGVERO,
     *            LFGRNO, FGKOLS, FGNUMV, IDSOUR, SUBARR, FRQSEL, IA1,
     *            IA2, BTIME, ETIME, 1, 0, 1, 0, PFLAGS, REASON, CATUV,
     *            FGBUFL, IRET)
               END IF
C                                       Report flagging problems
            IF (IRET.GT.0) THEN
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
      IF (FLAGED) CALL FLAGIT ('CLOS', LUN, INDISK, INNUM, FGVERC,
     *   FGVERO, LFGRNO, FGKOLS, FGNUMV, IDSOUR, SUBARR, FRQSEL, IA1,
     *   IA2, BTIME, ETIME, J, J, 1, 0, PFLAGS, REASON, CATUV, FGBUFL,
     *   IRET)
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
 1000 FORMAT ('UVMCOP: Error obtaining data',I3,' VIS NUM',I10)
 1080 FORMAT ('UVMCOP: Error flagging', I3)
 1110 FORMAT (I10,' visibilities processed for this source')
      END
      SUBROUTINE UVMLHI
C-----------------------------------------------------------------------
C   UVMLHI appends to the history file.
C-----------------------------------------------------------------------
      CHARACTER ATIME*8, ADATE*12, HILINE*72
      INTEGER   LUN, IERR, I, TIME(3), DATE(3), HBUFF(256), J, K
      INCLUDE 'UVMLN.INC'
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
         DO 10 I = 3,NSOUWD,2
            WRITE (HILINE,1104) TSKNAM, XSOUR(I), XSOUR(I+1)
            CALL HIADD (LUN, HILINE, HBUFF, IERR)
            IF (IERR.NE.0) GO TO 900
 10         CONTINUE
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
      WRITE (HILINE,1115) TSKNAM, BCHAN, ECHAN
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
C                                       Weights used
      DO 30 K = BIF,EIF
         DO 20 I = 1,20
            IF ((CHNSEL(1,I,K).GT.0) .AND.
     *         (CHNSEL(2,I,K).GE.CHNSEL(1,I,K))) THEN
               WRITE(HILINE,1120) TSKNAM, I, (CHNSEL(J,I,K), J = 1,3), K
               CALL HIADD (LUN, HILINE, HBUFF, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
 20         CONTINUE
 30      CONTINUE
C                                       Threshold
      WRITE (HILINE,1121) TSKNAM, RMS
      CALL HIADD (LUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 900
C                                      Number of flagged correlators
      WRITE (HILINE,1122) TSKNAM, NBDCOR
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
 1120 FORMAT (A6,'CHNSEL(',I2.2,') =',I5,':',I5,' BY',I3,
     *   '  / fit region IF=',I3)
 1121 FORMAT (A6,'/ Flagging threshold for unit weight = ',F8.2,' Jy')
 1122 FORMAT (A6,'/ Correlators flagged:', I8)
      END
      SUBROUTINE UVLINF (NUMVIS, IA1, IA2, NA, NP, NC, NI, MALL, MBAD,
     *   MCHAN, VIS, INCX, IRET)
C-----------------------------------------------------------------------
C   Routine to fit straight line to chosen channels, subtract and check
C   residual values.  *** IT FITS TO REAL AND IMAGINARY COMPONENTS ***
C   Inputs:
C      NUMVIS   I        Visibility number
C      IA1      I        First antenna number
C      IA2      I        Second antenna number
C      NA       I      Number of antennas
C      NP       I      Number of polarizations
C      NC       I      Number of channels
C      NI       I      Number of IFs
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
C      FITWTS   I(*)     Weights for fitting
C   Output:
C      MALL    I(*)   Counts samples by baseline etc
C      MBAD    I(*)   Counts full bad spectra by baseline
C      MCHAN   I(*)   Counts flags by channel
C      FLGKEY   I(*)     Flagging decisions
C      IRET     I        Return code   0 => OK
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2, NA, NP, NC, NI, MALL(NA,NA,NP,NI),
     *   MBAD(NA,NA,NP,NI), MCHAN(NP,NC,NI), INCX, IRET
      REAL      VIS(INCX,*)
C
      INCLUDE 'UVMLN.INC'
      INTEGER   CINDEX, FLGKEY(4*MAXIF), LOOP, INDEX, OFF, IOFF, IS,
     *   IIF, FITNUM, I
      REAL      RESULT(2), FITRE(MAXCHA), FITIM(MAXCHA), FITCHA(MAXCHA),
     *   AR, BR, AI, BI, AMPLIT, AVGR, AVGI
      LOGICAL   ALLBAD, GOODDT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /FLAGS/ FLGKEY
C-----------------------------------------------------------------------
C                                       Set up on first call
      IF (NUMVIS.EQ.1) THEN
         NBDCOR = 0
         END IF
C                                       process data
      IRET = 0
      IF (NUMVIS.GT.0) THEN
         IF (MOD(NUMVIS,10000).EQ.0) THEN
            WRITE (MSGTXT,1000) NUMVIS
            CALL MSGWRT (2)
            END IF
         I = 4 * NIF
         CALL FILL (I, 0, FLGKEY)
C                                       Loop over IF, Stokes
         DO 40 IIF = 1,NIF
            IOFF = (IIF - 1) * (ECHAN - BCHAN + 1)
            DO 30 IS = 1,NPOLN
               ALLBAD = .FALSE.
               GOODDT = .FALSE.
C                                       Offset in Stokes and IF
               OFF = (IS - 1) * INCSI + (IIF - 1) * INCIFI
C                                       Set index for info counters
               CINDEX = 4 * (IIF -1) + IS
C                                       Index for this channel, IF
               INDEX = OFF + 1
C                                       Get arrays for line fitting
               FITNUM = 0
               DO 10 LOOP = BCHAN,ECHAN
                  IF (VIS(3,INDEX).GT.0.0) THEN
                      GOODDT = .TRUE.
                      IF (FITWTS(LOOP+IOFF).GT.0) THEN
                          FITNUM = FITNUM + 1
                          FITRE(FITNUM) = VIS(1,INDEX)
                          FITIM(FITNUM) = VIS(2,INDEX)
                          FITCHA(FITNUM) = LOOP
                          END IF
                      END IF
                  INDEX = INDEX + INCFI
 10               CONTINUE
C                                       Some data to fit
               IF (FITNUM.GT.0) THEN
                  CALL LINFIT (NORDER, FITCHA, FITRE, FITNUM, AR, BR)
                  CALL LINFIT (NORDER, FITCHA, FITIM, FITNUM, AI, BI)
C                                       Now subtract from data
                  INDEX = OFF + 1
                  DO 20 LOOP = BCHAN,ECHAN
                     IF ((VIS(3,INDEX).GT.0.0) .AND.
     *                  (FITWTS(LOOP+IOFF).GT.0)) THEN
C                                       Interpolated value (re, imag)
                        AVGR = AR + BR * LOOP
                        AVGI = AI + BI * LOOP
C                                       Subtract from vis.
                        RESULT(1) = VIS(1,INDEX) - AVGR
                        RESULT(2) = VIS(2,INDEX) - AVGI
                        AMPLIT = RESULT(1)**2 + RESULT(2)**2
C                                       Scale by integration time
                        AMPLIT = SQRT (AMPLIT * VIS(3,INDEX))
C                                       Check against RMS
                        IF (AMPLIT.GT.RMS) THEN
                           ALLBAD = .TRUE.
                           MCHAN(IS,LOOP,IIF) = MCHAN(IS,LOOP,IIF) + 1
                           END IF
                       END IF
                    INDEX = INDEX + INCFI
 20                 CONTINUE
C                                       window all bad, some good?
               ELSE
                  ALLBAD = .TRUE.
                  END IF
C                                       Increase corresponding counter
C                                       and use FLGKEY to signal data
               IF (GOODDT) THEN
                  MALL(IA1,IA2,IS,IIF) = MALL(IA1,IA2,IS,IIF)+1
                  FLGKEY(CINDEX) = 1
C                                       Now flag everything if needed
                  IF (ALLBAD)  THEN
C                                       Increase corresponding counters
C                                       use FLGKEY to signal bad data
                     NBDCOR = NBDCOR + 1
                     MBAD(IA1,IA2,IS,IIF) = MBAD(IA1,IA2,IS,IIF)+1
                     FLGKEY(CINDEX) = -1
                     END IF
                  END IF
 30            CONTINUE
 40         CONTINUE
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
      A = 0.0
      B = 0.0
      IF ((NDATA.GE.2) .AND. (N.GT.0)) THEN
         DELTA = NDATA * SXX - SX * SX
         IF (DELTA.GT.0) THEN
            A = ( SXX * SY - SX * SXY ) / DELTA
            B = ( SXY * NDATA - SX * SY ) / DELTA
            END IF
      ELSE IF ((NDATA.GE.1) .AND. (N.EQ.0)) THEN
         A = SY / NDATA
         END IF
C
 999  RETURN
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
C                        Anything else = 'FLAG'
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VERI     I        FG file version copied to VER at start
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
      LOGICAL   PFLAGS(4), TFLAGS(4),  FIRST
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
            CALL FGREFM (DISK, CNO, VER, CATUV, LUN, IRET)
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

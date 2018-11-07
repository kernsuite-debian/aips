LOCAL INCLUDE 'GETJY.INC'
C                                       Program common variables
      INTEGER   MAXSOU
C                                       MAXSOU = max. no. sources
      PARAMETER (MAXSOU = 500)
      INTEGER   SEQIN, CNOIN, DISKIN, JBUFSZ, SUBA, BIF, EIF,
     *   SOUNUM(MAXSOU), NSUCAL, NOIF, NOANT, NOPOL, FREQID, SNVER
      LOGICAL   ISCAL(MAXSOU)
      REAL      XSIN, XDISIN, XQUAL, XB, XE, XTIME(8), XSUBA, XANT(50),
     *   SUFLX(MAXIF,MAXSOU), BUFF1(2048), XBAND, XFREQ, XFQID, XSNVER,
     *   SELBAN, TSTART, TEND, SUSAV(3,MAXIF,MAXSOU)
      DOUBLE PRECISION SELFRQ
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXSOUC, XXCAL(4,30),
     *   XXCALC
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XCAL(30)*16, XCALCO*4,
     *   XSOUCO*4, NAMSOU(MAXSOU)*16
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /INPARM/ SELFRQ, XNAMEI, XCLAIN, XSIN, XDISIN,
     *   XXSOUR, XXSOUC, XXCAL, XQUAL, XXCALC, XB, XE, XTIME, XANT,
     *   XSUBA, XBAND, XFREQ, XFQID, XSNVER,
     *   SELBAN, SEQIN, DISKIN, CNOIN, SUBA, BIF, EIF, TSTART, TEND,
     *   FREQID, SNVER
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XCAL, XCALCO, XSOUCO, NAMSOU
      COMMON /SNSTUF/ SUFLX, SUSAV, ISCAL, NSUCAL, NOIF, NOANT, NOPOL,
     *   SOUNUM
C                                                            End GETJY
LOCAL END
      PROGRAM GETJY
C-----------------------------------------------------------------------
C! Bootstrap Source flux densities from SN table.
C# Calibration UV EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2000, 2003, 2005, 2007, 2009, 2011-2012
C;  Copyright (C) 2015-2016
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
C   Flux density bootstrap using SN tables.  The flux densities are
C   inserted into the SU table and the solution tables are corrected.
C   Inputs:
C      AIPS adverb          Description.
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      SOURCES....Source list.
C      SOUCODE....Source "calibrator" code
C      CALSOUR....Calibrator list.
C      QUAL.......Qualifiers for sourcs and calibrator
C      CALCODE....CALCODEs to use to select calibrators.
C      TIMERANG...Time range of the data to be calibrated.
C      SUBARRAY...Subarray number to calibrate. 0=>all.
C      ANTENNAS...Antenna numbers to use to determine fluxes
C      SNVER......SN table to use.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, I, NWORDS, MANT, MIF, MP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'GETJY.INC'
C     REAL      SNVALS(NSUCAL,MANT,NP,NIF,MAXSNR)
      INTEGER   SNRECS(MAXSOU,MAXANT), MAXSNR
      REAL      SNVALS(2)
      LONGINT   OFFSNV
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'GETJY '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL GETJIN (PRGM, MANT, MIF, IRET)
      NWORDS = 3 * MAXIF * MAXSOU
      CALL RFILL (NWORDS, 0.0, SUSAV)
C                                       Read SU table
      IF (IRET.EQ.0) CALL GJYRSU (IRET)
C                                       count data samples
      NWORDS = MAXSOU * MAXANT
      CALL FILL (NWORDS, 0, SNRECS)
      IF (IRET.EQ.0) CALL GJYCSN (NSUCAL, MANT, SNRECS, MAXSNR, IRET)
      MP = NOPOL
      CALL FILL (NWORDS, 0, SNRECS)
C                                       allocate memory
      NWORDS = (NSUCAL * MANT * MIF * MP * MAXSNR - 1) / 1024 + 1
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SNVALS,
     *   OFFSNV, IRET)
      IF (IRET.EQ.0) CALL RFILL (1024*NWORDS, 0.0, SNVALS(1+OFFSNV))
C                                       Read SN tables
      IF (IRET.EQ.0) CALL GJYRSN (NSUCAL, MANT, MP, MIF, SNRECS,
     *   SNVALS(1+OFFSNV), IRET)
C                                       Find flux densities
       IF (IRET.EQ.0) CALL GJYFLX (NSUCAL, MANT, MP, MIF, SNRECS,
     *    SNVALS(1+OFFSNV), IRET)
C                                       Free memory
      IF (IRET.EQ.0) CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, SNVALS,
     *   OFFSNV, I)
C                                       Update SU table
      IF (IRET.EQ.0) CALL GJYWSU (IRET)
C                                       Update SN table
      IF (IRET.EQ.0) CALL GJYWSN (IRET)
C                                       History
      IF (IRET.EQ.0) CALL GETJHI
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE GETJIN (PRGN, MANT, MIF, JERR)
C-----------------------------------------------------------------------
C   GETJIN gets input parameters for GETJY and finds input file.
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C      MANT   I     Maximum antenna number
C      MIF    I     Maximum IF number
C      JERR   I     Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in GETJY for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   MANT, MIF, JERR
C
      CHARACTER STAT*4, UTYPE*2
      INTEGER   NPARM, IROUND, IERR, I, LUN, NUMAN(513), J, JJ, NVER
      LOGICAL   T, TABLE, EXIST, FITASC, MATCH, MULTI
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'GETJY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T /.TRUE./
      DATA LUN /29/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T, BUFF1)
      CALL VHDRIN
      JBUFSZ = 4096
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 315
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
      SUBA = IROUND (XSUBA)
      SNVER = IROUND (XSNVER)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      CALL H2CHR (4, 1, XXSOUC, XSOUCO)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
         CALL H2CHR (16, 1, XXCAL(1,I), XCAL(I))
 20      CONTINUE
C                                       Get CATBLK.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 0
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FREQID = IROUND (XFQID)
      IF (FREQID.EQ.0) FREQID = -1
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FREQID, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       IF Range
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XB)
         IF (BIF.LE.0) BIF = 1
         EIF = IROUND (XE)
         MIF = CATBLK(KINAX+JLOCIF)
         IF ((EIF.LT.BIF) .OR. (EIF.GT.MIF)) EIF = MIF
      ELSE
         BIF = 1
         EIF = 1
         MIF = 1
         END IF
C                                       Max antenna number
      MANT = MAXANT
      CALL FNDEXT ('AN', CATBLK, NVER)
      IF (NVER.GT.0) THEN
         CALL GETNAN (DISKIN, CNOIN, CATBLK, LUN, BUFF1, NUMAN, IERR)
         IF ((NVER.GT.0) .AND. (IERR.EQ.0)) THEN
            JJ = NUMAN(1)
            IF ((SUBA.GT.0) .AND. (SUBA.LE.JJ)) THEN
               MANT = NUMAN(SUBA+1)
            ELSE
               MANT = 0
               DO 80 J = 1,JJ
                  MANT = MAX (MANT, NUMAN(J+1))
 80               CONTINUE
               END IF
            END IF
         END IF
C                                       See if single or multi source
      CALL MULSDB (CATBLK, MULTI)
      IF (MULTI) THEN
         CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUN, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         MULTI = EXIST .AND. TABLE .AND. (IERR.EQ.0)
         END IF
C                                       Can only do multi source files.
      IF (.NOT.MULTI) THEN
         JERR = 6
         MSGTXT = 'ERROR: I ONLY WORK ON MULTI-SOURCE FILES'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETJIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE GJYRSU (IERR)
C-----------------------------------------------------------------------
C   Fills in arrays of source numbers to be included or excluded and get
C   the FLUX densities of the calibrators.
C   Inputs from common:
C      DISKIN      I    Input disk number.
C      CNOIN       I    Catalog slot number
C      XSOUR(30)   C    Names (16 char) of up to 30 sources, *=>all
C                       First character of name '-' => all except those
C                       specified.
C      XSOUCO      C    Source "cal" code
C      XCAL(30)    C    Names (16 char) of up to 30 calibrators, *=>all
C                       First character of name '-' => all except those
C                       specified.
C      XCALCO      C    Calibrator "cal" code
C   Input from common /MAPHDR/
C      CATBLK(256)  I    Catalog header record.
C   Input/Output:
C      BUFF1(*)     I    I/O Buffer for source (SU) table
C   Output in common:
C      NSUCAL       I    Number of sources plus calibrators
C      SOUNUM(*)    I    Source ID numbers of sources and calibrators.
C      ISCAL(*)     L    True if source a calibrator.
C      SUFLX(if,*)  R    Source IPOL flux densities 1/IF.
C      IERR         I    Return code, 0=>OK, otherwise failed.
C   Note: uses AIPS LUN 29.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELDEF*8, VELTYP*8, SOUNAM*16, CALCOD*4, TELTYP*8
      INTEGER   JERR, JQUAL, IDSOU, SUKOLS(MAXSUC), SUNUMV(MAXSUC), I1,
     *   IXLUN, IROUND,  QUAL, NUMIF, J, K, BUFFER(512), IIF, NSOUWD,
     *   NCALWD, NSOURC, I, ISURNO, MXSOU, SUFQID
      LOGICAL   T, F, EQUAL, TABLE, EXIST, FITASC, ALLSOU, EQUCAL,
     *   ALLCAL, DOSWNT, DOCWNT, ISSEL, CCBLNK, CCNBLK, CCNCAL, CHKCC,
     *   SCBLNK, SCNBLK, SCNCAL, CHKSC, SOUBLK, CALBLK
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, LSRVEL(MAXIF), FREQO(MAXIF), LRESTF(MAXIF), RAOBS,
     *   DECOBS
      REAL      FLUX(4,MAXIF)
      INCLUDE 'GETJY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (BUFFER, BUFF1)
      DATA T, F /.TRUE.,.FALSE./
      DATA IXLUN /29/
      DATA MXSOU /MAXSOU/
C-----------------------------------------------------------------------
C                                       See if SU file exists.
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, IXLUN, BUFFER, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((JERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST)) GO TO 800
C                                       Open SU table
      CALL SOUINI ('READ', BUFFER, DISKIN, CNOIN, 1, CATBLK, IXLUN,
     *   NUMIF, VELDEF, VELTYP, SUFQID, ISURNO, SUKOLS, SUNUMV, JERR)
      IF (JERR.GT.0) THEN
         WRITE (MSGTXT,1000) JERR
         GO TO 990
         END IF
C                                       Check FREQID compatibility.
CC                                      But not for AT data
      CALL H2CHR (8, 1, CATH(KHTEL), TELTYP)
      IF ((SUFQID.GT.0) .AND. (FREQID.GT.0) .AND. (SUFQID.NE.FREQID)
     *   .AND. (TELTYP.NE.'ATCA') .AND. (TELTYP.NE.'atca') .AND.
     *   (TELTYP.NE.'ATLBA') .AND. (TELTYP.NE.'LBA')) THEN
         WRITE (MSGTXT,1030)
         CALL MSGWRT (7)
         WRITE (MSGTXT,1040) SUFQID
         CALL MSGWRT (7)
         WRITE (MSGTXT,1050) FREQID
         CALL MSGWRT (7)
         WRITE (MSGTXT,1060)
         CALL MSGWRT (7)
         IERR = 5
         GO TO 999
         END IF
C                                       Get number of sources.
      NSOURC = BUFFER(5)
C                                       Check if empty
      IF (NSOURC.LE.0) GO TO 800
C                                       Selection by QUAL, CALCODE
      JQUAL = IROUND (XQUAL)
      CCBLNK = XCALCO.EQ.'    '
      CCNBLK = XCALCO.EQ.'*   '
      CCNCAL = XCALCO.EQ.'-CAL'
      CHKCC = .NOT. (CCBLNK.OR.CCNBLK.OR.CHKCC)
      SCBLNK = XSOUCO.EQ.'    '
      SCNBLK = XSOUCO.EQ.'*   '
      SCNCAL = XSOUCO.EQ.'-CAL'
      CHKSC = .NOT. (SCBLNK.OR.SCNBLK.OR.CHKSC)
      DOSWNT = T
      NSOUWD = 0
      ALLSOU = F
      DOCWNT = T
      NCALWD = 0
      ALLCAL = F
      SOUBLK = T
      CALBLK = T
C                                       Check if source excluded
C                                       or if all are included
      DO 30 J = 1,30
C                                       Sources
         IF (XSOUR(J)(1:1).EQ.'*') ALLSOU = .TRUE.
C                                       Check for blanks
         IF ((XSOUR(J).NE.' ') .AND. (XSOUR(J)(1:1).NE.'*')) SOUBLK = F
C                                       Check for "-"
         IF (XSOUR(J)(1:1).EQ.'-') DOSWNT = F
C                                       Calibrators
         IF (XCAL(J)(1:1).EQ.'*') ALLCAL = .TRUE.
C                                       Check for blanks
         IF ((XCAL(J).NE.' ') .AND. (XCAL(J)(1:1).NE.'*')) CALBLK = F
C                                       Check for "-"
         IF (XCAL(J)(1:1).EQ.'-') DOCWNT = F
 30      CONTINUE
      IF ((.NOT.ALLSOU) .AND. SOUBLK) ALLSOU = T
      IF ((.NOT.ALLCAL) .AND. CALBLK) ALLCAL = T
      NSUCAL = 0
C                                       Loop through records, do
C                                       it first for sources
      DO 100 I = 1,NSOURC
         IERR = 1
C                                       Read record
         ISURNO = I
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *      PMRA, PMDEC, JERR)
C                                       Check error
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1020) JERR
            GO TO 990
            END IF
         IERR = 0
C                                       Search lists
         DO 80 J = 1,30
C                                       Name
            I1 = 1
            IF (XSOUR(J)(1:1).EQ.'-') I1 = 2
            EQUAL = ((XSOUR(J)(I1:).EQ.SOUNAM)) .OR. ALLSOU
            EQUAL = EQUAL .AND. DOSWNT
C                                       Qualifier
            ISSEL = (JQUAL.LT.0) .OR. (JQUAL.EQ.QUAL)
C                                       Source 'cal' code
            IF (EQUAL .AND. (.NOT.SCBLNK)) THEN
               IF (SCNBLK) ISSEL = ISSEL .AND. (CALCOD.NE.'    ')
               IF (SCNCAL) ISSEL = ISSEL .AND. (CALCOD.EQ.'    ')
               IF (CHKSC) ISSEL = ISSEL .AND. (CALCOD.EQ.XSOUCO)
               END IF
            IF ((.NOT.EQUAL) .OR. (.NOT.ISSEL)) GO TO 80
C                                       Got one - redundancy check
            IF (NSUCAL.LT.1) GO TO 90
               DO 40 K = 1,NSUCAL
                  IF (SOUNUM(K).EQ.IDSOU) GO TO 100
 40               CONTINUE
C                                       Don't already have
            GO TO 90
 80         CONTINUE
C                                       No match
         GO TO 100
C                                       Add source
 90      IF (NSUCAL.GE.MXSOU) THEN
            WRITE (MSGTXT,1090) MXSOU
            CALL MSGWRT (6)
            GO TO 110
            END IF
C                                       Too many sources
         NSUCAL = NSUCAL + 1
         SOUNUM(NSUCAL) = IDSOU
         ISCAL(NSUCAL) = EQUCAL
         DO 95 IIF = 1,NUMIF
            SUFLX(IIF,NSUCAL) = FLUX(1,IIF)
            IF (SUFLX(IIF,NSUCAL).LE.1.E-10) SUFLX(IIF,NSUCAL) = 1.0
 95         CONTINUE
 100     CONTINUE
C                                       Then do calibrators
      DO 200 I = 1,NSOURC
         IERR = 1
C                                       Read record
         ISURNO = I
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *      PMRA, PMDEC, JERR)
C                                       Check error
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1020) JERR
            GO TO 990
            END IF
         IERR = 0
C                                       Search lists
         DO 180 J = 1,30
C                                       Name
            I1 = 1
            IF (XCAL(J)(1:1).EQ.'-') I1 = 2
            EQUCAL = ((XCAL(J)(I1:).EQ.SOUNAM)) .OR. ALLCAL
            EQUCAL = EQUCAL .AND. DOCWNT
C                                       Qual
            ISSEL = (JQUAL.LT.0) .OR. (JQUAL.EQ.QUAL)
C                                       Calcode
            IF (EQUCAL .AND. (.NOT.CCBLNK)) THEN
               IF(CCNBLK) ISSEL = ISSEL .AND. (CALCOD.NE.'    ')
               IF(CCNCAL) ISSEL = ISSEL .AND. (CALCOD.EQ.'    ')
               IF(CHKCC) ISSEL = ISSEL .AND. (CALCOD.EQ.XCALCO)
               END IF
            IF ((.NOT.EQUCAL) .OR. (.NOT.ISSEL)) GO TO 180
C                                       Got one - redundancy check
            IF (NSUCAL.LT.1) GO TO 190
               DO 140 K = 1,NSUCAL
                  IF (SOUNUM(K).EQ.IDSOU) THEN
                     ISCAL(K) = EQUCAL
                     GO TO 200
                     END IF
 140              CONTINUE
C                                       Don't already have
            GO TO 190
 180        CONTINUE
C                                       No match
         GO TO 200
C                                       Add source
 190     IF (NSUCAL.GE.MXSOU) THEN
            WRITE (MSGTXT,1090) MXSOU
            CALL MSGWRT (6)
            GO TO 110
            END IF
C                                       Too many sources
         NSUCAL = NSUCAL + 1
         NAMSOU(NSUCAL) = SOUNAM
         SOUNUM(NSUCAL) = IDSOU
         ISCAL(NSUCAL) = EQUCAL
         DO 195 IIF = 1,NUMIF
            SUFLX(IIF,NSUCAL) = FLUX(1,IIF)
            IF (SUFLX(IIF,NSUCAL).LE.1.E-10) SUFLX(IIF,NSUCAL) = 1.0
 195        CONTINUE
 200     CONTINUE
C                                       Should have found matches:
 110  IF (NSUCAL.GT.0) GO TO 900
      IERR = 9
      WRITE (MSGTXT,1100)
      GO TO 990
C                                       No SOURCE file
 800  IERR = 10
      WRITE (MSGTXT,1800)
      GO TO 990
C                                       Close file
 900  IF (JERR.LE.0) CALL TABIO ('CLOS', 0, I, BUFFER, BUFFER, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GJYRSU: ERROR',I3,' OPENING SOURCE TABLE')
 1020 FORMAT ('GJYRSU: ERROR',I3,' READING SOURCE TABLE')
 1030 FORMAT ('WARNING - POTENTIALLY FATAL ERROR')
 1040 FORMAT ('   Your calibrators have their fluxes set for FQID ',I3)
 1050 FORMAT ('   You are using them to bootstrap FQID ',I3,' fluxes')
 1060 FORMAT ('   Suggest you rerun SETJY with the correct FREQID')
 1090 FORMAT ('TOO MANY SOURCES, CAN PROCESS ONLY ',I5)
 1100 FORMAT ('GJYRSU: NO SOURCES SPECIFIED FOUND IN SU TABLE')
 1800 FORMAT ('GJYRSU: NO SU TABLE WAS FOUND.')
      END
      SUBROUTINE GJYCSN (NSOU, NANT, SNRECS, MAXSNR, IRET)
C-----------------------------------------------------------------------
C   GJYRSN reads all selected SN tables accumulating count of samples
C   by source and antenna
C   Inputs:
C      NSOU     I        Number of sources
C      NANT     I        Number of antennas
C   Inputs from common:
C      DISKIN   I        Disk number for first file.
C      CNOIN    I        Catalog slot number for first file
C      SOUNUM   I(*)     Source numbers wanted.
C      XTIME    R(8)     Time range (0's => all)
C      XSUBA    R        Subarray number
C      XANT     R        Antennas selected/deselected.
C      CATBLK   I(256)   Catalog header for the first file.
C      SNVER    I        Selected SN table, 0=>all
C   Output:
C      SNRECS   I(*)     Counts of samples (source, ant)
C      MAXSNR   I        Max value in SNRECS
C      IRET     I        Return error code  0 => ok, else failed.
C-----------------------------------------------------------------------
      INTEGER   NSOU, NANT, SNRECS(NSOU,NANT), MAXSNR, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INTEGER   IER, I, NEXT, IANTS(50), LUN, IROUND, BUFFER(512),
     *   NUMSNT, ITAB, IT, SNKOLS(MAXSNC), SNNUMV(MAXSNC), ANTKOL,
     *   TIMKOL, SUBKOL, SOUKOL, FRQKOL, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   J, MAXNUM, RECORD(XCLRSZ), SUID, IANT, SUINDX, NGANT, NRECIN,
     *   ANTS(MAXANT), ISNRNO, LOOP, LIMSN1, LIMSN2
      LOGICAL   T, F, ISAPPL, DESEL, ALLANT, TABLE, EXIST, FITASC
      REAL      GMMOD, RANOD(25), DECNOD(25), RECR(XCLRSZ)
      DOUBLE PRECISION RECD(XCLRSZ/2), DSTART, DEND
      INCLUDE 'GETJY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RECORD, RECR, RECD)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /28/
C-----------------------------------------------------------------------
C                                       Init. no. ant, IF, pol
      NOANT = 0
      NOIF = 0
      NOPOL = 0
      MAXSNR = 0
C                                       Time range
      DSTART = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   XTIME(4) / (24.0*60.0*60.0)
      DEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   XTIME(8) / (24.0*60.0*60.0)
C                                       All times
      IF ((ABS (DSTART) + ABS (DEND)) .LT.1.0E-5) THEN
         DSTART = -1.0D20
         DEND = 1.0D20
         END IF
      TSTART = DSTART
      TEND = DEND
C                                       Interprete antenna numbers.
      ALLANT = T
      DESEL = F
C                                       See if all selected or ant
C                                       deselected.
      MAXNUM = 0
      DO 10 I = 1,50
         IANTS(I) = IROUND (XANT(I))
         ALLANT = ALLANT .AND. (IANTS(I).EQ.0)
         DESEL = DESEL .OR. (IANTS(I).LT.0)
         IF (IANTS(I).NE.0) MAXNUM = I
 10      CONTINUE
      NEXT = 1
C                                       Not all selected - make list
C                                       ANTENNAS array.
      IF (.NOT.ALLANT) THEN
         DO 50 I = 1,MAXANT
C                                       See if ant I wanted.
            DO 30 J = 1,MAXNUM
               IF (I.EQ.ABS(IANTS(J))) THEN
                  IF (DESEL) GO TO 50
                  GO TO 40
                  END IF
 30            CONTINUE
            IF (.NOT.DESEL) GO TO 50
C                                       New antenna
 40         ANTS(NEXT) = I
            NEXT = NEXT + 1
 50         CONTINUE
         END IF
C                                       Number of antennas
      NGANT = NEXT - 1
C                                       Find number of SN tables
      CALL FNDEXT ('SN', CATBLK, NUMSNT)
C                                       No SN tables
      IF (NUMSNT.LE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1080)
         GO TO 990
         END IF
      IRET = 0
C                                       Which SN tables selected?
      IF (SNVER.GT.0) THEN
C                                       Only one
         LIMSN1 = SNVER
         LIMSN2 = SNVER
      ELSE
         LIMSN1 = 1
         LIMSN2 = NUMSNT
         END IF
C                                       Loop over tables
      DO 100 ITAB = LIMSN1,LIMSN2
C                                       If not there skip.
         IT = ITAB
         CALL ISTAB ('SN', DISKIN, CNOIN, IT, LUN, BUFFER, TABLE, EXIST,
     *      FITASC, IER)
         IF (.NOT.EXIST .OR. (IER.NE.0)) GO TO 100
C                                       Open Table
C                                       First check if reformat
         CALL SNREFM (DISKIN, CNOIN, IT, CATBLK, LUN, IER)
         IF (IER.GT.0) GO TO 999
         CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, IT, CATBLK, LUN,
     *      ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *      GMMOD, RANOD, DECNOD, ISAPPL, IER)
         IF (IER.GT.0) THEN
            IRET = 2
            WRITE (MSGTXT,1080) IER, 'OPEN'
            GO TO 990
            END IF
         NOPOL = MAX (NOPOL, NUMPOL)
C                                       Set table pointers
         TIMKOL = SNKOLS(SNDTIM)
         SOUKOL = SNKOLS(SNISID)
         ANTKOL = SNKOLS(SNIANT)
         SUBKOL = SNKOLS(SNISUB)
         FRQKOL = SNKOLS(SNIFQI)
C                                       Get number of records.
         NRECIN = BUFFER(5)
C                                       Antenna etc numbers
         NOANT = MAX (NOANT, NUMANT)
C                                       Read through table
         IRET = 6
         DO 95 LOOP = 1,NRECIN
            ISNRNO = LOOP
            CALL TABIO ('READ', 0, ISNRNO, RECORD, BUFFER, IER)
            IF (IER.GT.0) THEN
               WRITE (MSGTXT,1080) IER, 'READ'
               GO TO 990
               END IF
            IF (IER.LT.0) GO TO 95
C                                       Decide if wanted
C                                       Time
            IF ((RECD(TIMKOL).LT.DSTART) .OR. (RECD(TIMKOL).GT.DEND))
     *         GO TO 95
C                                       Subarray
            IF ((SUBA.NE.0) .AND. (RECORD(SUBKOL).NE.SUBA)) GO TO 95
C                                       Freq id
            IF ((RECORD(FRQKOL).NE.FREQID) .AND. (FREQID.GT.0))
     *         GO TO 95
C                                       Antenna
            IANT = RECORD(ANTKOL)
            IF (.NOT.ALLANT) THEN
               DO 60 I = 1,NGANT
                  IF (IANT.EQ.ANTS(I)) GO TO 65
 60               CONTINUE
               GO TO 95
               END IF
C                                       Source
 65         SUID = RECORD(SOUKOL)
            DO 70 I = 1,NSUCAL
              SUINDX = I
              IF (SUID.EQ.SOUNUM(I)) GO TO 75
 70           CONTINUE
            GO TO 95
C                                       Source wanted, sum solutions.
 75         SNRECS(SUINDX,IANT) = SNRECS(SUINDX,IANT) + 1
            MAXSNR = MAX (MAXSNR, SNRECS(SUINDX,IANT))
 95         CONTINUE
C                                       Close table.
         CALL TABIO ('CLOS', 0, ISNRNO, RECORD, BUFFER, IER)
         IF (IER.GT.0) THEN
            WRITE (MSGTXT,1080) IER, 'CLOS'
            GO TO 990
            END IF
C                                       End table loop
 100     CONTINUE
      IF (MAXSNR.GT.0) THEN
         IRET = 0
      ELSE
         IRET = 10
         MSGTXT = 'NO SN DATA FOUND FOR LISTED SOURCES'
         END IF
C                                       Error
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('ERROR: THERE ARE NO SN TABLES PRESENT')
 1080 FORMAT ('GJYRSN: ERROR ',I3,2X,A4,'ING SN TABLE')
      END
      SUBROUTINE GJYRSN (NSOU, NANT, NP, NIF, SNRECS, SNVALS, IRET)
C-----------------------------------------------------------------------
C   GJYRSN reads all selected SN tables listing the data values
C   Inputs:
C      NSOU     I        Number of sources
C      NANT     I        Number of antennas
C      NP       I        Number polarizations
C      NIF      I        Number of IFs
C   Inputs from common:
C      DISKIN   I        Disk number for first file.
C      CNOIN    I        Catalog slot number for first file
C      SOUNUM   I(*)     Source numbers wanted.
C      XTIME    R(8)     Time range (0's => all)
C      XSUBA    R        Subarray number
C      XANT     R        Antennas selected/deselected.
C      CATBLK   I(256)   Catalog header for the first file.
C      SNVER    I        Selected SN table, 0=>all
C   Output:
C      SNRECS   I(*)     Counts of samples (source, ant)
C      SNVALS   R(*)     Samples (source, ant, pol, IF, *)
C      IRET     I        Return error code  0 => ok, else failed.
C-----------------------------------------------------------------------
      INTEGER   NSOU, NANT, NP, NIF, SNRECS(NSOU,NANT), IRET
      REAL      SNVALS(NSOU,NANT,NP,NIF,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INTEGER   IER, I, NEXT, IANTS(50), LUN, IBIF, IEIF, IROUND,
     *   BUFFER(512), NUMSNT, ITAB, IT, SNKOLS(MAXSNC), SNNUMV(MAXSNC),
     *   ANTKOL, TIMKOL, SUBKOL, SOUKOL, RE1KOL, RE2KOL, IM1KOL, IM2KOL,
     *   WT1KOL, WT2KOL, FRQKOL, NUMANT, NUMPOL, NUMIF, NUMNOD, J, JJ,
     *   MAXNUM, RECORD(XCLRSZ), SUID, IANT, SUINDX, NGANT, NRECIN,
     *   ANTS(MAXANT), ISNRNO, LOOP, LIMSN1, LIMSN2
      LOGICAL   T, F, ISAPPL, DESEL, ALLANT, TABLE, EXIST, FITASC, NBLNK
      REAL      GMMOD, RANOD(25), DECNOD(25), RECR(XCLRSZ), AMP, RCR,
     *   RCI
      DOUBLE PRECISION RECD(XCLRSZ/2), DSTART, DEND
      INCLUDE 'GETJY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RECORD, RECR, RECD)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /28/
C-----------------------------------------------------------------------
C                                       Init. no. ant, IF, pol
      NOANT = 0
      NOIF = 0
      NOPOL = 0
C                                       Time range
      DSTART = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   XTIME(4) / (24.0*60.0*60.0)
      DEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   XTIME(8) / (24.0*60.0*60.0)
C                                       All times
      IF ((ABS (DSTART) + ABS (DEND)) .LT.1.0E-5) THEN
         DSTART = -1.0D20
         DEND = 1.0D20
         END IF
      TSTART = DSTART
      TEND = DEND
C                                       Interprete antenna numbers.
      ALLANT = T
      DESEL = F
C                                       See if all selected or ant
C                                       deselected.
      MAXNUM = 0
      DO 10 I = 1,50
         IANTS(I) = IROUND (XANT(I))
         ALLANT = ALLANT .AND. (IANTS(I).EQ.0)
         DESEL = DESEL .OR. (IANTS(I).LT.0)
         IF (IANTS(I).NE.0) MAXNUM = I
 10      CONTINUE
      NEXT = 1
C                                       Not all selected - make list
C                                       ANTENNAS array.
      IF (.NOT.ALLANT) THEN
         DO 50 I = 1,MAXANT
C                                       See if ant I wanted.
            DO 30 J = 1,MAXNUM
               IF (I.EQ.ABS(IANTS(J))) THEN
                  IF (DESEL) GO TO 50
                  GO TO 40
                  END IF
 30            CONTINUE
            IF (.NOT.DESEL) GO TO 50
C                                       New antenna
 40         ANTS(NEXT) = I
            NEXT = NEXT + 1
 50         CONTINUE
         END IF
C                                       Number of antennas
      NGANT = NEXT - 1
C                                       Find number of SN tables
      CALL FNDEXT ('SN', CATBLK, NUMSNT)
C                                       No SN tables
      IF (NUMSNT.LE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1080)
         GO TO 990
         END IF
      IRET = 0
C                                       Only one
      IF (SNVER.GT.0) THEN
         LIMSN1 = SNVER
         LIMSN2 = SNVER
C                                       all SN tables
      ELSE
         LIMSN1 = 1
         LIMSN2 = NUMSNT
         END IF
C                                       Loop over tables
      DO 100 ITAB = LIMSN1,LIMSN2
C                                       If not there skip.
         IT = ITAB
         CALL ISTAB ('SN', DISKIN, CNOIN, IT, LUN, BUFFER, TABLE, EXIST,
     *      FITASC, IER)
         IF (.NOT.EXIST .OR. (IER.NE.0)) GO TO 100
C                                       Open Table
C                                       First check if reformat
         CALL SNREFM (DISKIN, CNOIN, IT, CATBLK, LUN, IER)
         IF (IER.GT.0) GO TO 999
         CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, IT, CATBLK, LUN,
     *      ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *      GMMOD, RANOD, DECNOD, ISAPPL, IER)
         IF (IER.GT.0) THEN
            IRET = 2
            WRITE (MSGTXT,1080) IER, 'OPEN'
            GO TO 990
            END IF
C                                       Set table pointers
         TIMKOL = SNKOLS(SNDTIM)
         SOUKOL = SNKOLS(SNISID)
         ANTKOL = SNKOLS(SNIANT)
         SUBKOL = SNKOLS(SNISUB)
         FRQKOL = SNKOLS(SNIFQI)
         RE1KOL = SNKOLS(SNRRE1)
         IM1KOL = SNKOLS(SNRIM1)
         WT1KOL = SNKOLS(SNRWE1)
         RE2KOL = SNKOLS(SNRRE2)
         IM2KOL = SNKOLS(SNRIM2)
         WT2KOL = SNKOLS(SNRWE2)
C                                       Get number of records.
         NRECIN = BUFFER(5)
C                                       Antenna etc numbers
         NOANT = MAX (NOANT, NUMANT)
         NOIF = MAX (NOIF, NUMIF)
         NOPOL = MAX (NOPOL, NUMPOL)
C                                       IF range
         IBIF = MIN (BIF, NUMIF)
         IEIF = MIN (EIF, NUMIF)
C                                       Read through table
         IRET = 6
         DO 95 LOOP = 1,NRECIN
            ISNRNO = LOOP
            CALL TABIO ('READ', 0, ISNRNO, RECORD, BUFFER, IER)
            IF (IER.GT.0) THEN
               WRITE (MSGTXT,1080) IER, 'READ'
               GO TO 990
               END IF
            IF (IER.LT.0) GO TO 95
C                                       Decide if wanted
C                                       Time
            IF ((RECD(TIMKOL).LT.DSTART) .OR. (RECD(TIMKOL).GT.DEND))
     *         GO TO 95
C                                       Subarray
            IF ((SUBA.NE.0) .AND. (RECORD(SUBKOL).NE.SUBA)) GO TO 95
C                                       Freq id
            IF ((RECORD(FRQKOL).NE.FREQID) .AND. (FREQID.GT.0))
     *         GO TO 95
C                                       Antenna
            IANT = RECORD(ANTKOL)
            IF (.NOT.ALLANT) THEN
               DO 60 I = 1,NGANT
                  IF (IANT.EQ.ANTS(I)) GO TO 65
 60               CONTINUE
               GO TO 95
               END IF
C                                       Source
 65         SUID = RECORD(SOUKOL)
            DO 70 I = 1,NSUCAL
              SUINDX = I
              IF (SUID.EQ.SOUNUM(I)) GO TO 75
 70           CONTINUE
            GO TO 95
C                                       Source wanted, sum solutions.
 75         SNRECS(SUINDX,IANT) = SNRECS(SUINDX,IANT) + 1
            JJ = SNRECS(SUINDX,IANT)
            DO 80 I = IBIF,IEIF
               RCR = RECR(RE1KOL+I-1)
               RCI = RECR(IM1KOL+I-1)
               NBLNK = (RCR.NE.FBLANK) .AND. (RCI.NE.FBLANK)
               AMP = -1.0
               IF ((RECR(WT1KOL+I-1).GT.1.0) .AND. (NBLNK))
     *            AMP =  (RCR * RCR + RCI * RCI) / SUFLX(I,SUINDX)
               IF (AMP.GT.0.0) THEN
                  SNVALS(SUINDX,IANT,1,I,JJ) = AMP
               ELSE
                  SNVALS(SUINDX,IANT,1,I,JJ) = FBLANK
                  END IF
 80            CONTINUE
            IF (NUMPOL.GT.1) THEN
               DO 90 I = IBIF,IEIF
                  RCR = RECR(RE2KOL+I-1)
                  RCI = RECR(IM2KOL+I-1)
                  NBLNK = (RCR.NE.FBLANK) .AND. (RCI.NE.FBLANK)
                  AMP = -1.0
                  IF ((RECR(WT2KOL+I-1).GT.1.0) .AND. (NBLNK))
     *               AMP = (RCR * RCR + RCI * RCI) / SUFLX(I,SUINDX)
                  IF (AMP.GT.0.0) THEN
                     SNVALS(SUINDX,IANT,2,I,JJ) = AMP
                  ELSE
                     SNVALS(SUINDX,IANT,2,I,JJ) = FBLANK
                     END IF
 90               CONTINUE
               END IF
 95        CONTINUE
C                                       Close table.
         CALL TABIO ('CLOS', 0, ISNRNO, RECORD, BUFFER, IER)
         IF (IER.GT.0) THEN
            WRITE (MSGTXT,1080) IER, 'CLOS'
            GO TO 990
            END IF
C                                       End table loop
 100     CONTINUE
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('ERROR: THERE ARE NO SN TABLES PRESENT')
 1080 FORMAT ('GJYRSN: ERROR ',I3,2X,A4,'ING SN TABLE')
      END
      SUBROUTINE GJYFLX (NSOU, NANT, NP, NIF, SNRECS, SNVALS, IRET)
C-----------------------------------------------------------------------
C   GJYFLX computes source flux densities.
C   Inputs:
C      NSOU     I        Number of sources
C      NANT     I        Number of antennas
C      NP       I        Number polarizations
C      NIF      I        Number of IFs
C      SNRECS   I(*)     Counts of samples (source, ant)
C      SNVALS   R(*)     Samples (source, ant, pol, IF, *)
C   Inputs from common:
C      ISCAL    L(*)     True if source a calibrator.
C   Input/output in common:
C      SUFLX    R(*)     Flux densities (IF,source)
C   Output:
C      IRET     I        Return error code  0 => ok, else failed.
C-----------------------------------------------------------------------
      INTEGER   NSOU, NANT, NP, NIF, SNRECS(NSOU,NANT), IRET
      REAL      SNVALS(NSOU,NANT,NP,NIF,*)
C
      INTEGER   I, J, K, L, M, SUINDX, COUNT, ISOUR, ISLUN, LG, LB, LT,
     *   NCBAD, NCGOOD, NCTOT, NSGOOD, NSBAD, NSTOT, NFGOOD, NFBAD,
     *   NFTOT, WARN
      LOGICAL   FIRST
      DOUBLE PRECISION SUM, SUM2, RMS, AV, RMM(10), VV, SUMB2
      INCLUDE 'INCS:PUVD.INC'
      REAL      RCAL(MAXANT,MAXIF,2), ICAL(MAXANT,MAXIF,2),
     *   SCAL(MAXANT,2), JCAL(MAXANT,2), V
      INCLUDE 'GETJY.INC'
      INTEGER   NSB(MAXSOU), NSG(MAXSOU), NFG(MAXSOU), NFB(MAXSOU),
     *   NST(MAXSOU), NFT(MAXSOU)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA ISLUN /29/
      DATA RMM /6.D0, 5.D0, 4.D0, 6*3.D0, 6.D0/
C-----------------------------------------------------------------------
      DO 20 I = BIF,EIF
         DO 10 J = 1,MAXANT
            RCAL(J,I,1) = 0.0
            RCAL(J,I,2) = 0.0
            ICAL(J,I,1) = 0.0
            ICAL(J,I,2) = 0.0
 10         CONTINUE
 20      CONTINUE
C                                       Compute receiver cal factors
C                                       in Jy/whatever
      COUNT = 0
      NCBAD = 0
      NCGOOD = 0
 4    DO 100 ISOUR = 1,NSUCAL
         IF (ISCAL(ISOUR)) THEN
            DO 90 I = BIF,EIF
               DO 80 K = 1,NP
                  DO 70 J = 1,NOANT
C                                       average robustly
                     AV = 0.0D0
                     RMS = 1.D8
                     DO 40 L = 1,7
                        SUM = 0.0D0
                        SUM2 = 0.0D0
                        LG = 0
                        LB = 0
                        LT = 0
                        DO 30 M = 1,SNRECS(ISOUR,J)
                           V = SNVALS(ISOUR,J,K,I,M)
                           IF (V.NE.FBLANK) THEN
                              LT = LT + 1
                              VV = V
                              IF (ABS(VV-AV).LE.RMM(L)*RMS) THEN
                                 LG = LG + 1
                                 SUM = SUM + VV
                                 SUM2 = SUM2 + VV * VV
                              ELSE IF (ABS(V-AV).GT.RMM(10)*RMS) THEN
                                 LB = LB + 1
                                 END IF
                              END IF
 30                        CONTINUE
                        IF (LG.LE.0) GO TO 70
                        AV = SUM / LG
                        RMS = SUM2 / LG - AV * AV
                        RMS = SQRT (MAX (0.0D0, RMS))
 40                     CONTINUE
                     RMS = RMS * SUFLX(I,ISOUR)
                     AV = AV * SUFLX(I,ISOUR)
                     RMS = 1.
                     IF (RMS.GT.0.0) THEN
                        RMS = 1.0D0 / (RMS * RMS)
                     ELSE
                        RMS = 100.0D0
                        END IF
                     ICAL(J,I,K) = ICAL(J,I,K) + RMS
                     RCAL(J,I,K) = RCAL(J,I,K) + RMS * AV
                     NCBAD = NCBAD + LB
                     NCGOOD = NCGOOD + LG
                     NCTOT = NCTOT + LT
 70                  CONTINUE
 80               CONTINUE
 90            CONTINUE
            END IF
 100     CONTINUE
      COUNT = NCGOOD
C                                       No calibrator info.
      IF (COUNT.LE.0) THEN
         IRET = 5
         MSGTXT = 'NO VALID CALIBRATOR SOLUTIONS AND/OR FLUX ' //
     *      'DENSITIES FOUND'
         CALL MSGWRT (8)
         GO TO 999
      ELSE
         WRITE (MSGTXT,1100) NCGOOD, NCTOT
         CALL MSGWRT (4)
         WRITE (MSGTXT,1101) NCBAD
         IF (NCBAD.GT.0) CALL MSGWRT (4)
         END IF
C                                       Average
      DO 120 I = BIF,EIF
         DO 110 J = 1,MAXANT
            IF (ICAL(J,I,1).GT.0)
     *         RCAL(J,I,1) = RCAL(J,I,1) / ICAL(J,I,1)
            IF (ICAL(J,I,2).GT.0)
     *         RCAL(J,I,2) = RCAL(J,I,2) / ICAL(J,I,2)
 110        CONTINUE
 120     CONTINUE
C                                       Label
      WRITE (MSGTXT,1120)
      CALL MSGWRT (4)
C                                       calibrate unknowns
      DO 200 ISOUR = 1,NSUCAL
         IF (.NOT.ISCAL(ISOUR)) THEN
C                                       Get source name
            SUINDX = SOUNUM(ISOUR)
            CALL GETSOU (SUINDX, DISKIN, CNOIN, CATBLK, ISLUN, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       zero overal counters
            NSBAD = 0
            NSGOOD = 0
            NSTOT = 0
            NFBAD = 0
            NFGOOD = 0
            NFTOT = 0
            FIRST = .TRUE.
C                                       find flux 1 IF at a time
            DO 190 I = BIF,EIF
               CALL RFILL (2*MAXANT, 0.0, SCAL)
               CALL RFILL (2*MAXANT, 0.0, JCAL)
C                                       average robustly by pol, antenna
               DO 160 K = 1,NP
                  DO 150 J = 1,NOANT
C                                       robustly
                     AV = 0.0D0
                     RMS = 1.D8
                     DO 140 L = 1,7
                        SUM = 0.0D0
                        SUM2 = 0.0D0
                        LG = 0
                        LB = 0
                        LT = 0
                        DO 130 M = 1,SNRECS(ISOUR,J)
                           V = SNVALS(ISOUR,J,K,I,M)
                           IF (V.NE.FBLANK) THEN
                              LT = LT + 1
                              VV = V
                              IF (ABS(VV-AV).LE.RMM(L)*RMS) THEN
                                 LG = LG + 1
                                 SUM = SUM + VV
                                 SUM2 = SUM2 + VV * VV
                              ELSE IF (ABS(VV-AV).GT.RMM(10)*RMS) THEN
                                 LB = LB + 1
                                 END IF
                              END IF
 130                       CONTINUE
                        IF (LG.LE.0) GO TO 150
                        AV = SUM / LG
                        RMS = SUM2 / LG - AV * AV
                        RMS = SQRT (MAX (0.0D0, RMS))
 140                    CONTINUE
                     IF (RMS.GT.0.0) THEN
                        RMS = 1.0D0 / (RMS * RMS)
                     ELSE
                        RMS = 100.0D0
                        END IF
C                                       sum ratio with cal
                     IF (AV.GT.0.0) THEN
                        JCAL(J,K) = JCAL(J,K) + RMS
                        SCAL(J,K) = SCAL(J,K) + RMS * RCAL(J,I,K) / AV
                        NSBAD = NSBAD + LB
                        NSGOOD = NSGOOD + LG
                        NSTOT = NSTOT + LT
                        END IF
 150                 CONTINUE
 160              CONTINUE
C                                       average
               DO 170 K = 1,NP
                  DO 165 J = 1,NOANT
                     IF (JCAL(J,K).GT.0.0) SCAL(J,K) = SCAL(J,K) /
     *                  JCAL(J,K)
 165                 CONTINUE
 170              CONTINUE
C                                       robust average across pol, ant
               AV = 0.0D0
               RMS = 1.D8
               DO 185 L = 1,7
                  SUM = 0.0D0
                  SUM2 = 0.0D0
                  SUMB2 = 0.0D0
                  LG = 0
                  LB = 0
                  LT = 0
                  DO 180 K = 1,NP
                     DO 175 J = 1,NOANT
                        VV = SCAL(J,K)
                        IF (VV.GT.0.0D0) THEN
                           LT = LT + 1
                           SUMB2 = SUMB2 + (VV-AV) * (VV-AV)
                           IF (ABS(VV-AV).LE.RMM(L)*RMS) THEN
                              LG = LG + 1
                              SUM = SUM + VV
                              SUM2 = SUM2 + VV * VV
                           ELSE IF (ABS(VV-AV).GT.RMM(10)*RMS) THEN
                              LB = LB + 1
                              END IF
                           END IF
 175                    CONTINUE
 180                 CONTINUE
                  IF (LG.GT.0) THEN
                     AV = SUM / LG
                     RMS = SUM2 / LG - AV * AV
                     RMS = SQRT (MAX (0.0D0, RMS))
                     END IF
                  IF (LT.GT.0) THEN
                     SUMB2 = SUMB2 / LT
                     SUMB2 = SQRT (MAX (0.0D0, SUMB2))
                     END IF
 185              CONTINUE
               NFBAD = NFBAD + LB
               NFGOOD = NFGOOD + LG
               NFTOT = NFTOT + LT
               RMS = RMS / SQRT (MAX (1.0D0, LG - 1.0D0))
               SUMB2 = SUMB2 / SQRT (MAX (1.0D0, LG - 1.0D0))
               SUSAV(1,I,ISOUR) = AV
               SUSAV(2,I,ISOUR) = RMS
               SUSAV(3,I,ISOUR) = SUMB2
               SUFLX(I,ISOUR) = AV
               IF (FIRST) THEN
                  WRITE (MSGTXT,1185) SNAME, QUAL, CALCOD, I, AV, RMS,
     *               SUMB2
               ELSE
                  WRITE (MSGTXT,1186) I, AV, RMS, SUMB2
                  END IF
               CALL MSGWRT (4)
               FIRST = .FALSE.
 190           CONTINUE
            NSB(ISOUR) = NSBAD
            NSG(ISOUR) = NSGOOD
            NST(ISOUR) = NSTOT
            NFB(ISOUR) = NFBAD
            NFG(ISOUR) = NFGOOD
            NFT(ISOUR) = NFTOT
            END IF
 200     CONTINUE
C                                       summaries
      WRITE (MSGTXT,1200)
      CALL MSGWRT (4)
      WARN = 0
      DO 220 ISOUR = 1,NSUCAL
         IF (.NOT.ISCAL(ISOUR)) THEN
            WARN = WARN + EIF - BIF + 1
            DO 210 I = BIF,EIF
               IF ((SUSAV(1,I,ISOUR).GT.0.0) .AND.
     *            (SUSAV(1,I,ISOUR).GT.0.0)) WARN = WARN - 1
 210           CONTINUE
C                                       Get source name
            SUINDX = SOUNUM(ISOUR)
            CALL GETSOU (SUINDX, DISKIN, CNOIN, CATBLK, ISLUN, IRET)
            IF (IRET.NE.0) GO TO 999
            NAMSOU(ISOUR) = SNAME
            WRITE (MSGTXT,1210) SNAME, QUAL, CALCOD, NSG(ISOUR),
     *         NST(ISOUR), NSB(ISOUR), NFG(ISOUR), NFT(ISOUR),
     *         NFB(ISOUR)
            CALL MSGWRT (4)
            END IF
 220     CONTINUE
C                                       Give warning if didn't find all
C                                       fluxes.
      IF (WARN.GT.0) THEN
         MSGTXT = 'WARNING: All requested fluxes were NOT determined'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Calibrator robust averaging used',I7,' of',I7,
     *   ' gain samples')
 1101 FORMAT (I10,' samples were more than 6 * rms out')
 1120 FORMAT ('   Source:Qual      ','CALCODE',' IF ',' Flux (Jy)')
 1185 FORMAT (A16,':',I3,3X,A4,I3,1X,F10.5,' +/- ',2F9.5)
 1186 FORMAT (27X,I3,1X,F10.5,' +/- ',2F9.5)
 1200 FORMAT ('   Source:Qual    ','CALCODE',4X,'used',3X,'total',
     *   5X,'bad',' used  tot  bad')
 1210 FORMAT (A16,':',I3,1X,A4,3I8,3I5)
      END
      SUBROUTINE GJYWSU (IERR)
C-----------------------------------------------------------------------
C   Inserts derived source flux densities into the SU table.  The
C   values returned in SUFLX are the appropriate values to multiply
C   the real and imaginary part of the SN table gains to correct to the
C   the new flux density.
C   Inputs from common:
C      DISKIN      I    Input disk number.
C      CNOIN       I    Catalog slot number
C      NSUCAL      I    Number of sources plus calibrators
C      SOUNUM(*)   I    Source ID numbers of sources and calibrators.
C      ISCAL(*)    L    True if source a calibrator don't update.
C   Input from common /MAPHDR/
C      CATBLK(256) I    Catalog header record.
C   Input/Output:
C      SUFLX(if,*) R    On input ,Source IPOL flux densities 1/IF;
C                       on output, the factor needed to correct the
C                       gains in the SN table.
C      BUFF1(*)    I    I/O Buffer for source (SU) table
C   Output:
C      IERR         I    Return code, 0=>OK, otherwise failed.
C   Note: uses AIPS LUN 29.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELDEF*8, VELTYP*8, CALCOD*4, SOUNAM*16
      INTEGER   IDSOU, SUKOLS(MAXSUC), SUNUMV(MAXSUC), SUFQID,
     *   QUAL, NUMIF, J, BUFFER(512), SUINDX, ISLUN, NSOURC, I, ISURNO
      REAL      OLDFLX, FLUX(4,MAXIF)
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, LSRVEL(MAXIF), FREQO(MAXIF), LRESTF(MAXIF), RAOBS,
     *   DECOBS
      INCLUDE 'GETJY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (BUFFER, BUFF1)
      DATA ISLUN /29/
C-----------------------------------------------------------------------
C                                       Open SU table, Read for Keywords
      CALL SOUINI ('READ', BUFFER, DISKIN, CNOIN, 1, CATBLK, ISLUN,
     *   NUMIF, VELDEF, VELTYP, SUFQID, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Close
      CALL TABIO ('CLOS', 0, I, BUFFER, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open SU table
      CALL SOUINI ('WRIT', BUFFER, DISKIN, CNOIN, 1, CATBLK, ISLUN,
     *   NUMIF, VELDEF, VELTYP, SUFQID, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Get number of sources.
      NSOURC = BUFFER(5)
C                                       Loop through records
      DO 100 I = 1,NSOURC
         ISURNO = I
C                                       Read record
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *      PMRA, PMDEC, IERR)
C                                       Check error
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1020) IERR, 'READ'
            GO TO 990
            END IF
C                                       Search list
         DO 50 J = 1,NSUCAL
            SUINDX = J
            IF ((.NOT.ISCAL(J)) .AND. (IDSOU.EQ.SOUNUM(J))) GO TO 60
 50         CONTINUE
C                                       No match
         GO TO 100
C                                       Update
 60      DO 70 J = BIF,EIF
            OLDFLX = FLUX(1,J)
            IF (SUFLX(J,SUINDX).GT.1.0E-20) FLUX(1,J) = SUFLX(J,SUINDX)
C                                       Correction factor for SN tables
            IF (OLDFLX.GT.1.0E-20) SUFLX(J,SUINDX) =
     *         FLUX(1,J) / OLDFLX
            SUFLX(J,SUINDX) = SQRT (SUFLX(J,SUINDX))
C                                       Use 1 if no cal info
            IF (SUFLX(J,SUINDX).LT.1.0E-20) SUFLX(J,SUINDX) = 1.0
 70         CONTINUE
C                                       Rewrite entry
         ISURNO = I
         CALL TABSOU ('WRIT', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, IERR)
C                                       Check error
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1020) IERR, 'WRIT'
            GO TO 990
            END IF
 100     CONTINUE
C                                       Set calibrator factors to 1.0
      DO 140 I = 1,NSUCAL
         IF (ISCAL(I)) THEN
            DO 120 J = 1,NUMIF
               SUFLX(J,I) = 1.0
 120           CONTINUE
            END IF
 140     CONTINUE
C                                       Close file
 900  CALL TABIO ('CLOS', 0, I, BUFFER, BUFFER, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GJYWSU: ERROR',I3,' OPENING SOURCE TABLE')
 1020 FORMAT ('GJYWSU: ERROR',I3,1X,A4,'ING SOURCE TABLE')
      END
      SUBROUTINE GJYWSN (IRET)
C-----------------------------------------------------------------------
C   GJYWSN updates all SN tables correcting for new flux densities.
C   Inputs from common:
C    DISKIN     I     Disk number for first file.
C    CNOIN      I     Catalog slot number for first file
C    SOUNUM(*)  I     Source numbers wanted.
C    XSUBA      R     Subarray number
C    SUFLX(if,*)R     The amplitude calibration factor for each source
C                     and IF.
C    CATBLK(256)I     Catalog header for the first file.
C    SNVER      I     Selected SN table, 0=>all
C   Output:
C    BUFF1(*)   R     Work buffer
C    IRET       I     Return error code  0 => ok, else failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INTEGER   IER, I, LUN, IBIF, IEIF,
     *   BUFFER(2048), NUMSNT, ITAB, SNKOLS(MAXSNC), IT,
     *   SNNUMV(MAXSNC), ANTKOL, TIMKOL, SUBKOL, SOUKOL, RE1KOL, RE2KOL,
     *   IM1KOL, IM2KOL, WT1KOL, WT2KOL, FRQKOL,
     *   NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   RECORD(2048), SUID, SUINDX, NRECIN, ISNRNO, LOOP, LIMSN1,
     *   LIMSN2
      LOGICAL   ISAPPL, TABLE, EXIST, FITASC, NBLNK
      REAL      GMMOD,  RANOD(25), DECNOD(25), RECR(2048),RCR,RCI
      DOUBLE PRECISION    RECD(1024)
      INCLUDE 'GETJY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (BUFF1, BUFFER), (RECORD, RECR, RECD)
      DATA LUN /28/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Find number of SN tables
      CALL FNDEXT ('SN', CATBLK, NUMSNT)
C                                       Which SN tables selected?
C                                       Only one
      IF (SNVER.GT.0) THEN
         LIMSN1 = SNVER
         LIMSN2 = SNVER
      ELSE
         LIMSN1 = 1
         LIMSN2 = NUMSNT
         END IF
C                                       Loop over tables
      DO 600 ITAB = LIMSN1,LIMSN2
         IT = ITAB
C                                       If not there skip.
         CALL ISTAB ('SN', DISKIN, CNOIN, IT, LUN, BUFFER, TABLE, EXIST,
     *      FITASC, IER)
         IF (.NOT.EXIST .OR. (IER.NE.0)) GO TO 600
C                                       reformat if needed
         CALL SNREFM (DISKIN, CNOIN, IT, CATBLK, LUN, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'REFORMAT'
            CALL MSGWRT (7)
            END IF
C                                       Open Table for keywords
         CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, IT, CATBLK, LUN,
     *      ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *      GMMOD, RANOD, DECNOD, ISAPPL, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1080) IRET, 'OPEN'
            GO TO 990
            END IF
C                                       Close table.
         CALL TABIO ('CLOS', 0, ISNRNO, RECORD, BUFFER, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1080) IRET, 'CLOS'
            GO TO 990
            END IF
C                                       Open for write
         IT = ITAB
         CALL SNINI ('WRIT', BUFFER, DISKIN, CNOIN, IT, CATBLK, LUN,
     *      ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *      GMMOD, RANOD, DECNOD, ISAPPL, IRET)
C                                       Error check
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1080) IRET, 'OPEN'
            GO TO 990
            END IF
C                                       Set table pointers
         TIMKOL = SNKOLS(SNDTIM)
         SOUKOL = SNKOLS(SNISID)
         ANTKOL = SNKOLS(SNIANT)
         SUBKOL = SNKOLS(SNISUB)
         FRQKOL = SNKOLS(SNIFQI)
         RE1KOL = SNKOLS(SNRRE1)
         IM1KOL = SNKOLS(SNRIM1)
         WT1KOL = SNKOLS(SNRWE1)
         RE2KOL = SNKOLS(SNRRE2)
         IM2KOL = SNKOLS(SNRIM2)
         WT2KOL = SNKOLS(SNRWE2)
C                                       Get number of records.
         NRECIN = BUFFER(5)
C                                       IF range
         IBIF = MIN (BIF, NUMIF)
         IEIF = MIN (EIF, NUMIF)
C                                       Read through table
         DO 400 LOOP = 1,NRECIN
            ISNRNO = LOOP
            CALL TABIO ('READ', 0, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1080) IRET, 'READ'
               GO TO 990
               END IF
C                                       Decide if wanted:
C                                       Freq id
            IF ((RECORD(FRQKOL).NE.FREQID) .AND. (FREQID.GT.0))
     *         GO TO 400
C                                       Subarray
            IF ((SUBA.NE.0) .AND. (RECORD(SUBKOL).NE.SUBA)) GO TO 400
            SUID = RECORD(SOUKOL)
            DO 180 I = 1,NSUCAL
              SUINDX = I
              IF (SUID.EQ.SOUNUM(I)) GO TO 200
 180          CONTINUE
C                                       Don't have source
            GO TO 400
 200        IF (.NOT.ISCAL(SUINDX)) THEN
C                                       Correct gains
               DO 220 I = BIF,EIF
                  RCR = RECR(RE1KOL+I-1)
                  RCI = RECR(IM1KOL+I-1)
                  NBLNK = RCR.NE.FBLANK.AND.RCI.NE.FBLANK
                  IF (RECR(WT1KOL+I-1).GT.1.0 .AND. NBLNK) THEN
                     RECR(RE1KOL+I-1) = RCR * SUFLX(I,SUINDX)
                     RECR(IM1KOL+I-1) = RCI * SUFLX(I,SUINDX)
                     END IF
 220              CONTINUE
               IF (NUMPOL.GT.1) THEN
                  DO 240 I = BIF,EIF
                     RCR = RECR(RE2KOL+I-1)
                     RCI = RECR(IM2KOL+I-1)
                     NBLNK = RCR.NE.FBLANK.AND.RCI.NE.FBLANK
                     IF (RECR(WT2KOL+I-1).GT.1.0 .AND. NBLNK) THEN
                        RECR(RE2KOL+I-1) = RCR * SUFLX(I,SUINDX)
                        RECR(IM2KOL+I-1) = RCI * SUFLX(I,SUINDX)
                        END IF
 240                 CONTINUE
                  END IF
               END IF
            ISNRNO = LOOP
            CALL TABIO ('WRIT', 0, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1080) IRET, 'WRIT'
               GO TO 990
               END IF
 400        CONTINUE
C                                       Close table.
         CALL TABIO ('CLOS', 0, ISNRNO, RECORD, BUFFER, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1080) IRET, 'CLOS'
            GO TO 990
            END IF
C                                       End table loop
 600     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('ERROR: THERE ARE NO SN TABLES PRESENT')
 1080 FORMAT ('GJYWSN: ERROR ',I3,2X,A4,'ING SN TABLE')
      END
      SUBROUTINE GETJHI
C-----------------------------------------------------------------------
C   GETJHI updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, CTIME(2)*12
      INTEGER   IERR, I, J, LUN, TIME(3), DATE(3), NSOUWD, NCALWD,
     *   IQUAL, ISUBA, IANT(50), NANTSL,  LIMIT, LIMIT2, I1, I2
      LOGICAL   T, DOSWNT, DOCWNT, DOAWNT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'GETJY.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN /28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN, DISKIN, CNOIN, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
C                                       Task message
10    CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       file name
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN, BUFF1,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Source list
      DOSWNT = T
      NSOUWD = 0
      DO 20 I = 1,30
         IF (XSOUR(I)(1:1).EQ.'-') DOSWNT = .FALSE.
         IF (XSOUR(I).NE.' ') NSOUWD = I
 20      CONTINUE
C                                       Sources
      IF (NSOUWD.LE.0) THEN
         WRITE (HILINE,3000) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
C                                       Included or excluded?
      ELSE
         WRITE (HILINE,3001) TSKNAM
         IF (DOSWNT) WRITE (HILINE,3002) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       1st 2 and label.
         I1 = 1
         I2 = 1
         IF (XSOUR(1)(1:1).EQ.'-') I1 = 2
         IF (XSOUR(2)(1:1).EQ.'-') I2 = 2
         WRITE (HILINE,3003) TSKNAM, XSOUR(1)(I1:), XSOUR(2)(I2:)
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Rest of sources
         DO 30 I = 3,NSOUWD,2
            I1 = 1
            I2 = 1
            IF (XSOUR(I)(1:1).EQ.'-') I1 = 2
            IF (XSOUR(I+1)(1:1).EQ.'-') I2 = 2
            WRITE (HILINE,3004) TSKNAM, XSOUR(I)(I1:), XSOUR(I+1)(I2:)
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
 30         CONTINUE
         END IF
C                                       Source "Cal code"
      WRITE (HILINE,4004) TSKNAM, XSOUCO
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Calibrators
 40   DOCWNT = T
      NCALWD = 0
      DO 50 I = 1,30
         IF (XCAL(I)(1:1).EQ.'-') DOCWNT = .FALSE.
         IF (XCAL(I).NE.' ') NCALWD = I
 50      CONTINUE
      IF (NCALWD.LE.0) THEN
         WRITE (HILINE,3005) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
C                                       Included or excluded?
      ELSE
         WRITE (HILINE,3006) TSKNAM
         IF (DOCWNT) WRITE (HILINE,3007) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       1st 2 and label.
         I1 = 1
         I2 = 1
         IF (XCAL(1)(1:1).EQ.'-') I1 = 2
         IF (XCAL(2)(1:1).EQ.'-') I2 = 2
         WRITE (HILINE,3008) TSKNAM, XCAL(1)(I1:), XCAL(2)(I2:)
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Rest of calibrators
         DO 60 I = 3,NCALWD,2
            I1 = 1
            I2 = 1
            IF (XCAL(I)(1:1).EQ.'-') I1 = 2
            IF (XCAL(I+1)(1:1).EQ.'-') I2 = 2
            WRITE (HILINE,3009) TSKNAM, XCAL(I)(I1:), XCAL(I+1)(I2:)
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
 60         CONTINUE
         END IF
C                                       QUAL, CALCODE
      IQUAL = XQUAL + 0.1
      WRITE (HILINE,3010) TSKNAM, IQUAL, XCALCO
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       BIF,EIF
      WRITE (HILINE,3016) TSKNAM, BIF, EIF
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       TIMERANG
      CALL HITIME (TSTART, TEND, LUN, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Subarray
      ISUBA = XSUBA + 0.1
      WRITE (HILINE,2003) TSKNAM, ISUBA
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Antennas
      NANTSL = 0
      DOAWNT = T
      DO 80 I = 1,50
         DOAWNT = DOAWNT .AND. (XANT(I).GE.-0.1)
         J = ABS (XANT(I)) + 0.5
         IF (J.GT.0) THEN
            NANTSL = NANTSL + 1
            IANT(NANTSL) = J
            END IF
 80      CONTINUE
      IF (NANTSL.LE.0) THEN
         WRITE (HILINE,3011) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
C                                       Included or excluded?
      ELSE
         WRITE (HILINE,3012) TSKNAM
         IF (DOAWNT) WRITE (HILINE,3013) TSKNAM
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       1st 12 and label.
         LIMIT = MIN (12, NANTSL)
         WRITE (HILINE,3014) TSKNAM, (IANT(J), J = 1,LIMIT)
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Rest of antennas
         DO 90 I = 13,NANTSL,12
            LIMIT = I
            LIMIT2 = I + 11
            LIMIT2 = MIN (NANTSL, LIMIT2)
            WRITE (HILINE,3015) TSKNAM, (IANT(J), J = LIMIT,LIMIT2)
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
 90         CONTINUE
         END IF
C                                       Solution Table(s)
      WRITE (HILINE,3017) TSKNAM, SNVER
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       the answers
      DO 120 J = 1,NSUCAL
         IF (.NOT.ISCAL(J)) THEN
            WRITE (HILINE,3100) TSKNAM, NAMSOU(J)
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
            DO 110 I = BIF,EIF
               WRITE (HILINE,3110) TSKNAM, I, SUSAV(1,I,J),
     *            SUSAV(2,I,J), SUSAV(3,I,J)
               CALL HIADD (LUN, HILINE, BUFF1, IERR)
               IF (IERR.NE.0) GO TO 200
 110           CONTINUE
            END IF
 120     CONTINUE
C                                       Close HI file
 200  CALL HICLOS (LUN, T, BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETJHI: ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2003 FORMAT (A6,'SUBARRAY =',I4)
 3000 FORMAT (A6,'SOURCES = ''''     /All sources selected')
 3001 FORMAT (A6,'/Sources excluded:')
 3002 FORMAT (A6,'/Sources included:')
 3003 FORMAT (A6,'SOURCES = ''',A16,''',''',A16,'''')
 3004 FORMAT (A6,'         ,''',A16,''',''',A16,'''')
 4004 FORMAT (A6,'SOUCODE =''',A,''' /Source Cal code')
 3005 FORMAT (A6,'CALSOUR = ''''     /All calibrators selected')
 3006 FORMAT (A6,'/Calibrators excluded:')
 3007 FORMAT (A6,'/Calibrators included:')
 3008 FORMAT (A6,'CALSOUR = ''',A16,''',''',A16,'''')
 3009 FORMAT (A6,'         ,''',A16,''',''',A16,'''')
 3010 FORMAT (A6,'QUAL = ',I4,' CALCODE = ',A4)
 3011 FORMAT (A6,'ANTENNAS = 0     /All antennas selected')
 3012 FORMAT (A6,'/Antennas excluded:')
 3013 FORMAT (A6,'/Antennas included:')
 3014 FORMAT (A6,'ANTENNAS = ',12(I3,' '))
 3015 FORMAT (A6,'           ',12(I3,' '))
 3016 FORMAT (A6,'BIF =',I4,', EIF =',I4,' / IF range')
 3017 FORMAT (A6,'SNVER = ', I4, ' /SN version, 0=> all')
 3100 FORMAT (A6,'/ Flux fit for ',A)
 3110 FORMAT (A6,'/ IF=',I2,' FLUX=',F10.5,' +/-',F9.5,' or',F9.5,' Jy')
      END

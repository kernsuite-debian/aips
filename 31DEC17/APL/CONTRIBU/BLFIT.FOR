LOCAL INCLUDE 'BLFIT.INC'
C                                       Local include for BLFIT
      INTEGER   SEQIN, DISKIN, ANTS(50), REFANT, SUBARR, CLVER, PRTLEV,
     *   ISOLM, NITER, NUMSOU, SOUNU(300), NUMCAL, CALNU(300), CNO,
     *   BUFFER(512), BUFF2(512)
      HOLLERITH XNAMEI(3), XCLAIN(2), XNEXT, XXSOUR(4,30), XXCAL(4,30),
     *   XXSOLM
      CHARACTER NAMEIN*12, CLAIN*6, INEXT*2, XSOUR(30)*16, XCAL(31)*16,
     *   XSOLMO*4
      REAL      XSIN, XDISIN, XANTS(50), XREFA, XSUBA, XTIME(8), XGUSE,
     *   XNIT, PRTL, DPARM(10)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XXCAL,
     *   XANTS, XREFA, XSUBA, XTIME, XNEXT, XGUSE, XXSOLM, XNIT, PRTL,
     *   DPARM
      COMMON /INFO/ SEQIN, DISKIN, ANTS, REFANT, SUBARR, CLVER, PRTLEV,
     *   ISOLM, NITER, NUMSOU, SOUNU, NUMCAL, CALNU, CNO,
     *   BUFFER, BUFF2
      COMMON /CHRCOM/ NAMEIN, CLAIN, INEXT, XSOUR, XCAL, XSOLMO
LOCAL END
LOCAL INCLUDE 'BLFIT2.INC'
C                                       Data/model common
      INTEGER   XMXSOU, XMXDAT, XMXPRM
C                                       XMXSOU = Max. no. sources
      PARAMETER (XMXSOU = 300)
C                                       XMXDAT = max. no. vis records.
      PARAMETER (XMXDAT = 5000)
C                                       XMXPRM = max. no parameters.
      PARAMETER (XMXPRM = MAXANT*7+XMXSOU*2)
      INTEGER   NUMDAT, MAXDAT, NOIF, NOPOL, IANT(XMXDAT),
     *   ISOUR(XMXDAT), REFDAT(XMXDAT), IFQ(XMXDAT), ANTPNT(MAXANT),
     *   SOUPNT(XMXSOU), SOUIDN(1000), ATMPNT(MAXANT), CLKPNT(MAXANT),
     *   NANT, NSOUR, NVAR, NOBS, DOANT
      LOGICAL   GOTSOU(1000), GOTANT(MAXANT), GOTCOR(2,2,MAXANT),
     *   ALLANT, NOAFIT
      CHARACTER SRCNM(XMXSOU)*16
C                                       Max. 20 FQ entries
      DOUBLE PRECISION TIMDAT(XMXDAT), HADAT(XMXDAT), FRQIF(MAXIF,20),
     *   ANTPRM(6,MAXANT), SOUPRM(4,XMXSOU), SOUPOS(2,XMXSOU), REFTIM,
     *   GRD(XMXPRM)
C                                       2 IFs for now
      REAL      OBSDAT(2,2,XMXDAT), WTDAT(2,2,XMXDAT), ANTATM(2,MAXANT),
     *   ANTCLK(2,2,MAXANT), SUMWT, SOUSCL
      COMMON /UVDATA/ FRQIF, TIMDAT, HADAT, ANTPRM, SOUPRM, SOUPOS,
     *   REFTIM, GRD, OBSDAT, WTDAT, ANTATM, ANTCLK, SUMWT, SOUSCL,
     *   NUMDAT, MAXDAT, NOIF, NOPOL, IANT, ISOUR, REFDAT, IFQ, ANTPNT,
     *   SOUPNT, SOUIDN, ATMPNT, CLKPNT, NANT, NSOUR, NVAR, NOBS, DOANT,
     *   GOTSOU, GOTANT, GOTCOR, ALLANT, NOAFIT
      COMMON /UVCHR/ SRCNM
LOCAL END
      PROGRAM BLFIT
C-----------------------------------------------------------------------
C! Fits antenna and source coordinates from CL table
C# UV Modeling Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996, 2004
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
C-----------------------------------------------------------------------
C   BLFIT fits antenna coordinates and source coordinates from a uv data
C   set using a crude geometric model using CALIB solutions from a
C   specified CL table.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      SOURCES        XSOUR         Source list to fit
C      CALSOUR        XCAL          Calibrator source list.
C      ANTENNAS       ANTENS        Antenna list to fit.
C      REFANT         REFANT        Reference antenna
C      SUBARRAY       SUBA          Subarray.
C      TIMERANG       TIMER         Timerange
C      GAINUSE        CLUSE         CL table to use.
C      SOLMODE        XSOLMO        Data type to use.
C      PRTLEV         PRTLEV        Print level from fitting
C      DPARM          DPARM         Control parameters.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BLFIT.INC'
      INCLUDE 'BLFIT2.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'BLFIT '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL BLFIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read data
      CALL BLFDAT (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Fit positions.
      CALL BLSFIT (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE BLFIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   BLFIN gets input parameters for BLFIT
C   Inputs
C       PRGN    C*6       Program name
C   Output
C       JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      FRQIF   D(*,*)    Frequency offset per IF and FQ.
C      ISOLM   I         Data type, 1 = Residual phase.
C      NUMSOU  I         Number of sources requested (positions fitted).
C      SOUNU   I(*)      List of Source numbers
C      NUMCAL  I         Number of calibrators requested (positions
C                        fixed).
C      CALNU   I(*)      List of Calibrator numbers
C      ALLANT  L         If true all antennas are requested.
C      NOAFIT  L         If true then dont fit any antenna positions.
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in BLFIT for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   JERR
C
      CHARACTER STAT*4, UTYPE*2, STYPE(1)*4
      INTEGER   IROUND, NPARM, IERR, LUN, I, IVER, NIF, NFQ, IIF,
     *   IIFQ, NST, QUAL(30), NID
      LOGICAL   T, F, TABLE, EXIST, FITASC
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISBAND(MAXIF)
      DOUBLE PRECISION FOFF(MAXIF)
      REAL FINC(MAXIF)
      INCLUDE 'BLFIT.INC'
      INCLUDE 'BLFIT2.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA NST, STYPE /1, 'RESP'/
      DATA QUAL /30*-1/
      DATA LUN /25/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Set max. number vis.
      MAXDAT = XMXDAT
C                                       Get input parameters.
      NPARM = 322
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      REFANT = IROUND (XREFA)
      SUBARR = IROUND (XSUBA)
      CLVER = IROUND (XGUSE)
      PRTLEV = IROUND (PRTL)
      NITER = IROUND (XNIT)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (2, 1, XNEXT, INEXT)
      CALL H2CHR (4, 1, XXSOLM, XSOLMO)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
         CALL H2CHR (16, 1, XXCAL(1,I), XCAL(I))
 20      CONTINUE
C                                       Antennas
      ALLANT = .TRUE.
      DO 30 I = 1,50
         ANTS(I) = IROUND (XANTS(I))
         ALLANT = ALLANT .AND. (ANTS(I).EQ.0)
 30      CONTINUE
C                                       Defaults
      IF ((INEXT.NE.'SN') .AND. (INEXT.NE.'CL')) INEXT = 'CL'
      IF (NITER.LE.0) NITER = 50
      IF (SUBARR.LE.0) SUBARR = 1
C                                       Find input file.
      CNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1030) JERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read catalog header
      STAT = 'WRIT'
      CALL CATIO ('READ', DISKIN, CNO, CATBLK, STAT, BUFFER, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1040) JERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = CNO
      FVOL(NCFILE) = DISKIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Check XSOLMO
      IF (XSOLMO.EQ.'    ') XSOLMO = STYPE(1)
      ISOLM = -1
      DO 10 I = 1,NST
         IF (XSOLMO.EQ.STYPE(I)) ISOLM = I
 10      CONTINUE
      IF (ISOLM.LE.0) THEN
         JERR = 7
         WRITE (MSGTXT,1010) XSOLMO
         GO TO 990
         END IF
C                                       See if CL/SN table exists
      CALL ISTAB (INEXT, DISKIN, CNO, CLVER, LUN, BUFFER, TABLE, EXIST,
     *   FITASC, IERR)
      IF ((.NOT.EXIST) .OR. (IERR.NE.0)) THEN
C                                       Must be there
         WRITE (MSGTXT,1050) CLVER
         JERR = 6
         GO TO 990
         END IF
C                                       Read FQ table
      IIFQ = 1
      IVER = 1
      BUFFER(5) = 0
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNO, IVER, CATBLK, LUN, NIF,
     *   FOFF, ISBAND, FINC, IIFQ, JERR)
      IF (JERR.NE.0) GO TO 999
      DO 100 IIF = 1,NIF
         FRQIF(IIF,IIFQ) = FOFF(IIF) + FREQ
 100     CONTINUE
C                                       Number of FQ entries.
      NFQ = BUFFER(5)
C                                       More?
      IF (NFQ.GT.1) THEN
         DO 150 I = 2,NFQ
            IIFQ = I
            CALL CHNDAT ('READ', BUFFER, DISKIN, CNO, IVER, CATBLK, LUN,
     *         NIF, FOFF, ISBAND, FINC, IIFQ, JERR)
            IF (JERR.NE.0) GO TO 999
            DO 120 IIF = 1,NIF
               FRQIF(IIF,IIFQ) = FOFF(IIF) + FREQ
 120           CONTINUE
 150        CONTINUE
         END IF
C                                       Look up sources selected.
      NUMSOU = 30
      NID = XMXSOU
      CALL SOURNU (XSOUR, QUAL, NUMSOU, DISKIN, CNO, NID, BUFFER, SOUNU,
     *   JERR)
      NUMSOU = NID
C                                       See if any
      IF (SOUNU(1).LE.0) NUMSOU = 0
      IF (JERR.NE.0) GO TO 999
C                                       Calibrators
      NUMCAL = 30
      NID = XMXSOU
      CALL SOURNU (XCAL, QUAL, NUMCAL, DISKIN, CNO, NID, BUFFER, CALNU,
     *   JERR)
      NUMCAL = NID
C                                       See if any
      IF (CALNU(1).LE.0) NUMCAL = 0
      IF (JERR.NE.0) GO TO 999
C                                       If no calibrators specified then
C                                       all except sources named are
C                                       calibrators.
      IF (NUMCAL.LE.0) THEN
C                                       Any sources selected?
         IF (NUMSOU.GT.0) THEN
C                                       Deselect SOURCES
            XCAL(1) = '-               '
            DO 200 I = 1,30
               XCAL(I+1) = XSOUR(I)
 200           CONTINUE
            NUMCAL = 31
         ELSE
C                                       Deselect a nonexistant source
            XCAL(1) = '-no source'
            NUMCAL = 1
            END IF
C                                       Get sources not deselected
         NID = XMXSOU
         CALL SOURNU (XCAL, QUAL, NUMCAL, DISKIN, CNO, NID, BUFFER,
     *      CALNU, JERR)
         NUMCAL = NID
         IF (CALNU(1).LE.0) NUMCAL = 0
         IF (JERR.NE.0) GO TO 999
         END IF
C                                       Must have gotten some sources by
C                                       here.
      IF ((NUMSOU+NUMCAL).LE.0) THEN
         JERR = 5
         MSGTXT = 'YOU MUST SELECT SOME SOURCES'
         GO TO 990
         END IF
C                                       If no antennas specified but
C                                       source positions are to be
C                                       fitted then fit all ante
C                                       positions.
         NOAFIT = ALLANT .AND. (NUMSOU.GT.0)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLFIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('UNKNOWN SOLMODE : ',A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR: CL TABLE',I3,' DOES NOT EXIST')
      END
      SUBROUTINE BLFDAT (IRET)
C-----------------------------------------------------------------------
C   BLFDAT reads the CL table and converts to the form expected in the
C   rest of the program. Will read up to MAXDAT data points.
C   Input from common:
C      CATBLK  I(256) Catalog header
C      MAXDAT  I      Maximum number of visibilities.
C      ISOLM   I      Data type
C                       1 => Residual phase. (rad).
C      DPARM   R(10)  1 = min. elevation in deg.
C      INEXT   C*2    'CL' or 'SN' table to read
C      CLVER   I      CL or SN table version to read.
C      NUMSOU  I      Number of sources requested (positions fitted).
C      SOUNU   I(*)   List of Source numbers
C      NUMCAL  I      Number of calibrators requested (positions fixed).
C      CALNU   I(*)   List of Calibrator numbers
C   Output:
C      IRET    I      Return error code: 0=>OK, otherwise error.
C   Output in common:
C      NUMDAT   I     Number of data values read.
C      NOIF     I     Number of IFs
C      NOPOL    I     Number of polarizations
C      ALLANT   L     If true all antennas are requested.
C      NOAFIT   L     If true then don't fit any antenna positions.
C      TIMDAT   D(*)  Time (days) of data.
C      HADAT    D(*)  Hour angle of observation (rad)
C      WTDAT    R(2,2,*)Weights of data (IF,pol,ant)
C      IANT     I(*)  Antenna numbers of data
C      REFDAT   I(*)  Reference antenna.
C      ISOUR    I(*)  Source numbers of data
C      IFQ      I(*)  FG numbers of data.
C      OBSDAT   R(2,2,*)Observed data, depends on ISOLM (IF,pol,ant)
C                      1 => Residual phase - Ref residual phase
C      GOTSOU   L(*) If true then have data for this source.
C      GOTANT   L(*) If true then have data for this antenna.
C      GOTCOR   L(2,2,*) If true then have data for correlator
C                    (IF,pol,ant)
C      REFTIM   D    Reference time (days) = middle of obs.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LUN, LUN2, IRNO, NUMANT, NUMPOL, NUMIF, NUMREC,
     *   I, IIF, IPOL, LOOP, NKEY, KLOCS(3), KEYVAL(3), KEYTYP(3),
     *   IPOINT
      REAL      WEIGHT, CREAL, CIMAG
      LOGICAL   T, F, WANTED
      DOUBLE PRECISION TSTART, TEND
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER KEYW(3)*8, TITLE1(11)*24, TITLE2(11)*24, TITLE3(11)*24,
     *   COLHED(MAXCLC)*24
      INTEGER   KOLS(MAXCLC), SOURID, ANTNO, SUBA, FREQID, SBFSZ, BUFSZ,
     *   KEY(2,2), SNO, FQID, IERR, RTCNT, NREC, NCOL, DATP(128,2),
     *   KOL(MAXCLC), RECI(XCLRSZ), WTKOL, REKOL, IMKOL, TIMCL, INTCL,
     *   SOUCL, ANTCL, SUBCL, FRQCL, RE1CL, IM1CL, WE1CL, RE2CL, IM2CL,
     *   WE2CL
      REAL      TIMEI, FKEY(2,2), TMPDAT(2,2,MAXANT), TMPWT(2,2,MAXANT),
     *   RECORD(XCLRSZ)
      DOUBLE PRECISION TIME, HA, HATMP(MAXANT), RECD(XCLRSZ/2), REFT,
     *   DARG, ELV, SUMTIM
      INCLUDE 'BLFIT.INC'
      INCLUDE 'BLFIT2.INC'
      PARAMETER (SBFSZ = 14 * XMXDAT)
      INTEGER   SBUFF(SBFSZ)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (COLHED(1), TITLE1), (COLHED(12), TITLE2),
     *   (COLHED(23), TITLE3)
      EQUIVALENCE (SBUFF, TIMDAT)
      EQUIVALENCE (RECD, RECORD, RECI)
C                                       old format for CL table!!!!
C     EQUIVALENCE (KOLS(1), TIMCL), (KOLS(2), INTCL),
C    *   (KOLS(3),SOUCL), (KOLS(4),ANTCL), (KOLS(5),SUBCL),
C    *   (KOLS(6),FRQCL), (KOLS(7),IFRCL), (KOLS(8),GDLCL),
C    *   (KOLS(9),GPSCL), (KOLS(10),GRACL), (KOLS(11),DOPCL),
C    *   (KOLS(12),CGD1CL), (KOLS(13),DCG1CL),
C    *   (KOLS(14),CPD1CL), (KOLS(15),DCP1CL), (KOLS(16),AGD1CL),
C    *   (KOLS(17),DAG1CL), (KOLS(18),APD1CL), (KOLS(19),DAP1CL),
C    *   (KOLS(20),RE1CL), (KOLS(21),IM1CL), (KOLS(22),RA1CL),
C    *   (KOLS(23),DE1CL), (KOLS(24),TS1CL), (KOLS(25),WE1CL),
C    *   (KOLS(26),RF1CL),
C    *   (KOLS(27),CGD2CL), (KOLS(28),DCG2CL),
C    *   (KOLS(29),CPD2CL), (KOLS(30),DCP2CL), (KOLS(31),AGD2CL),
C    *   (KOLS(32),DAG2CL), (KOLS(33),APD2CL), (KOLS(34),DAP2CL),
C    *   (KOLS(35),RE2CL), (KOLS(36),IM2CL), (KOLS(37),RA2CL),
C    *   (KOLS(38),DE2CL), (KOLS(39),TS2CL), (KOLS(40),WE2CL),
C    *   (KOLS(41),RF2CL)
      EQUIVALENCE          (KOLS(1),TIMCL),  (KOLS(2),INTCL),
     *   (KOLS(3),SOUCL),  (KOLS(4),ANTCL),  (KOLS(5),SUBCL),
     *   (KOLS(6),FRQCL),  (KOLS(17),RE1CL), (KOLS(18),IM1CL),
     *   (KOLS(28),RE2CL), (KOLS(29),IM2CL), (KOLS(32),WE2CL)
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYW /'NO_ANT  ', 'NO_POL  ', 'NO_IF   '/
      DATA LUN, LUN2 /28,29/
      DATA T, F /.TRUE.,.FALSE./
C                                       old format
C     DATA COLHD1 /'TIME                    ',
C    *   'TIME INTERVAL           ',
C    *   'SOURCE ID               ', 'ANTENNA NO.             ',
C    *   'SUBARRAY                ', 'FREQ ID                 ',
C    *   'I.FAR.ROT               ',
C    *   'GEODELAY                ', 'GEOPHASE                ',
C    *   'GEORATE                 ', 'DOPPOFF                 ',
C    *   'CLKGD 1                 ', 'DCLKGD 1                ',
C    *   'CLKPD 1                 ', 'DCLKPD 1                '/
C     DATA COLHD2 /'ATMGD 1                 ',
C    *   'DATMGD 1                ',
C    *   'ATMPD 1                 ', 'DATMPD 1                ',
C    *   'REAL1                   ', 'IMAG1                   ',
C    *   'RATE 1                  ', 'DELAY 1                 ',
C    *   'TSYS 1                  ', 'WEIGHT 1                ',
C    *   'REFANT 1                ',
C    *   'CLKGD 2                 ', 'DCLKGD 2                '/
C     DATA COLHD3 /'CLKPD 2                 ',
C    *   'DCLKPD 2                ',
C    *   'ATMGD 2                 ', 'DATMGD 2                ',
C    *   'ATMPD 2                 ', 'DATMPD 2                ',
C    *   'REAL2                   ', 'IMAG2                   ',
C    *   'RATE 2                  ', 'DELAY 2                 ',
C    *   'TSYS 2                  ', 'WEIGHT 2                ',
C    *   'REFANT 2                '/
      DATA TITLE1 /'TIME                    ',
     *   'TIME INTERVAL           ',
     *   'SOURCE ID               ', 'ANTENNA NO.             ',
     *   'SUBARRAY                ', 'FREQ ID                 ',
     *   'I.FAR.ROT               ',
     *   'GEODELAY                ', 'DOPPOFF                 ',
     *   'ATMOS                   ', 'DATMOS                  '/
      DATA TITLE2 /'MBDELAY1                ',
     *   'CLOCK 1                 ', 'DCLOCK 1                ',
     *   'DISP 1                  ', 'DDISP 1                 ',
     *   'REAL1                   ', 'IMAG1                   ',
     *   'RATE 1                  ', 'DELAY 1                 ',
     *   'WEIGHT 1                ', 'REFANT 1                '/
      DATA TITLE3 /'MBDELAY2                ',
     *   'CLOCK 2                 ', 'DCLOCK 2                ',
     *   'DISP 2                  ', 'DDISP 2                 ',
     *   'REAL2                   ', 'IMAG2                   ',
     *   'RATE 2                  ', 'DELAY 2                 ',
     *   'WEIGHT 2                ', 'REFANT 2                '/
C-----------------------------------------------------------------------
      NUMDAT = 0
      NOBS = 0
      SUMWT = 0.0
      SUMTIM = 0.0D0
      RTCNT = 0
      REFT = -1.0D-6
      SNO = -10
      FQID = -10
C                                       Get antenna info
      CALL GETANT (DISKIN, CNO, SUBARR, CATBLK, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Timerange
      TSTART = XTIME(1) + (XTIME(2) / 24.0) + (XTIME(3) / 24.0*60.0) +
     *   (XTIME(4) / 24.0*3600.0)
      TEND = XTIME(5) + (XTIME(6) / 24.0) + (XTIME(7) / 24.0*60.0) +
     *   (XTIME(8) / 24.0*3600.0)
      IF ((TEND-TSTART).LE.1.0D-5) TEND = 1000.0D0
C                                       Open input table.
      NKEY = 0
      NREC = 10
      NCOL = 0
      CALL TABINI ('READ', INEXT, DISKIN, CNO, CLVER, CATBLK, LUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IRET)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, INEXT, CLVER
         GO TO 980
         END IF
C                                       Table keywords
      NKEY = 3
      CALL TABKEY ('READ', KEYW, NKEY, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Retrieve keyword values
C                                       No. antennas.
      IPOINT = KLOCS(1)
      IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. IFs per pair.
      IPOINT = KLOCS(2)
      IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF pairs.
      IPOINT = KLOCS(3)
      IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
      NOIF = NUMIF
      NOPOL = NUMPOL
C                                       Find column numbers
      NKEY = 15 + 13 + 13
      CALL FNDCOL (NKEY, COLHED, 24, .TRUE., BUFFER, KOL, IRET)
      IF ((IRET.GT.0) .AND. (IRET.LT.10)) GO TO 999
      DO 10 I = 1,NKEY
         IF (KOL(I).GT.0) THEN
            IPOINT = KOL(I)
            KOLS(I) = DATP(IPOINT,1)
         ELSE
            KOLS(I) = -1
            END IF
 10      CONTINUE
C                                       Sort to time-FQ if necessary
      IF ((BUFFER(44).NE.KOL(1)) .OR.
     *   ((BUFFER(43).NE.KOL(6).AND.(KOL(6).GT.0)))) THEN
C                                       Close table
         CALL TABIO ('CLOS', 0, IRNO, SBUFF, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 999
         KEY(1,1) = KOL(6)
         KEY(2,1) = 0
         KEY(1,2) = KOL(1)
         KEY(2,2) = 0
C                                       Sort
         BUFSZ = SBFSZ
         CALL TABSRT (DISKIN, CNO, INEXT, CLVER, CLVER, KEY, FKEY,
     *      SBUFF, BUFSZ, BUFFER, CATBLK, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Reopen table
         CALL TABINI ('READ', INEXT, DISKIN, CNO, CLVER, CATBLK, LUN,
     *      NKEY, NREC, NCOL, DATP, BUFFER, IRET)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, INEXT, CLVER
            GO TO 980
            END IF
         END IF
C                                       Initialize source and antenna
C                                       flags , temporary arrays
      DO 20 I = 1,1000
         GOTSOU(I) = .FALSE.
 20      CONTINUE
      DO 30 I = 1,MAXANT
         GOTANT(I) = .FALSE.
         HATMP(I) = DBLANK
         DO 30 IIF = 1,NOIF
            GOTCOR(IIF,1,I) = .FALSE.
            GOTCOR(IIF,2,I) = .FALSE.
            TMPDAT(IIF,1,I) = 0.0
            TMPDAT(IIF,2,I) = 0.0
            TMPWT(IIF,1,I) = 0.0
            TMPWT(IIF,2,I) = 0.0
 30         CONTINUE
C                                       Number of records
      NUMREC = BUFFER(5)
      DO 800 LOOP = 1,NUMREC
C                                       Read record
         IRNO = LOOP
         CALL TABIO ('READ', 0, IRNO, RECORD, BUFFER, IRET)
C                                       Blanked?
         IF (IRET.LT.0) GO TO 800
         IF (IRET.GT.0) GO TO 999
C                                       Get general info
         TIME = RECD(TIMCL)
         TIMEI = RECORD(INTCL)
         SOURID = RECI(SOUCL)
         ANTNO = RECI(ANTCL)
         SUBA = RECI(SUBCL)
         IF (FRQCL.GT.0) THEN
            FREQID = RECI(FRQCL)
         ELSE
            FREQID = 1
            END IF
C                                       Wanted?
         IF ((SUBA.NE.SUBARR) .OR.
     *      ((TIME.LT.TSTART) .OR. (TIME.GT.TEND))) GO TO 800
C                                       Check antenna
         WANTED = ALLANT
         DO 50 I = 1,NUMANT
            WANTED = WANTED .OR. (ANTNO.EQ.ABS (ANTS(I)))
 50         CONTINUE
         IF (.NOT.WANTED) GO TO 800
C                                       Source?
         WANTED = F
         DO 60 I = 1,NUMSOU
            WANTED = WANTED .OR. (SOURID.EQ.SOUNU(I))
 60         CONTINUE
C                                       Calibrator?
         DO 70 I = 1,NUMCAL
            WANTED = WANTED .OR. (SOURID.EQ.CALNU(I))
 70         CONTINUE
         IF (.NOT.WANTED) GO TO 800
C                                       Want this one
         GOTSOU(SOURID) = .TRUE.
         GOTANT(ANTNO) = .TRUE.
C                                       Reference time
         SUMTIM = SUMTIM + TIME
         RTCNT = RTCNT + 1
C                                       New time?
         IF (((TIME-REFT) .GT. 5.0D-5) .OR. (FQID.NE.FREQID)) THEN
            CALL DATDMP (SNO, FQID, REFT, HATMP, TMPDAT, TMPWT, IERR)
            IF (IERR.EQ.1) GO TO 810
            REFT = TIME
            FQID = FREQID
C                                       New source?
            IF (SNO.NE.SOURID) THEN
               SNO = SOURID
               REFTIM = TIME
               CALL BLFSOU (SUBARR, BUFF2, SNO, DISKIN, CNO, CATBLK,
     *            LUN2, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            END IF
C                                       Compute HA
         HA = GSTIAT + STNLON(ANTNO) * 1.002738 + TIME *
     *      ROTIAT - RAAPP
C                                       Check elevation
         DARG = SIN (STNLAT(ANTNO)) * SIN (DECAPP) + COS (STNLAT(ANTNO))
     *      * COS (DECAPP) * COS (HA)
         ELV = (1.570796327 - ACOS (DARG))
         IF ((ELV*57.296) .LT. DPARM(1)) GO TO 800
         HATMP(ANTNO) = HA
C                                       Select data type
         GO TO (100, 150), ISOLM
C                                       Residual phase
C                                       Value in table is correction
C                                       rather than value so negate.
 100        CONTINUE
C                                       Loop over Stokes
         DO 120 IPOL = 1,NUMPOL
C                                       Pointers.
            IF (IPOL.EQ.1) THEN
               WTKOL = WE1CL - 1
               REKOL = RE1CL - 1
               IMKOL = IM1CL - 1
            ELSE
               WTKOL = WE2CL - 1
               REKOL = RE2CL - 1
               IMKOL = IM2CL - 1
               END IF
C                                       Loop over IF
            DO 120 IIF = 1,NUMIF
               WEIGHT = RECORD(WTKOL+IIF)
               CREAL = RECORD(REKOL+IIF)
               CIMAG = RECORD(IMKOL+IIF)
               IF ((WEIGHT.LE.0.0) .OR.
     *            (CREAL.EQ.FBLANK) .OR.
     *            (CIMAG.EQ.FBLANK)) THEN
                  TMPWT(IIF,IPOL,ANTNO) = 0.0
                  TMPDAT(IIF,IPOL,ANTNO) = FBLANK
               ELSE
                  GOTCOR(IIF,IPOL,ANTNO) = .TRUE.
                  TMPWT(IIF,IPOL,ANTNO) = WEIGHT
                  TMPDAT(IIF,IPOL,ANTNO) = -ATAN2 (CIMAG,
     *               CREAL + 1.0E-20)
                  END IF
 120           CONTINUE
            GO TO 800
C                                       Next
 150        CONTINUE
            GO TO 800
 800     CONTINUE
C                                       Close file
 810  CALL TABIO ('CLOS', 0, IRNO, SBUFF, BUFFER, IRET)
C                                       Average time of observation.
      IF (RTCNT.GT.0) REFTIM = SUMTIM / RTCNT
C                                       Process last scan
      CALL DATDMP (SNO, FQID, REFT, HATMP, TMPDAT, TMPWT, IERR)
C                                       Tell how many vis. used.
      IF (NUMDAT.GT.1) THEN
         WRITE (MSGTXT,1230) NUMDAT
      ELSE
C                                       No data selected
         IRET = 5
         MSGTXT = 'NO DATA SELECTED'
         GO TO 980
         END IF
C                                       Error
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLFDAT: ERROR',I3,' OPENING ',A2,' TABLE, VERSION',I5)
 1230 FORMAT ('BLFDAT: Using ',I6,' values')
      END
      SUBROUTINE DATDMP (SOURID, FREQID, REFT, HATMP, TMPDAT, TMPWT,
     *   IERR)
C-----------------------------------------------------------------------
C   Routine to find the specified reference antenna or a suitable
C   reference antenna and copy the data to common refering the data to
C   the reference antenna.  All data should refer to a common time REFT.
C   Inputs:
C      SOURID  I      Source ID number
C      FREQID  I      FQ id number.
C      REFT    D      Time in days
C   Inputs/output:
C      HATMP   D(*)   Hour angle of source at each antenna
C                     If = DBLANK then not observed.
C      TMPDAT  D(2,2,*) Data array (IF,pol,ant)
C      TMPWT   D(2,2,*) Weight array (IF,pol,ant)
C   Output in common:
C      NUMDAT   I     Number of data values read.
C      NOBS     I     Number of valid measurments.
C      TIMDAT   D(*)  Time (days) of data.
C      HADAT    D(*)  Hour angle of observation (rad)
C      IANT     I(*)  Antenna numbers of data
C      REFDAT   I(*)  Reference antenna.
C      ISOUR    I(*)  Source numbers of data
C      IFQ      I(*)  FG numbers of data.
C      OBSDAT   R(2,2,*)Observed data, depends on ISOLM (IF,pol,ant)
C                      1 => Residual phase - Ref residual phase
C      WTDAT    R(2,2,*)Weights of data (IF,pol,ant)
C      SUMWT    R       Sum  of weights
C   Output:
C      IERR     I     Return code 0=OK, 1 =filled internal tables
C-----------------------------------------------------------------------
      INTEGER   SOURID, FREQID, IERR
      DOUBLE PRECISION REFT, HATMP(*)
      REAL      TMPDAT(2,2,*), TMPWT(2,2,*)
C
      INTEGER   I, IIF, IPOL, RFA, IA
      LOGICAL   ISOK
      REAL      WTSUM, MAXWT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BLFIT.INC'
      INCLUDE 'BLFIT2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IF ((SOURID.LT.0) .OR. (FREQID.LT.0)) GO TO 999
C                                       Find reference antenna
      ISOK = .FALSE.
      IF ((REFANT.GT.0) .AND. (HATMP(REFANT).NE.DBLANK)) THEN
         DO 20 IIF = 1,NOIF
            DO 10 IPOL = 1,NOPOL
               ISOK = ISOK .OR. (TMPWT(IIF,IPOL,REFANT).GT.1.0E-3)
 10            CONTINUE
 20         CONTINUE
         END IF
C                                       REFANT OK?
      IF (ISOK) THEN
         RFA = REFANT
      ELSE
C                                       Refant not there find best.
         MAXWT = -1.0E5
         DO 100 IA = 1,MAXANT
            IF (HATMP(IA).NE.DBLANK) THEN
               WTSUM = 0.0
               DO 60 IIF = 1,NOIF
                  DO 50 IPOL = 1,NOPOL
                     WTSUM = WTSUM + TMPWT(IIF,IPOL,IA)
 50                  CONTINUE
 60               CONTINUE
C                                       Use antenna with max. weight.
                  IF (WTSUM.GT.MAXWT) THEN
                     MAXWT = WTSUM
                     RFA = IA
                     END IF
                  END IF
 100        CONTINUE
         END IF
C                                       Loop over data saving in common
      DO 200 IA = 1,MAXANT
C                                       Drop reference antenna
         IF ((HATMP(IA).NE.DBLANK) .AND. (IA.NE.RFA)) THEN
            NUMDAT = NUMDAT + 1
            ISOK = .FALSE.
            IF (NUMDAT.GT.MAXDAT) THEN
C                                       Too much data - use what you
C                                       have.
               WRITE (MSGTXT,1100) XMXDAT
               CALL MSGWRT (8)
               IERR = 1
               GO TO 999
               END IF
            TIMDAT(NUMDAT) = REFT
            HADAT(NUMDAT) = 0.5D0 * (HATMP(IA) + HATMP(RFA))
            IANT(NUMDAT) = IA
            ISOUR(NUMDAT) = SOURID
            IFQ(NUMDAT) =  FREQID
            REFDAT(NUMDAT) = RFA
C                                       Data, refer to RFA
            DO 160 IPOL = 1,NOPOL
               DO 140 IIF = 1,NOIF
                  IF ((TMPWT(IIF,IPOL,IA).GT.1.0E-3) .AND.
     *               (TMPWT(IIF,IPOL,RFA).GT.1.0E-3)) THEN
                     OBSDAT(IIF,IPOL,NUMDAT) = TMPDAT(IIF,IPOL,IA) -
     *                  TMPDAT(IIF,IPOL,RFA)
                     WTDAT(IIF,IPOL,NUMDAT) = TMPWT(IIF,IPOL,IA)
                     SUMWT = SUMWT + TMPWT(IIF,IPOL,IA)
                     NOBS = NOBS + 1
                     ISOK = .TRUE.
                  ELSE
                     OBSDAT(IIF,IPOL,NUMDAT) = 0.0
                     WTDAT(IIF,IPOL,NUMDAT) = 0.0
                     END IF
 140              CONTINUE
 160           CONTINUE
C                                       Any good data?
            IF (.NOT.ISOK) NUMDAT = NUMDAT - 1
            END IF
 200     CONTINUE
C                                       Blank temporary data arrays
      DO 700 I = 1,MAXANT
         HATMP(I) = DBLANK
         DO 700 IIF = 1,NOIF
            TMPDAT(IIF,1,I) = 0.0
            TMPDAT(IIF,2,I) = 0.0
            TMPWT(IIF,1,I) = 0.0
            TMPWT(IIF,2,I) = 0.0
 700        CONTINUE
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('USING LIMIT OF ',I6,' VALUES')
      END
      SUBROUTINE BLSFIT (IRET)
C-----------------------------------------------------------------------
C   Routine to fit positions.
C   Inputs from common:
C      ISOLM     I     Data type
C                       1 => Residual phase. (rad).
C      NUMDAT    I     Number of data values read.
C      NOIF      I     Number of IFs
C      NOPOL     I     Number of polarizations
C      TIMDAT    D(*)  Time (days) of data.
C      HADAT     D(*)  Hour angle of observation (rad)
C      WTDAT     R(2,2,*)Weights of data (IF,pol,ant)
C      IANT      I(*)  Antenna numbers of data
C      REFDAT    I(*)  Reference antenna.
C      ISOUR     I(*)  Source numbers of data
C      IFQ       I(*)  FQ numbers of data.
C      OBSDAT    R(2,2,*)Observed data, depends on ISOLM (IF,pol,ant)
C                       1 => Residual phase - Ref residual phase
C      GOTSOU    L(*)  If true then have data for this source.
C      GOTANT    L(*)  If true then have data for this antenna.
C      GOTCOR    L(2,2,*) If true then have data for correlator
C                     (IF,pol,ant)
C      REFTIM    D     Reference time (days) = first found
C   Output in Common:
C      NANT      I      Number of antennas with data
C      ANTPNT    I(*)   Parameter no. of first ant. posn. value.
C                       0 = fixed.
C      ANTPRM    D(6,*) Antenna parameters (X, Y, Z, DX, DY, DZ)
C      ATMPNT    I(*)   Parameter no. of 1st ant. atm. parameter
C      ANTATM    R(2,*) Antenna atmospheric parameters.
C                       NOTE: atmospheric model not yet implemented
C      CLKPNT    I(*)   Parameter no. of 1st ant. clock parameter
C      ANTCLK    R(2,2,*) Antenna clock parameters. (IF,pol,ant)
C      NSOUR     I      Number of sources
C      SOUPNT    I(*)   Parameter no. of first source position value.
C      SOUPRM    D(4,*) Source parameters (RA, dec, DRA, Ddec of date).
C                       (RA and Dec in rad, DRA and Ddec in rad*SOUSCL)
C      SOUPOS    D(2,*) Initial source parameters (RA, dec, DRA, Ddec of
C                       date).
C      SOUIDN    I(*)   Lookup table to translate SU table numbers to
C                       parameters list numbers.
C      SRCNM     C(*)*16 Source names
C      SOUSCL    R      Scaling factor for source position offset.
C   Output:
C      IRET      I      Return code, 0=> OK otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, J
      DOUBLE PRECISION RAD2D
      EXTERNAL UVFUN1
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BLFIT.INC'
      INCLUDE 'BLFIT2.INC'
      DOUBLE PRECISION VALUE(XMXPRM), ERR(XMXPRM)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
C-----------------------------------------------------------------------
      RAD2D = 45.0D0 / ATAN (1.0D0)
C                                       Position offset scaling factor
C                                       to help LS solution.
      SOUSCL = 1.0E-5
C                                       Setup for model fitting
C                                       Get initial model
      CALL BLFMOD (VALUE, ERR, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check NVAR
      IF (NVAR.GT.NOBS) THEN
         IRET = 5
         WRITE (MSGTXT,1020) NVAR, NOBS
         GO TO 990
         END IF
C                                       Fit model
      IF (ISOLM.EQ.1) CALL SOL1 (VALUE, ERR, IRET)
      IF (IRET.GT.1) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
         END IF
C                                       Finish results
      DO 200 I = 1,NANT
C                                       Have data and fitted posn?
         IF (GOTANT(I) .AND. (ANTPNT(I).GT.0)) THEN
C                                       Correct posn.
            ANTPRM(1,I) = ANTPRM(1,I) - ANTPRM(4,I)
            ANTPRM(2,I) = ANTPRM(2,I) - ANTPRM(5,I)
            ANTPRM(3,I) = ANTPRM(3,I) - ANTPRM(6,I)
C                                       Errors, leave in ANTPRM(4-6,*)
            J = ANTPNT(I)
            ANTPRM(4,I) = ERR(J)
            ANTPRM(5,I) = ERR(J+1)
            ANTPRM(6,I) = ERR(J+2)
            END IF
 200     CONTINUE
C                                       Sources
      DO 300 I = 1,NSOUR
C                                       Have data and fitted posn?
         IF (SOUPNT(I).GT.0) THEN
C                                       Correct posn.
            SOUPRM(1,I) = (SOUPRM(1,I) - SOUPRM(3,I)*SOUSCL) * RAD2D
            SOUPRM(2,I) = (SOUPRM(2,I) - SOUPRM(4,I)*SOUSCL) * RAD2D
            SOUPOS(1,I) = SOUPOS(1,I) * RAD2D
            SOUPOS(2,I) = SOUPOS(2,I) * RAD2D
C                                       Errors, leave in SOUPRM(3&4,*)
            J = SOUPNT(I)
            SOUPRM(3,I) = ERR(J) * RAD2D * 3600.0D0 * SOUSCL
            SOUPRM(4,I) = ERR(J+1) * RAD2D * 3600.0D0 * SOUSCL
            END IF
 300     CONTINUE
C                                       Write results in AN and/or SU
C                                       tables.
      CALL BLFOUT (IRET)
      IF (IRET.NE.0) GO TO 999
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('MORE UNKNOWNS (',I5,') THAN DATA (',I5,')')
 1100 FORMAT ('MODFIT: ERROR',I3,' FITTING MODEL')
      END
      SUBROUTINE BLFMOD (VALUE, ERR, IRET)
C-----------------------------------------------------------------------
C   Sets initial model values from reading the AN and SU tables.
C   Set up to fit for errors in antenna and source coordinates.
C   If no antenna positions are being fitted then antenna clocks will
C   not be fitted.  In this case the a priori values in FXMOD? should be
C   determined only from calibrator sources.
C   Outputs
C      VALUE   D(*)   The initial value of the parameters
C      ERR     D(*)   Estimate of error (use all 1.0)
C      IRET    I      Return code, 0=>OK, else failed.
C   Input in Common:
C      GOTSOU  L(*)   If true then have data for this source.
C      GOTANT  L(*)   If true then have data for this antenna.
C      GOTCOR  L(2,2,*) If true then have data for correlator
C                     (IF,pol,ant)
C      ALLANT  L      If true all antennas are requested.
C      NOAFIT  L      If true then don't fit any antenna positions.
C      DPARM   R(10)  If (2) .gt. 0 then the antenna locations use a
C                     left handed system.
C   Output in Common:
C      NVAR    I      The number of parameters being fitted.
C      NANT    I      Number of antennas with data
C      ANTPNT  I(*)   Parameter no. of first ant. posn. value.
C                     0 = fixed.
C      ANTPRM  D(6,*) Antenna parameters (X, Y, Z, DX, DY, DZ)
C      ATMPNT  I(*)   Parameter no. of 1st ant. atm. parameter
C      ANTATM  R(2,*) Antenna atmospheric parameters.
C                     NOTE: atmospheric model not yet implemented
C      CLKPNT  I(*)   Parameter no. of 1st ant. clock parameter
C      ANTCLK  R(2,2,*) Antenna clock parameters. (IF,pol,ant)
C      NSOUR   I      Number of sources
C      SOUPNT  I(*)   Parameter no. of first source position value.
C      SOUPRM  D(4,*) Source parameters (RA, dec, DRA, Ddec of date).
C                     (RA and Dec in rad, DRA and Ddec in rad*SOUSCL)
C      SOUPOS  D(2,*) Initial source parameters (RA, dec, DRA, Ddec of
C                     date).
C      SOUIDN  I(*)   Lookup table to translate SU table numbers to
C                     parameters list numbers.
C   Output:
C     IRET           I    Return code, 0=> OK otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
      DOUBLE PRECISION VALUE(*), ERR(*)
C
      INTEGER   MXPRM, LOOP, I, J, LUN, IS
      LOGICAL   FITTED
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BLFIT.INC'
      INCLUDE 'BLFIT2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA MXPRM /XMXPRM/
      DATA LUN /28/
C-----------------------------------------------------------------------
      NVAR = 0
C                                       Antenna info
      CALL GETANT (DISKIN, CNO, SUBARR, CATBLK, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
      NANT = NSTNS
      DO 50 LOOP = 1,NANT
         ANTPRM(1,LOOP) = STNX(LOOP)
         ANTPRM(2,LOOP) = STNY(LOOP)
C                                       Left handed?
         IF (DPARM(2).GT.0.0001) ANTPRM(2,LOOP) = -ANTPRM(2,LOOP)
         ANTPRM(3,LOOP) = STNZ(LOOP)
         ANTPRM(4,LOOP) = 0.0D0
         ANTPRM(5,LOOP) = 0.0D0
         ANTPRM(6,LOOP) = 0.0D0
C                                       Antenna pos. fitted?
         FITTED = ALLANT .AND. (LOOP.NE.REFANT)
         FITTED = FITTED .AND. (.NOT.NOAFIT)
         DO 40 I = 1,NANT
            FITTED = FITTED .OR. (LOOP.EQ.ANTS(I))
 40         CONTINUE
         IF (FITTED.AND.GOTANT(LOOP)) THEN
            ANTPNT(LOOP) = NVAR + 1
            VALUE(NVAR+1) = ANTPRM(4,LOOP)
            ERR(NVAR+1) = 1.0D0
            VALUE(NVAR+2) = ANTPRM(5,LOOP)
            ERR(NVAR+2) = 1.0D0
            VALUE(NVAR+3) = ANTPRM(6,LOOP)
            ERR(NVAR+3) = 1.0D0
            NVAR = NVAR + 3
         ELSE
            ANTPNT(LOOP) = 0
            END IF
 50      CONTINUE
C                                       Source info (fitted)
      NSOUR = 0
      DO 100 LOOP = 1,NUMSOU
         IF (GOTSOU(SOUNU(LOOP))) THEN
            CALL BLFSOU (SUBARR, BUFFER, SOUNU(LOOP), DISKIN, CNO,
     *         CATBLK, LUN, IRET)
            IF (IRET.NE.0) GO TO 999
            NSOUR = NSOUR + 1
            SOUPRM(1,NSOUR) = RAAPP
            SOUPRM(2,NSOUR) = DECAPP
            SOUPOS(1,NSOUR) = RAAPP
            SOUPOS(2,NSOUR) = DECAPP
            SOUPRM(3,NSOUR) = 0.0D0
            SOUPRM(4,NSOUR) = 0.0D0
            SOUPNT(NSOUR) = NVAR + 1
            VALUE(NVAR+1) = SOUPRM(3,NSOUR)
            ERR(NVAR+1) = 1.0D0
            VALUE(NVAR+2) = SOUPRM(4,NSOUR)
            ERR(NVAR+2) = 1.0D0
            NVAR = NVAR + 2
            IS = SOUNU(LOOP)
            SOUIDN(IS) = NSOUR
            SRCNM(NSOUR) = SNAME
         ELSE
            SOUPNT(LOOP) = 0
            END IF
 100      CONTINUE
C                                       Source info (calibrator)
      DO 200 LOOP = 1,NUMCAL
         IF (GOTSOU(CALNU(LOOP))) THEN
            CALL BLFSOU (SUBARR, BUFFER, CALNU(LOOP), DISKIN, CNO,
     *         CATBLK, LUN, IRET)
            IF (IRET.NE.0) GO TO 999
            NSOUR = NSOUR + 1
            SOUPRM(1,NSOUR) = RAAPP
            SOUPRM(2,NSOUR) = DECAPP
            SOUPOS(1,NSOUR) = RAAPP
            SOUPOS(2,NSOUR) = DECAPP
            SOUPRM(3,NSOUR) = 0.0D0
            SOUPRM(4,NSOUR) = 0.0D0
            SOUPNT(NSOUR) = 0
            IS = CALNU(LOOP)
            SOUIDN(IS) = NSOUR
            SRCNM(NSOUR) = SNAME
            END IF
 200      CONTINUE
C                                       Instrumental phases (clock)
      DO 300 LOOP = 1,NANT
         IF (GOTANT(LOOP)) THEN
            CLKPNT(LOOP) = 0
            IF ((LOOP.NE.REFANT) .AND. (.NOT.NOAFIT))
     *         CLKPNT(LOOP) = NVAR + 1
C                                       Loop over IF
            DO 260 J = 1,NOPOL
C                                       Loop over poln.
               DO 240 I = 1,NOIF
C                                       Initial guess = 0.0
                  ANTCLK(I,J,LOOP) = 0.0
C                                       Fix reference antenna or all
                  IF ((LOOP.NE.REFANT) .AND. (.NOT.NOAFIT)) THEN
                     VALUE(NVAR+1) = ANTCLK(I,J,LOOP)
                     ERR(NVAR+1) = 1.0D0
                     NVAR = NVAR + 1
                     END IF
 240              CONTINUE
 260           CONTINUE
            END IF
 300      CONTINUE
C                                       Check number of parameters
      IF (NVAR.GT.MXPRM) THEN
         WRITE (MSGTXT,1300) NVAR, MXPRM
         IRET = 9
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1300 FORMAT ('TOO MANY PARAMETERS TO FIT,',I3,' > ',I4)
      END
      SUBROUTINE BLFSOU (SUBARR, BUFFER, SID, DISK, CNO, CATBLK,
     *   LUN, IRET)
C-----------------------------------------------------------------------
C   Routine to fetch source information and precess between the mean
C   epoch and the position of date.  Values returned in common in
C   INCS:DSOU.INC.
C   Inputs:
C      SUBARR I      Subarray number
C      BUFFER I(*)   I/O Buffer for AN table.
C      SID    I      Source ID number.
C      DISK   I      Disk number
C      CNO    I      Catalog slot number
C      CATBLK I(256) Catalog header
C      LUN    I      LUN to use
C   Input in common:
C      RAEPO    D       Right ascension at mean EPOCH (radians)
C      DECEPO   D       Declination at mean EPOCH (radians)
C      REFTIM   D       Reference time.
C   Output in common:
C      SOUNAM   C*16    Source name
C      EPOCH    D       Mean Epoch for position in yr. since year 0.0
C      RAAPP    D       Apparent Right ascension (radians)
C      DECAPP   D       Apparent Declination(radians)
C   Outputs:
C      IRET   I      Return code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   SUBARR, BUFFER(*), SID, DISK, CNO, CATBLK(256), LUN,
     *   IRET
C
      INTEGER   DIR, ANVER
      LOGICAL   LDIR, GR, DA, NR
      DOUBLE PRECISION DELDAT, RHOGEO, PHIGEO, JD, OBSPOS(3)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BLFIT2.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA DIR /1/
      DATA DELDAT /0.1D0/
      DATA LDIR, GR, DA, NR /.TRUE.,.TRUE.,.FALSE.,.TRUE./
      DATA RHOGEO, PHIGEO, OBSPOS /5*0.0D0/
C-----------------------------------------------------------------------
C                                       Get Subarray and timing info
C                                       from the AN table
      ANVER = SUBARR
      CALL ANTINI ('READ', BUFFER, DISK, CNO, ANVER, CATBLK, LUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *   RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS,ANAME, NUMORB, NOPCAL,
     *   ANFQID, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Close table
      CALL TABIO ('CLOS', 0, IANRNO, BUFFER, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Julian date
      CALL JULDAY (RDATE, JD)
      JD = JD + REFTIM
C                                       Convert polar position to asec.
      POLRXY(1) = POLRXY(1) * 206265.0 / 6378140.0
      POLRXY(2) = POLRXY(2) * 206265.0 / 6378140.0
C                                       Get source info.
      CALL GETSOU (SID, DISK, CNO, CATBLK, LUN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Precess position to the apparent
C                                       coordinates.
C                                       JPRECS works for both J2000 and
C                                       B1950 starting December 2004
      CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLRXY, RAEPO,
     *      DECEPO, RAAPP, DECAPP)
C
 999  RETURN
      END
      SUBROUTINE FXMOD1 (VALUE)
C-----------------------------------------------------------------------
C   Model fixup routine for residual phase data.
C   Sets initial clock parameters to the average of all data values.
C   If no antennas are being fitted then only calibrators sources are
C   used.
C   Input in Common:
C      NUMDAT    I     Number of data values read.
C      NOIF      I     Number of IFs
C      NOPOL     I     Number of polarizations
C      IANT      I(*)  Antenna numbers of data
C      ISOUR     I(*)  Source numbers of data
C      CLKPNT    I(*)  Parameter no. of 1st ant. clock parameter
C      OBSDAT    R(2,2,*)Observed data, depends on ISOLM (IF,pol,ant)
C                       1 => Residual phase - Ref residual phase
C      WTDAT     R(2,2,*)Weights of data (IF,pol,ant)
C      NOAFIT    L     If true then don't fit any antenna positions.
C      NUMSOU    I     Number of sources requested (positions fitted).
C      SOUNU     I(*)  List of Source numbers
C   Outputs:
C      VALUE   D(*)   The initial value of the parameters
C-----------------------------------------------------------------------
      DOUBLE PRECISION VALUE(*)
C
      INTEGER   LOOP, IIF, IPOL, IPNT, IA, IS, I
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BLFIT.INC'
      INCLUDE 'BLFIT2.INC'
      REAL      SUMC(2,2,MAXANT), SUMS(2,2,MAXANT), SUMSWT(2,2,MAXANT),
     *   AVG, WT
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Clear sums
      DO 30 LOOP = 1,MAXANT
         DO 20 IPOL = 1,NOPOL
            DO 10 IIF = 1,NOIF
               SUMC(IIF,IPOL,LOOP) = 0.0
               SUMS(IIF,IPOL,LOOP) = 0.0
               SUMSWT(IIF,IPOL,LOOP) = 0.0
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
C                                       Vector sums
      DO 100 LOOP = 1,NUMDAT
C                                       Want this one?
         IF (NOAFIT) THEN
            IS = ISOUR(LOOP)
            DO 50 I = 1,NUMSOU
               IF (IS.EQ.SOUNU(I)) GO TO 100
 50            CONTINUE
            END IF
         IA = IANT(LOOP)
         DO 90 IPOL = 1,NOPOL
            DO 80 IIF = 1,NOIF
               IF (WTDAT(IIF,IPOL,LOOP).GT.1.0E-5) THEN
                  WT = WTDAT(IIF,IPOL,LOOP)
                  SUMC(IIF,IPOL,IA) = SUMC(IIF,IPOL,IA) +
     *               COS (OBSDAT(IIF,IPOL,LOOP)) * WT
                  SUMS(IIF,IPOL,IA) = SUMS(IIF,IPOL,IA) +
     *               SIN (OBSDAT(IIF,IPOL,LOOP)) * WT
                  SUMSWT(IIF,IPOL,IA) = SUMSWT(IIF,IPOL,IA) + WT
C                                       If NOAFIT then we are thru with
C                                       this data; zero weight
                  IF (NOAFIT) WTDAT(IIF,IPOL,LOOP) = 0.0
                  END IF
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
C                                       Set initial guess
      DO 300 LOOP = 1,MAXANT
         IPNT = CLKPNT(LOOP)
C                                       Loop over IF
         DO 260 IPOL = 1,NOPOL
C                                       Loop over poln.
            DO 240 IIF = 1,NOIF
               ANTCLK(IIF,IPOL,LOOP) = 0.0
C                                       Use average as guess
               IF (SUMSWT(IIF,IPOL,LOOP).GT.1.0E-5) THEN
                  SUMS(IIF,IPOL,LOOP) = SUMS(IIF,IPOL,LOOP) /
     *               SUMSWT(IIF,IPOL,LOOP)
                  SUMC(IIF,IPOL,LOOP) = SUMC(IIF,IPOL,LOOP) /
     *               SUMSWT(IIF,IPOL,LOOP)
                  AVG = ATAN2 (SUMS(IIF,IPOL,LOOP),
     *               SUMC(IIF,IPOL,LOOP)+1.0E-20)
                  ANTCLK(IIF,IPOL,LOOP) = AVG
                  IF (IPNT.GT.0) THEN
                     VALUE(IPNT) = AVG
                     IPNT = IPNT + 1
                     END IF
                  END IF
 240           CONTINUE
 260        CONTINUE
 300     CONTINUE
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE BLFOUT (IRET)
C-----------------------------------------------------------------------
C   Write final solutions to AN and/or SU tables as appropriate
C   Also write residuals into a new SN table
C   Input from common:
C      NANT      I      Number of antennas with data
C      ANTPNT    I(*)   Parameter no. of first ant. posn. value.
C                       0 = fixed.
C      ANTPRM    D(6,*) Antenna parameters (X, Y, Z,err X,err Y,err Z) m
C      ATMPNT    I(*)   Parameter no. of 1st ant. atm. parameter
C      ANTATM    R(2,*) Antenna atmospheric parameters.
C                       NOTE: atmospheric model not yet implemented
C      CLKPNT    I(*)   Parameter no. of 1st ant. clock parameter
C      ANTCLK    R(2,2,*) Antenna clock parameters. (IF,pol,ant)
C      NSOUR     I      Number of sources
C      SOUPNT    I(*)   Parameter no. of first source position value.
C      SOUPRM    D(4,*) Source parameters (RA, dec, E RA, E dec of
C                       date). (Deg.)
C      SOUPOS    D(2,*) Initial source parameters (RA, dec, DRA, Ddec of
C                       date).
C      SOUIDN    I(*)   Lookup table to translate SU table numbers to
C                       parameters list numbers.
C      SRCNM     C(*)*16 Source names
C      REFTIM    D      Reference time (days) = middle of obs.
C      NUMDAT    I     Number of data values read.
C      NOIF      I     Number of IFs
C      NOPOL     I     Number of polarizations
C      TIMDAT    D(*)  Time (days) of data.
C      IANT      I(*)  Antenna numbers of data
C      REFDAT    I(*)  Reference antenna.
C      ISOUR     I(*)  Source numbers of data
C      IFQ       I(*)  FG numbers of data.
C      OBSDAT    R(2,2,*)Residuals, depends on ISOLM (IF,pol,ant)
C                       1 => Residual phase - Ref residual phase
C      WTDAT     R(2,2,*)Weights of data (IF,pol,ant)
C      DPARM     R(10)  If (2) .gt. 0 then the antenna locations use a
C                       left handed system.
C   Output:
C      IRET      I      Return code, 0=>OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LUN, LUN2, LOOP, INVER, OUTVER, NSUREC, ISURNO, NUMIF,
     *   IDSOU, QUAL, IS, DIR, J, RAHM(2), DECHM(2), ISNRNO,
     *   NUMNOD, SOURID, ANTNO, SUBA, FREQID, NODENO, IPOL, IIF
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4, CHRA*1, CHDEC*1
      REAL      RASEC, DECSEC, GMMOD, RANOD(1), DECNOD(1), TIMEI, IFR
      LOGICAL   ISANT, ISSOU, LDIR, GR, DA, NR, ISAPPL
      DOUBLE PRECISION BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, PMRA,
     *   PMDEC, DELDAT, JD, RHOGEO, PHIGEO, D1, D2, D3, D2RAD,
     *   RAD2D, TIME, OBSPOS(3)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC), SNKOLS(MAXSNC),
     *   SNNUMV(MAXSNC), REFA(2,MAXIF), MAXIF2, SUFQID
      REAL      FLUX(4,MAXIF), CREAL(2,MAXIF), CIMAG(2,MAXIF),
     *   DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF), MBDELY(2)
      DOUBLE PRECISION FREQO(MAXIF), LSRVEL(MAXIF), LRESTF(MAXIF)
      PARAMETER (MAXIF2=MAXIF*2)
      INCLUDE 'BLFIT.INC'
      INCLUDE 'BLFIT2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA LUN, LUN2 /28,29/
      DATA DIR /-1/
      DATA DELDAT /0.1D0/
      DATA LDIR, GR, DA, NR /.FALSE.,.TRUE.,.FALSE.,.TRUE./
      DATA RHOGEO, PHIGEO, OBSPOS /5*0.0D0/
      DATA NUMNOD, RANOD, DECNOD /1, 0.0, 0.0/
      DATA GMMOD /1.0/
      DATA ISAPPL /.FALSE./
      DATA CREAL,     CIMAG,       DELAY,      RATE,     WEIGHT
     *  /MAXIF2*0.0, MAXIF2*0.0, MAXIF2*0.0, MAXIF2*0.0, MAXIF2*0.0/
      DATA REFA /MAXIF2*0/
      DATA MBDELY /0.0, 0.0/
C-----------------------------------------------------------------------
      RAD2D = 45.0D0 / ATAN (1.0D0)
      D2RAD = ATAN (1.0D0) / 45.0D0
C                                       Any antennas positions fitted?
      ISANT = .FALSE.
      DO 50 LOOP = 1,NANT
         ISANT = ISANT .OR. (ANTPNT(LOOP).GT.0)
 50      CONTINUE
C                                       Write  AN table if necessary.
      IF (ISANT) THEN
C                                       Copy input table to output.
         INVER = SUBARR
         OUTVER = 0
         CALL TABCOP ('AN', INVER, OUTVER, LUN, LUN2, DISKIN, DISKIN,
     *      CNO, CNO, CATBLK, BUFFER, BUFFER(257), IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Tell about new AN table.
         CALL FNDEXT ('AN', CATBLK, OUTVER)
         WRITE (MSGTXT,1050) OUTVER
         CALL MSGWRT (5)
C                                       Open AN table 1st for 'READ' to
C                                       get keywords.
         CALL ANTINI ('READ', BUFFER, DISKIN, CNO, OUTVER, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, NUMORB,
     *      NOPCAL, ANFQID, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Close table
         CALL TABIO ('CLOS', 0, IANRNO, BUFFER, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Then 'WRIT'
         CALL ANTINI ('WRIT', BUFFER, DISKIN, CNO, OUTVER, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, NUMORB,
     *      NOPCAL, ANFQID, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Loop over antennas rewriting
         DO 100 LOOP = 1,NANT
            IANRNO = LOOP
            CALL TABAN ('READ', BUFFER, IANRNO, ANKOLS, ANNUMV,
     *         ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.NE.0) GO TO 999
            IF (ANTPNT(NOSTA).GT.0) THEN
C                                       Left handed?
               IF (DPARM(2).GT.0.001) THEN
                  ANTPRM(2,NOSTA) = -ANTPRM(2,NOSTA)
                  ANTPRM(5,NOSTA) = -ANTPRM(5,NOSTA)
                  END IF
C                                       Reset antenna coordinates
               D1 = ANTPRM(1,NOSTA) - STAXYZ(1)
               D2 = ANTPRM(2,NOSTA) - STAXYZ(2)
               D3 = ANTPRM(3,NOSTA) - STAXYZ(3)
               STAXYZ(1) = ANTPRM(1,NOSTA)
               STAXYZ(2) = ANTPRM(2,NOSTA)
               STAXYZ(3) = ANTPRM(3,NOSTA)
C                                       Tell about new values
               WRITE (MSGTXT,1100) NOSTA, (ANTPRM(J,NOSTA), J = 1,3)
               CALL MSGWRT (8)
               WRITE (MSGTXT,1101) D1, D2, D3
               CALL MSGWRT (8)
               WRITE (MSGTXT,1102) (ANTPRM(J,NOSTA), J = 4,6)
               CALL MSGWRT (8)
               IANRNO = LOOP
               CALL TABAN ('WRIT', BUFFER, IANRNO, ANKOLS, ANNUMV,
     *            ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF,
     *            POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
 100        CONTINUE
C                                       Close table
         CALL TABIO ('CLOS', 0, IANRNO, BUFFER, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Any source positions fitted?
      ISSOU = .FALSE.
      DO 150 LOOP = 1,NSOUR
         ISSOU = ISSOU .OR. (SOUPNT(LOOP).GT.0)
 150     CONTINUE
C                                       Write  AN table if necessary.
      IF (ISSOU) THEN
C                                       Set JD of mean of observations.
         CALL JULDAY (RDATE, JD)
         JD = JD + REFTIM
C                                       Copy input table to output.
         INVER = 1
         OUTVER = 0
         CALL TABCOP ('SU', INVER, OUTVER, LUN, LUN2, DISKIN, DISKIN,
     *      CNO, CNO, CATBLK, BUFFER, BUFFER(257), IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Tell about new SU table.
         CALL FNDEXT ('SU', CATBLK, OUTVER)
         WRITE (MSGTXT,1150) OUTVER
         CALL MSGWRT (5)
C                                       Open SU table
         CALL SOUINI ('WRIT', BUFFER, DISKIN, CNO, OUTVER, CATBLK, LUN,
     *      NUMIF, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         NSUREC = BUFFER(5)
C                                       Convert polar posn to asec.
         POLRXY(1) = POLRXY(1) * 206265.0 / 6378140.0
         POLRXY(2) = POLRXY(2) * 206265.0 / 6378140.0
C                                       Loop over sources rewriting
         DO 200 LOOP = 1,NSUREC
            ISURNO = LOOP
            CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *         IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *         DECEPO, EPOCH, RAAPP, DECAPP, LSRVEL, LRESTF, PMRA,
     *         PMDEC, IRET)
            IF (IRET.NE.0) GO TO 999
            IS = SOUIDN(IDSOU)
            IF (SOUPNT(IS).GT.0) THEN
               D1 = (SOUPRM(1,IS) - SOUPOS(1,IS)) * 3600.0
               D2 = (SOUPRM(2,IS) - SOUPOS(2,IS)) * 3600.0
               RAAPP = SOUPRM(1,IS) * D2RAD
               DECAPP = SOUPRM(2,IS) * D2RAD

C                                       Precess position to the apparent
C                                       coordinates.
C                                       JPRECS works for both J2000 and
C                                       B1950 starting December 2004
               CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLRXY,
     *            RAEPO,DECEPO, RAAPP, DECAPP)
C                                       Give new positions
               RAEPO = RAEPO * RAD2D
               DECEPO = DECEPO * RAD2D
               RAAPP = SOUPRM(1,IS)
               DECAPP = SOUPRM(2,IS)
               CALL COORDD (1, RAEPO, CHRA, RAHM, RASEC)
               CALL COORDD (2, DECEPO, CHDEC, DECHM, DECSEC)
               WRITE (MSGTXT,1200) SRCNM(IS), RAHM, RASEC, CHDEC, DECHM,
     *            DECSEC
               CALL MSGWRT (8)
               WRITE (MSGTXT,1201) D1, D2
               CALL MSGWRT (8)
               WRITE (MSGTXT,1202) (SOUPRM(J,IS), J=3,4)
               CALL MSGWRT (8)
C                                       Reset source coordinates
               ISURNO = LOOP
               CALL TABSOU ('WRIT', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *            IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW,
     *            RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, LSRVEL, LRESTF,
     *            PMRA, PMDEC, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
 200        CONTINUE
C                                       Close table
         CALL TABIO ('CLOS', 0, ISURNO, BUFFER, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Write residuals in SN table
      OUTVER = 0
C                                       Open SU table
      CALL SNINI ('WRIT', BUFFER, DISKIN, CNO, OUTVER, CATBLK, LUN,
     *   ISNRNO, SNKOLS, SNNUMV, NANT, NOPOL, NOIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Tell about new SN table.
      WRITE (MSGTXT,1250) OUTVER
      CALL MSGWRT (5)
C                                       Loop over data.
      DO 300 LOOP = 1,NUMDAT
         IFR = 0.0
         TIME = TIMDAT(LOOP)
         TIMEI = 0.0
         SOURID = ISOUR(LOOP)
         ANTNO = IANT(LOOP)
         SUBA = SUBARR
         FREQID = IFQ(LOOP)
         NODENO = 1
C                                       Loop over polarization
         DO 250 IPOL = 1,NOPOL
C                                       loop over IF
            DO 240 IIF = 1,NOIF
               REFA(IPOL,IIF) = REFDAT(LOOP)
               WEIGHT(IPOL,IIF) = WTDAT(IIF,IPOL,LOOP)
C                                       Use correct data type
               IF (ISOLM.EQ.1) THEN
                  CREAL(IPOL,IIF) = COS (OBSDAT(IIF,IPOL,LOOP))
                  CIMAG(IPOL,IIF) = SIN (OBSDAT(IIF,IPOL,LOOP))
                  END IF
 240           CONTINUE
 250        CONTINUE
         CALL TABSN ('WRIT', BUFFER, ISNRNO, SNKOLS, SNNUMV,
     *      NOPOL, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      NODENO, MBDELY, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
 300     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
      GO TO 999
C                                       Error
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('Fitted antenna coordinates written in AN table ',I3)
 1150 FORMAT ('Fitted source coordinates written in SU table ',I3)
 1100 FORMAT ('ant(',I2,') X =',1PD20.12,' Y=',D20.12,' Z=',D20.12)
 1101 FORMAT ('       DX =',1PD19.11,' DY=',D19.11,' DZ=',D19.11)
 1102 FORMAT ('   errors    ',1PD20.12,3X,D20.12,2X,D20.12)
 1200 FORMAT (A16,' RA = ',2I3,F8.4,' Dec = ',A1,2I3,F7.3)
 1201 FORMAT ('   change (asec)',6X,F15.7,5X,F15.7)
 1202 FORMAT ('   errors    ',14X,F10.7,10X,F10.7)
 1250 FORMAT ('Residual values written in SN table ',I3)
      END
      SUBROUTINE SOL1 (VALUE, ERR, IRET)
C-----------------------------------------------------------------------
C   Routine to fit positions from residual phase data.  If only antenna
C   positions are being fitted then this is done one antenna at a time
C   to increase the speed of the fitting.
C   Input/output:
C      VALUE     D(*)  Model parameters
C      ERR       D(*)  Errors in parameters
C   Inputs from common:
C      NOIF      I     Number of IFs
C      NOPOL     I     Number of polarizations
C      NANT      I     Number of antennas with data
C      GOTANT    L(*)  If true then have data for this antenna.
C      GOTCOR    L(2,2,*) If true then have data for correlator
C                      (IF,pol,ant)
C      NSOUR     I     Number of sources
C      SOUPNT    I(*)  Parameter no. of first source position value.
C      REFTIM    D     Reference time (days) = first found
C   Input/Output in Common:
C      ANTPNT    I(*)   Parameter no. of first ant. posn. value.
C                       0 = fixed.
C      ANTPRM    D(6,*) Antenna parameters (X, Y, Z, DX, DY, DZ)
C      ATMPNT    I(*)   Parameter no. of 1st ant. atm. parameter
C      ANTATM    R(2,*) Antenna atmospheric parameters.
C                       NOTE: atmospheric model not yet implemented
C      CLKPNT    I(*)   Parameter no. of 1st ant. clock parameter
C      ANTCLK    R(2,2,*) Antenna clock parameters. (IF,pol,ant)
C      SOUPRM    D(4,*) Source parameters (RA, dec, DRA, Ddec of date).
C                       (RA and Dec in rad, DRA and Ddec in rad*SOUSCL)
C      SOUPOS    D(2,*) Initial source parameters (RA, dec, DRA, Ddec of
C                       date).
C      SOUIDN    I(*)   Lookup table to translate SU table numbers to
C                       parameters list numbers.
C      DOANT     I      Antenna number to process, 0 => all.
C   Output:
C      IRET      I      Return code, 0=> OK otherwise failed.
C-----------------------------------------------------------------------
      DOUBLE PRECISION VALUE(*), ERR(*)
      INTEGER   IRET
C
      INTEGER   I, J, K, IPNT, JPNT, MVAR, TAPNT, TCPNT
      LOGICAL   FITSOU, WANTAN
      REAL      EDROP
      DOUBLE PRECISION TVAL(20), TERR(20), EPS, FOPT, GNOPT, CHISQ, FAC,
     *   RMS
      EXTERNAL UVFUN1
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BLFIT.INC'
      INCLUDE 'BLFIT2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Set tolerance
      EDROP = 1.0
      EPS = 1.0D-12 * EDROP
C                                       Any mods to the initial model.
      CALL FXMOD1 (VALUE)
C                                       Tell data type
      MSGTXT = 'Fitting to residual phases'
      CALL MSGWRT (6)
C                                       Antenna flag for UVFUN1
      DOANT = 0
C                                       Get Chi squares
      CALL UVFUN1 (VALUE, CHISQ, GRD, -1)
      RMS = SQRT (CHISQ / SUMWT) * 57.296
      WRITE (MSGTXT,1030) RMS
      CALL MSGWRT (6)
C                                       See if only fitting antenna
C                                       posns.
      FITSOU = .FALSE.
      DO 50 I = 1,NSOUR
         FITSOU = FITSOU .OR. (SOUPNT(I).GT.0)
 50      CONTINUE
      IF (FITSOU) THEN
C                                       Do global solutions
         CALL DVDMIN (UVFUN1, VALUE, ERR, NVAR, EPS, NITER, FOPT, GNOPT,
     *      PRTLEV, IRET)
C                                       Get errors
         FAC = NOBS
         FAC = SQRT (FAC / (NOBS - NVAR))
         IF (FOPT.LT.1.0E-20) FOPT = 1.0
         RMS = SQRT (FOPT/NUMDAT) * FAC
         DO 100 I = 1,NVAR
            ERR(I) = ERR(I) * RMS
 100        CONTINUE
      ELSE
C                                       Fit antennas separately
C                                       Loop over antennas
         DO 500 I = 1,NANT
C                                       Fit this antenna?
            WANTAN = GOTANT(I) .AND. (ANTPNT(I).GT.0)
            IF (WANTAN) THEN
               DOANT = I
               MVAR = 3 + (NOIF * NOPOL)
C                                       Shuffle pointers etc to use
C                                       DVDMIN for one antenna at a
C                                       time.
C                                       Prior antenna values:
               IPNT = ANTPNT(I)
               TAPNT = IPNT
               ANTPNT(I) = 1
               TVAL(1) = VALUE(IPNT)
               TVAL(2) = VALUE(IPNT+1)
               TVAL(3) = VALUE(IPNT+2)
               TERR(1) = ERR(IPNT)
               TERR(2) = ERR(IPNT+1)
               TERR(3) = ERR(IPNT+2)
C                                       Prior clock values:
               IPNT = CLKPNT(I)
               TCPNT = IPNT
               CLKPNT(I) = 4
               JPNT = 4
               DO 120 J = 1,NOPOL
                  DO 110 K = 1,NOIF
                     TVAL(JPNT) = VALUE(IPNT)
                     TERR(JPNT) = ERR(IPNT)
                     JPNT = JPNT + 1
                     IPNT = IPNT + 1
 110                 CONTINUE
 120              CONTINUE
C                                       Antenna message
               IF (PRTLEV.GT.0) THEN
                  WRITE (MSGTXT,1120) I
                  CALL MSGWRT (3)
                  END IF
C                                       Fit position:
               CALL DVDMIN (UVFUN1, TVAL, TERR, MVAR, EPS, NITER,
     *            FOPT, GNOPT, PRTLEV, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Save results, restore pointers.
C                                       Antenna values:
               ANTPNT(I) = TAPNT
               IPNT = ANTPNT(I)
               VALUE(IPNT) = TVAL(1)
               VALUE(IPNT+1) = TVAL(2)
               VALUE(IPNT+2) = TVAL(3)
               ERR(IPNT) = TERR(1)
               ERR(IPNT+1) = TERR(2)
               ERR(IPNT+2) = TERR(3)
C                                       Clock values:
               CLKPNT(I) = TCPNT
               IPNT = CLKPNT(I)
               JPNT = 4
               DO 220 J = 1,NOPOL
                  DO 210 K = 1,NOIF
                     VALUE(IPNT) = TVAL(JPNT)
                     ERR(IPNT) = TERR(JPNT)
                     JPNT = JPNT + 1
                     IPNT = IPNT + 1
 210                 CONTINUE
 220              CONTINUE
               END IF
C                                       End antenna loop
 500        CONTINUE
C                                       Enable all antennas
         DOANT = 0
C                                       Get errors
         CALL UVFUN1 (VALUE, FOPT, GRD, 1)
         FAC = NOBS
         FAC = SQRT (FAC / (NOBS - NVAR))
         IF (FOPT.LT.1.0E-20) FOPT = 1.0
         RMS = SQRT (FOPT/NUMDAT) * FAC
         DO 600 J = 1,NVAR
            ERR(J) = ERR(J) * RMS
 600        CONTINUE
         END IF
C                                       Get post fit Chi squares and get
C                                       residuals.
      CALL UVFUN1 (VALUE, CHISQ, GRD, -2)
      RMS = SQRT (CHISQ / SUMWT) * 57.296
      WRITE (MSGTXT,1600) RMS
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1120 FORMAT ('Fitting antenna number ',I4)
 1030 FORMAT ('Prefit RMS phase residuals = ',F10.5,' degrees')
 1600 FORMAT ('Postfit RMS phase residuals = ',F10.5,' degrees')
      END
      SUBROUTINE UVFUN1 (P, F, GRAD, IFLAG)
C-----------------------------------------------------------------------
C   Model routine for fitting sines/cosines of residual phases.
C   Source positions are in radians, antenna locations are in m.
C   Given the vector P of solution parameters, this subroutine computes
C   the value of the chi-squared function F (a sum of squared residuals)
C   and, optionally, the gradient, GRAD, of F w.r.t. P.  When IFLAG=1,
C   only F is computed.  Otherwise F and GRAD are both computed.  Note
C   that P is to contain only the parameters which are being solved for
C   --- not the parameters that are to be held fixed.  This subroutine
C   is called by the minimization routine DVDMIN.
C      Phase residual model from Thompson, Moran and Swenson, 1986,
C   "Interferometry and Synthesis in Radio Astronomy", John Wiley and
C   Sons, New York, Chapter 12.
C    Inputs:
C     IFLAG      I    IFLAG=1 => compute just F from sines of phases
C                     IFLAG.GT.1 => compute both F and GRAD.
C                     IFLAG=-1 => compute F from residual phases
C                     IFLAG=-2 => replace data with residuals.
C      P         D(*) Model parameters which are being fitted
C    Inputs from common:
C      NUMDAT    I     Number of data values read.
C      NOIF      I     Number of IFs
C      NOPOL     I     Number of polarizations
C      DOANT     I     Antenna number to process, 0 => all.
C      TIMDAT    D(*)  Time (days) of data.
C      HADAT     D(*)  Hour angle of observation (rad)
C      IANT      I(*)  Antenna numbers of data
C      REFDAT    I(*)  Reference antenna.
C      ISOUR     I(*)  Source numbers of data
C      IFQ       I(*)  FG numbers of data.
C      OBSDAT    R(2,2,*)Observed data, depends on ISOLM (IF,pol,ant)
C                       1 => Residual phase - Ref residual phase
C      WTDAT     R(2,2,*)Weights of data (IF,pol,ant)
C      REFTIM    D     Reference time (days) = first found
C      NANT      I      Number of antennas with data
C      ANTPNT    I(*)   Parameter no. of first ant. posn. value.
C                        0 = fixed.
C      ANTPRM    D(6,*) Antenna parameters (X, Y, Z, DX, DY, DZ)
C      ATMPNT    I(*)   Parameter no. of 1st ant. atm. parameter
C      ANTATM    R(2,*) Antenna atmospheric parameters.
C                       NOTE: atmospheric model not yet implemented
C      CLKPNT    I(*)   Parameter no. of 1st ant. clock parameter
C      ANTCLK    R(2,2,*) Antenna clock parameters. (IF,pol,ant)
C      NSOUR     I      Number of sources
C      SOUPNT    I(*)   Parameter no. of first source position value.
C      SOUPRM    D(4,*) Source parameters (RA, dec, DRA, Ddec of date).
C                       (RA and Dec in rad, DRA and Ddec in rad*SOUSCL)
C      SOUIDN    I(*)   Lookup table to translate SU table numbers to
C                       parameters list numbers.
C      SOUSCL    R      Scaling factor for source position offset.
C   Input/Outputs:
C      F         D    The value of the chi-squared function
C                     corresponding to the given P.
C      GRAD      D(*) The gradient of the chi-squared function.  I.e.,
C                      GRAD(I) = derivative of F w.r.t. P(I).
C-----------------------------------------------------------------------
      DOUBLE PRECISION P(*), F, GRAD(*)
      INTEGER   IFLAG
C
      INTEGER   I, J, K, IPNT, IIF, IPOL, ID, IA, IR, IS, IIFQ, IT
      LOGICAL   NGRAD, WANTAN
      REAL      RES, TOOPI, PI, WT, WTRES, RESRE, RESIM, RES2, CMOD,
     *   SMOD
      DOUBLE PRECISION PHSMOD, D(3), DD(3), S(3), CHA, SHA, CDEC, SDEC,
     *   TDEC, DRA, DDEC, P0, P1, P2, C1, TWOPI, CLIGI, SUM, PART
C                                       CLIGI = 1/Speed of light.
      PARAMETER (CLIGI = 1.0D0 / 2.997925E8)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BLFIT2.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      TWOPI = 8.0D0 * ATAN (1.0D0)
      PI = 4.0 * ATAN (1.0)
      TOOPI = 2.0 * PI
      NGRAD = IFLAG.LE.1
C                                       Clear GRAD
      IF (.NOT.NGRAD) THEN
         DO 10 I = 1,NVAR
            GRAD(I) = 0.0D0
 10         CONTINUE
            END IF
C                                       Get current model values
C                                       Antenna position errors
      DO 20 I = 1,NANT
         WANTAN = ((DOANT.LE.0) .OR. (I.EQ.DOANT))
         IF ((ANTPNT(I).GT.0) .AND. WANTAN) THEN
            IPNT = ANTPNT(I)
            ANTPRM(4,I) = P(IPNT)
            ANTPRM(5,I) = P(IPNT+1)
            ANTPRM(6,I) = P(IPNT+2)
            END IF
 20      CONTINUE
C                                       Source position errors
      DO 30 I = 1,NSOUR
         IF (SOUPNT(I).GT.0) THEN
            IPNT = SOUPNT(I)
            SOUPRM(3,I) = P(IPNT)
            SOUPRM(4,I) = P(IPNT+1)
            END IF
 30      CONTINUE
C                                       Instrumental terms
      DO 60 I = 1,NANT
         WANTAN = ((DOANT.LE.0) .OR. (I.EQ.DOANT))
         IF ((CLKPNT(I).GT.0) .AND. WANTAN) THEN
            IPNT = CLKPNT(I)
            DO 50 J = 1,NOPOL
               DO 40 K = 1,NOIF
C                                       In case no data:
                  IF (.NOT.GOTCOR(K,J,I)) GRAD(IPNT) = 1000.0D0
                  ANTCLK(K,J,I) = P(IPNT)
                  IPNT = IPNT + 1
 40               CONTINUE
 50            CONTINUE
            END IF
 60      CONTINUE
C                                       Compute Chi squares
C                                       sum (wt*(res**2))
      SUM = 0.0D0
C                                       Loop over data
      DO 200 ID = 1,NUMDAT
C                                       Antennas, source, FQ
         IA = IANT(ID)
         IR = REFDAT(ID)
         IT = ISOUR(ID)
         IS = SOUIDN(IT)
         IIFQ = IFQ(ID)
C                                       Want this data?
         WANTAN = ((DOANT.LE.0) .OR. (IA.EQ.DOANT))
         IF (.NOT.WANTAN) GO TO 200
C                                       Hour angle
         CHA = COS (HADAT(ID))
         SHA = SIN (HADAT(ID))
C                                       Declination
         CDEC = COS (SOUPRM(2,IS))
         SDEC = SIN (SOUPRM(2,IS))
         TDEC = TAN (SOUPRM(2,IS))
C                                       Source and baseline vectors
         D(1) = ANTPRM(1,IA) - ANTPRM(1,IR)
         D(2) = ANTPRM(2,IA) - ANTPRM(2,IR)
         D(3) = ANTPRM(3,IA) - ANTPRM(3,IR)
         DD(1) = ANTPRM(4,IR) - ANTPRM(4,IA)
         DD(2) = ANTPRM(5,IR) - ANTPRM(5,IA)
         DD(3) = ANTPRM(6,IR) - ANTPRM(6,IA)
         S(1) = CDEC * CHA
         S(2) = -CDEC * SHA
         S(3) = SDEC
         DDEC = SOUPRM(4,IS) * SOUSCL
         DRA =  SOUPRM(3,IS) * SOUSCL
         P0 = -(DD(3) * SDEC + D(3) * DDEC * CDEC)
         P1 = -DD(1) + D(1) * DDEC * TDEC - D(2) * DRA
         P2 = DD(2) - D(1) * DRA - D(2) * DDEC * TDEC
         DO 190 IPOL = 1,NOPOL
            DO 180 IIF = 1,NOIF
               IF (WTDAT(IIF,IPOL,ID).LE.0.0) GO TO 180
               WT = WTDAT(IIF,IPOL,ID)
C                                       C1 = 2 pi / wavelength
               C1 = TWOPI * CLIGI * FRQIF(IIF,IIFQ)
               PHSMOD = ANTCLK(IIF,IPOL,IA) + C1 * (P0 + P1 * S(1) -
     *            P2 * S(2))
               PHSMOD = MOD (PHSMOD, TWOPI)
C                                       Residual cosine(phase) or phase?
               IF (IFLAG.GT.0) THEN
                  CMOD = COS (PHSMOD)
                  SMOD = SIN (PHSMOD)
                  RESRE = CMOD - COS (OBSDAT(IIF,IPOL,ID))
                  RESIM = SMOD - SIN (OBSDAT(IIF,IPOL,ID))
                  RES2 = RESRE*RESRE + RESIM*RESIM
               ELSE
C                                       Phase residual - less than pi
                  RES = PHSMOD - OBSDAT(IIF,IPOL,ID)
                  RES = MOD (RES, TOOPI)
                  IF (RES.GT.PI) RES = RES - TOOPI
                  IF (RES.LT.-PI) RES = RES + TOOPI
C                                       Replace data with residual?
                  IF (IFLAG.EQ.-2) OBSDAT(IIF,IPOL,ID) = RES
                  RES2 = RES * RES
                  END IF
C                                       Sum chi squares
               SUM = SUM + WT * RES2
               IF (NGRAD) GO TO 180
C                                       Gradient wanted - do partials
               WTRES = 2.0 * WT * (RESIM * CMOD - RESRE * SMOD)
               IF (SOUPNT(IS).GT.0) THEN
C                                       Delta RA
                  PART = -C1 * (S(1) * D(2) - S(2) * D(1))
                  IPNT = SOUPNT(IS)
                  GRAD(IPNT) = GRAD(IPNT) + PART * WTRES * SOUSCL
C                                       Delta dec
                  PART = -C1 * (D(3) * CDEC - S(1) * D(1) * TDEC -
     *               S(2) * D(2) * TDEC)
                  GRAD(IPNT+1) = GRAD(IPNT+1) + PART * WTRES * SOUSCL
                  END IF
C                                       Antenna
               IF (ANTPNT(IA).GT.0) THEN
C                                       Delta X
                  PART = C1 * S(1)
                  IPNT = ANTPNT(IA)
                  GRAD(IPNT) = GRAD(IPNT) + PART * WTRES
C                                       Delta Y
                  PART = C1 * S(2)
                  GRAD(IPNT+1) = GRAD(IPNT+1) + PART * WTRES
C                                       Delta Z
                  PART = C1 * SDEC
                  GRAD(IPNT+2) = GRAD(IPNT+2) + PART * WTRES
                  END IF
C                                       Instrumental phase
               IF (CLKPNT(IA).GT.0) THEN
                  PART = 1.0D0
                  IPNT = CLKPNT(IA) + (IPOL-1) * NOIF + IIF - 1
                  GRAD(IPNT) = GRAD(IPNT) + PART * WTRES
                  END IF
C                                       Ref. antenna
               IF (ANTPNT(IR).GT.0) THEN
C                                       Delta X
                  PART = -S(1) * C1
                  IPNT = ANTPNT(IR)
                  GRAD(IPNT) = GRAD(IPNT) - PART * WTRES
C                                       Delta Y
                  PART = -S(2) * C1
                  GRAD(IPNT+1) = GRAD(IPNT+1) - PART * WTRES
C                                       Delta Z
                  PART = -C1 * SDEC
                  GRAD(IPNT+2) = GRAD(IPNT+2) - PART * WTRES
                  END IF
C                                       Instrumental phase
               IF (CLKPNT(IR).GT.0) THEN
                  PART = -1.0D0
                  IPNT = CLKPNT(IR) + (IPOL-1) * NOIF + IIF - 1
                  GRAD(IPNT) = GRAD(IPNT) + PART * WTRES
                  END IF
 180           CONTINUE
 190        CONTINUE
 200     CONTINUE
      F = SUM
C
 999  RETURN
      END
      SUBROUTINE DVDMIN (FX, XI, ERR, N, EPS, ITMAX, FOPT, GNOPT,
     *   NPR, IER)
C-----------------------------------------------------------------------
C    This is a Fortran implementation of Davidon's optimally conditioned
C  variable metric (quasi-Newton) method for function minimization.  It
C  is based on the algorithm given in W. C. Davidon:  Optimally condi-
C  tioned optimization algorithms without line searches, Mathematical
C  Programming, vol. 9 (1975) pp. 1-30.  One should refer to that re-
C  ference for the algorithmic details.  Here, the steps of the
C  algorithm which are delineated by COMMENT lines correspond to the
C  numbered steps in Davidon's paper.  The user must supply a subroutine
C  FX to calculate the objective function and its gradient at a given
C  point.  The objective function F is assumed to be a real-valued
C  function of N real variables.  Here, 0 is assumed to be a lower
C  bound for F.  If F can assume negative values, Step 2 must be modi-
C  fied in one of two different ways, depending on whether a lower
C  bound is known (see Davidon for details).
C  Inputs:
C    FX      ENTRY     A user-supplied subroutine of the form
C                      FX (X, F, G, K) which is used to calculate the
C                      value of the objective function F at X and, op-
C                      tionally, the gradient G of F at X.  When K=1, FX
C                      need only compute F.  When K=2, both F and G are
C                      required.
C    XI(N)   D         An initial estimate for the location of a mini-
C                      mum.
C    ERR(N)  D         An initial estimate of the square roots of the
C                      diagonal elements of the inverse of the Hessian
C                      matrix of the objective function evaluated at XI.
C                      When no estimates are known, it should suffice to
C                      set ERR(I)=1.0D0, for all I.
C    N       I         The number of unknowns.
C    EPS     D         A small positive number used in tests to set a
C                      lower bound on the squared Euclidean norm of
C                      vectors considered significantly different from
C                      0.  EPS is used in the convergence test.  Usually
C                      setting EPS in the range 10**(-12) to 10**(-8) is
C                      reasonable.  Very close to a minimum, the algo-
C                      rithm generally exhibits a quadratic rate of con-
C                      vergence, so setting EPS a few orders of magni-
C                      tude too small usually is not too costly.
C    ITMAX   I         The maximum number of iterations.  On average, a
C                      few evaluations of F and slightly more than one
C                      evaluation of G are required at each iteration.
C    NPR     I         A print flag.  When NPR=0, there is no printout;
C                      for NPR=1, the value of F and the Euclidean norm
C                      of G, both evaluated at the location of the best
C                      minimum found so far, are printed at each itera-
C                      tion; for NPR=2, the latter information, together
C                      with the location of the best minimum, is print-
C                      ed at each iteration.
C  Outputs:
C    XI(N)   I         The user-supplied initial guess is replaced by
C                      the location of the best minimum found by the al-
C                      gorithm.
C    ERR(N)  D         The initial estimate supplied by the user is re-
C                      placed by an estimate of the square roots of the
C                      diagonal elements of the Hessian matrix evaluated
C                      at the best minimum found.  In least-squares ap-
C                      plications, assuming that F is the sum of squared
C                      residuals, estimates of the standard errors of
C                      the unknowns can be obtained by multiplying ERR
C                      by the r.m.s. residual.
C    FOPT     D        The value of F evaluated at the location of the
C                      best minimum that was found.
C    GNOPT    D        The Euclidean norm of the gradient of the objec-
C                      tive function, evaluated at the location of the
C                      best minimum that was found.
C    IER      I        An error flag.  When IER=0, convergence was
C                      achieved in ITMAX or fewer iterations; other-
C                      wise not.
C  Remarks:
C  1)
C     This algorithm can be used for under-determined problems.
C  2) It maintains an approximation, in factored form J*transpose(J),
C     to the inverse Hessian of F.  At each iteration, a rank two update
C     is added to this approximation.  This approximation remains posi-
C     tive definite throughout the iteration.  In cases where an un-
C     known, say the Ith unknown, is ill-determined, ERR(I) will be
C     finite on exit from this routine. So, in least-squares applica-
C     tions, the error estimates for ill-determined parameters are like-
C     ly to be too small.
C  2.5) In the case of an under-determined problem (i.e., when the
C     Hessian matrix is singular) J*transpose(J) is a non-singular
C     matrix whose inverse is close to the Hessian matrix.
C  3) Furthermore, in cases where an excellent initial guess is supplied
C     by the user, DVDMIN is likely to converge before it has iterated
C     long enough to get a good approximation to the inverse Hessian.
C     (Understand that it is trying to estimate this second-order in-
C     formation only from the first-order information that is supplied
C     by FX.)  So, in least-squares applications, when convergence oc-
C     curs in just a couple of iterations, the derived error estimates
C     may be inaccurate.
C  4) Another Fortran implementation is given in the technical report
C     by W. C. Davidon and L. Nazareth:  DRVOCR - A Fortran implementa-
C     tion of Davidon's optimally conditioned method, Argonne National
C     Lab., Applied Math. Div. Technical Memo. No. 306, August 1977.
C  5) Comparisons of Davidon's algorithm with other quasi-Newton mini-
C     mization algorithms are given in  J. N. Lyness:  A bench mark
C     experiment for minimization algorithms, Math. of Computation,
C     vol. 33 (1979) pp. 249-264.  This algorithm compares quite favor-
C     ably with others, including the routine QNMDER of Gill et al.,
C     and the Harwell Library routine VA13AD.
C  6) Argonne Lab.'s MINPACK routines (non-proprietary) or NAG Library
C     routines (proprietary) could be used in place of DVDMIN.  They
C     would provide somewhat more flexibility.  They're a bit more con-
C     servative (and therefore more robust, but perhaps less efficient).
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BLFIT2.INC'
C                                       MAXUNK = max. no unknowns.
      INTEGER   MAXUNK
      PARAMETER (MAXUNK=XMXPRM)
      INTEGER   N, ITMAX, NPR, IER, NF, NG, IT, I, J, L, I1, II
      DOUBLE PRECISION XI(*), ERR(*), EPS, FOPT, GNOPT, LAMBDA, F0,
     *   MSQ, MU, NSQ, NU, XJ(MAXUNK,MAXUNK), X0(MAXUNK), X(MAXUNK),
     *   K0(MAXUNK), K(MAXUNK), S(MAXUNK), G(MAXUNK), M(MAXUNK),
     *   P(MAXUNK), Q(MAXUNK), WUN(MAXUNK), AX(MAXUNK),
     *   XDDOT, XDMACH, TINYC, F, GN, F0P, XX, UTU, FP,
     *   UTS, A, B, C, ALF, GAMMA, DELTA, T1, T2, T3, T4, T5, T6,
     *   QTK0, B0
      EXTERNAL FX
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Initialization:
      CALL DMACH (2, XDMACH)
      TINYC = 1.0D-3*SQRT (XDMACH)
      NF = 1
      NG = 1
      IT = -1
      DO 20 I = 1,N
         X(I) = XI(I)
         X0(I) = XI(I)
         DO 10 J = 1,N
            XJ(I,J) = 0.0D0
 10         CONTINUE
         XJ(I,I) = ERR(I)
 20      CONTINUE
      CALL FX (X, F, G, 2)
      F0 = F
      DO 40 I = 1,N
         DO 30 J = 1,N
            AX(J) = XJ(J,I)
 30         CONTINUE
         CALL DDOT (N, AX, 1, G, 1, WUN(I))
         K0(I) = WUN(I)
 40      CONTINUE
C                                       Step 1:
 100  IT = IT+1
      CALL DNRM2 (N, G, 1, GN)
      IF (NPR.GE.1) WRITE (MSGTXT,1010) IT, F0, GN
      IF (NPR.GE.1) CALL MSGWRT (6)
      IF (NPR.GT.1) WRITE (MSGTXT,1020)
      IF (NPR.GT.1) CALL MSGWRT (6)
      DO 105 I = 1,N,6
         I1 = I + 5
         I1 = MIN (I1, N)
         IF (NPR.GT.1) WRITE (MSGTXT,1021) (X0(II), II = I,I1)
         IF (NPR.GT.1) CALL MSGWRT (6)
 105     CONTINUE
      IF (IT.LT.ITMAX) GO TO 110
         IER = 1
         GO TO 900
 110  DO 120 I = 1, N
         S(I) = -K0(I)
 120     CONTINUE
      CALL DDOT (N, K0, 1, S, 1, F0P)
      LAMBDA = 2.0D0
      IF (4.0D0*F0.GE.-F0P) GO TO 200
         XX = -4.0D0*F0/F0P
         DO 130 I = 1, N
            S(I) = XX*S(I)
 130        CONTINUE
         F0P = -4.0D0*F0
C                                       Step 2:
 200  DO 220 I = 1,N
         DO 210 J = 1,N
            AX(J) = XJ(I,J)
 210        CONTINUE
         CALL DDOT (N, AX, 1, S, 1, XDDOT)
         X(I) = X0(I) + XDDOT
 220     CONTINUE
      IF (-F0P.GE.EPS) GO TO 230
         IER = 0
         GO TO 900
 230  CALL FX (X, F, G, 1)
      NF = NF+1
      IF (F.LT.F0) GO TO  300
         DO 240 I = 1,N
            S(I) = 0.5D0*S(I)
 240        CONTINUE
         F0P = 0.5D0*F0P
         LAMBDA = 0.5D0
         GO TO 200
C                                       Step 3:
 300  CALL FX (X, F, G, 2)
      NF = NF+1
      NG = NG+1
      DO 320 I = 1,N
         DO 310 J = 1,N
            AX(J) = XJ(J,I)
 310        CONTINUE
         CALL DDOT (N, AX, 1, G, 1, K(I))
         M(I) = S(I)+K0(I)-K(I)
         K0(I) = K(I)
         X0(I) = X(I)
 320     CONTINUE
      CALL DDOT (N, K, 1, S, 1, FP)
      B0 = FP-F0P
      F0 = F
      F0P = FP
      IF (B0.GE.EPS) GO TO 400
         DO 330 I = 1,N
            S(I) = LAMBDA*S(I)
 330        CONTINUE
         F0P = LAMBDA*F0P
         GO TO 200
C                                       Step 4:
 400  CALL DNRM2 (N, M, 1, MSQ)
      MSQ = MSQ * MSQ
      IF (MSQ.LT.EPS) GO TO 100
         CALL DDOT (N, M, 1, S, 1, NU)
         MU = NU-MSQ
         CALL DDOT (N, M, 1, WUN, 1, XX)
         XX = XX / MSQ
         DO 410 I = 1,N
            WUN(I) = WUN(I)-XX*M(I)
 410        CONTINUE
         CALL DNRM2 (N, WUN, 1, UTU)
         UTU = UTU * UTU
C                                       Trap trouble
         UTU = MAX (UTU, EPS)
         CALL DDOT (N, M, 1, WUN, 1, XX)
         IF (XX.LT.TINYC .OR. ((1D3*XX)**2.LT.MSQ*UTU)) GO TO 450
            DO 420 I = 1,N
               WUN(I) = 0.0D0
 420           CONTINUE
            NSQ = 0.0D0
            GO TO 500
C                                       Step 4A:
 450     CALL DDOT (N, WUN, 1, S, 1, UTS)
         XX = UTS / UTU
         DO 460 I = 1,N
            WUN(I) = XX * WUN(I)
 460        CONTINUE
         NSQ = UTS * XX
C                                       Step 5:
 500  XX = NU / MSQ
      B = NSQ + MU*XX
      IF (B.GE.EPS) GO TO 600
         DO 510 I = 1,N
            WUN(I) = S(I) - XX*M(I)
 510        CONTINUE
         NSQ = B0 - MU*XX
         B = B0
C                                       Step 6:
 600  IF (MU*NU.LT.MSQ*NSQ) GO TO 650
         GAMMA = 0.0D0
         DELTA = SQRT (NU/MU)
         GO TO 700
C                                       Step 6A:
 650  A = B-MU
      C = B+NU
      GAMMA = SQRT ((1.0D0 - MU*NU/(MSQ*NSQ))/(A*B))
      DELTA = SQRT (C / A)
      IF (C.LT.A) GAMMA = -GAMMA
C                                       Step 7:
 700  XX = NSQ * GAMMA
      ALF = NU + MU*DELTA + MSQ*XX
      T1 = DELTA - XX
      T2 = GAMMA * NU
      T3 = (1.0D0+XX) / ALF
      T4 = -GAMMA * MU / ALF
      XX = MU * NU / ALF
      T5 = NSQ * (1.0D0 + GAMMA*XX)
      T6 = -(1.0D0 + DELTA) * XX
      DO 710 I = 1,N
         P(I) = T1*M(I) + T2*WUN(I)
         Q(I) = T3*M(I) + T4*WUN(I)
         WUN(I) = T5*M(I) + T6*WUN(I)
 710     CONTINUE
      CALL DDOT (N, Q, 1, K0, 1, QTK0)
      DO 730 I = 1,N
         K0(I) = K0(I) + QTK0*P(I)
         DO 720 L = 1,N
            AX(L) = XJ(I,L)
 720        CONTINUE
         CALL DDOT (N, AX, 1, Q, 1, XX)
         DO 730 J = 1,N
            XJ(I,J) = XJ(I,J) + XX*P(J)
 730        CONTINUE
      IF (NSQ.GT.0.0D0) GO TO 100
         DO 740 I = 1,N
            WUN(I) = K0(I)
 740        CONTINUE
         GO TO 100
C                                       Exit:
 900  DO 920 I = 1,N
         XI(I) = X0(I)
         DO 910 J = 1,N
            AX(J) = XJ(I,J)
 910        CONTINUE
         CALL DNRM2 (N, AX, 1, ERR(I))
 920     CONTINUE
      FOPT = F0
      GNOPT = GN
      IF (NPR.LE.0) GO TO 999
         IF (IER.EQ.0) WRITE (MSGTXT,1030)
         IF (IER.EQ.0) CALL MSGWRT (6)
         IF (IER.EQ.1) WRITE (MSGTXT,1040)
         IF (IER.EQ.1) CALL MSGWRT (6)
         WRITE (MSGTXT,1050) NF,NG
         CALL MSGWRT (6)
         WRITE (MSGTXT,1060)
         CALL MSGWRT (6)
         DO 930 I = 1,N,6
            I1 = I + 5
            I1 = MIN (I1, N)
            WRITE (MSGTXT,1061) (XI(II), II = I,I1)
            CALL MSGWRT (6)
 930        CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (' Iteration #',I4,5X,' F=',1PD16.8,' Gradient=',1PD16.8)
 1020 FORMAT (' Parameters:')
 1021 FORMAT (1X,1PD13.5,5D13.5)
 1030 FORMAT (' ***  Convergence achieved.')
 1040 FORMAT (' ***  Maximum number of iterations reached.')
 1050 FORMAT (1X,I4,' Function evaluations and ',I4,
     *   ' Gradient evaluations.')
 1060 FORMAT (' Solution parameters:')
 1061 FORMAT (1X,1PD13.5,5D13.5)
      END

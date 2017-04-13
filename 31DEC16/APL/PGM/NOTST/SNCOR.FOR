LOCAL INCLUDE 'SNCOR.INC'
C                                                          Include SNCOR
C                                       Local include for SNCOR
C                                       Needs parameter from PUVD.INC
C                                       Inputs and general info
      INTEGER   SEQIN, SUBA, DISKIN, CNOIN, NUMHIS, SNVER, NSOUWD,
     *   SOUWAN(30), NANTSL, ANTENS(50), BIF, EIF, ISTOK, FREQID, KANT
      LOGICAL   DOSWNT, DOAWNT
      REAL      XSIN, XDISIN, XFQID, XBAND, XFREQ, XBIF, XEIF, XTIME(8),
     *   XANT(50), XSUBA, XGVER, BPARM(30), XPARM(30), XBAD(10), SELBAN
      CHARACTER  HISCRD(50)*64, NAMEIN*12, CLAIN*6, XSOUR(30)*16,
     *   XSTOK*4, OPCODE*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXSTOK, XOPCOD
      DOUBLE PRECISION FRQOFF(MAXIF), SELFRQ
C                                       Buffers and file info
      INTEGER   BUFFER(1024)
C                                       Important constants
      DOUBLE PRECISION PI, TWOPI, SIDER, CLIGHT
C                                       Internal storage
      INTEGER   SNRECI(10+15*MAXIF), SNKOLS(MAXSNC), SNNUMV(MAXSNC),
     *   NUMANT, NUMPOL, NUMIF, ICODE, FIXCNT,
     *   TIMSN, INTSN, SOUSN, ANTSN, SUBSN, FRQSN, IFRSN, NODSN,
     *   RE1SN, IM1SN, DL1SN, RA1SN, WT1SN, RF1SN,
     *   RE2SN, IM2SN, DL2SN, RA2SN, WT2SN, RF2SN
      REAL      GMMOD, SNRECR(10+15*MAXIF), PARM(40), PANGLE(MAXANT)
      DOUBLE PRECISION COSDEC, SINDEC, SNRECD(10+15*MAXIF)
C                                       Inputs and general info
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN,
     *   XXSOUR, XXSTOK, XBAND, XFREQ, XFQID, XBIF, XEIF, XTIME,
     *   XANT,  XSUBA, XGVER, XOPCOD, BPARM, XPARM, XBAD, SELBAN,
     *   SEQIN, DISKIN, CNOIN, SUBA, SNVER
      COMMON /CINFO/ FRQOFF, SELFRQ,
     *   DOSWNT, DOAWNT,
     *   NSOUWD, SOUWAN, NANTSL, ANTENS, BIF, EIF, ISTOK, FREQID,
     *   KANT, NUMHIS
      COMMON /CHRCOM/ HISCRD, NAMEIN, CLAIN, XSOUR, XSTOK, OPCODE
C                                       Buffers and file info
      COMMON /BUFRS/ BUFFER
C                                       Important constants
      COMMON /CONST/ PI, TWOPI, SIDER, CLIGHT
C                                       Internal storage
      COMMON /SNRECC/ COSDEC, SINDEC, SNRECD,
     *   GMMOD, PARM, PANGLE,
     *   FIXCNT, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, ICODE,
     *   TIMSN, INTSN, SOUSN, ANTSN, SUBSN, FRQSN, IFRSN, NODSN,
     *   RE1SN, IM1SN, DL1SN, RA1SN, WT1SN, RF1SN,
     *   RE2SN, IM2SN, DL2SN, RA2SN, WT2SN, RF2SN
      EQUIVALENCE (SNRECI, SNRECR, SNRECD)
C                                                          End SNCOR
LOCAL END
      PROGRAM SNCOR
C-----------------------------------------------------------------------
C! Determines applies calibration corrections to the SN table.
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2012
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
C   Task SNCOR applies corrections to SN tables.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'SNCOR '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      XPARM(10) = 0.0
      CALL SNCLIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Apply corrections
      CALL SNCUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy and update HI file.
      CALL SNCLHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
 999  STOP
      END
      SUBROUTINE SNCLIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   SNCLIN gets input parameters for SNCOR.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                1 => Invalid request
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   JERR
C
      CHARACTER STAT*4, UTYPE*2
      LOGICAL   T, F, ALLANT, DESEL, MATCH, WEXIST, WTABLE, WFITS
      INTEGER   NPARM, IERR, I, NEXT, IARG, LIMIT, J, IROUND, LUN, IIVER
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DUMMY(MAXIF)
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /29/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T, BUFFER)
      CALL VHDRIN
      NUMHIS = 0
C                                       Set important constants
      PI = 3.1415926536D0
      TWOPI = 6.2831853072D0
      SIDER = 1.002737923D0
      CLIGHT = 2.997925D8
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 264
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SUBA = XSUBA + 0.5
      IF (SUBA.LE.0) SUBA = 1
      SNVER = IROUND (XGVER)
      DO 20 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 20      CONTINUE
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 25      CONTINUE
C                                       Find file, read CATBLK
 30   CNOIN = 1
      STAT = 'SRCH'
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
 40   CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 45
         WRITE (MSGTXT,1040) IERR
         GO TO 990
 45   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
 50   CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      NRPARM = CATBLK(KIPCN)
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FREQID = IROUND (XFQID)
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FREQID, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       IF range
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         IF (BIF.LE.0) BIF = 1
         IF (EIF.LE.0) EIF = CATBLK(KINAX+JLOCIF)
         IF (EIF.LE.0) EIF = 1
         IF (BIF.GT.CATBLK(KINAX+JLOCIF)) BIF = CATBLK(KINAX+JLOCIF)
         IF (EIF.GT.CATBLK(KINAX+JLOCIF)) EIF = CATBLK(KINAX+JLOCIF)
         END IF
C                                       Stokes' type.
      ISTOK = 0
      IF (XSTOK.EQ.'R   ') ISTOK = 1
      IF (XSTOK.EQ.'L   ') ISTOK = MIN (2, CATBLK(KINAX+JLOCS))
C                                       Check Stokes'
      IF (ISTOK.EQ.0) THEN
C                                       If none selected take what you
C                                       have.
            IF (CATBLK(KINAX+JLOCS).EQ.1) ISTOK = 1
      ELSE
C                                       Is selected Stokes' available?
         IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND.
     *      (ABS (CATD(KDCRV+JLOCS)+ISTOK).GT.0.5D0)) THEN
            JERR = 1
            MSGTXT = 'STOKES ' // XSTOK // ' UNAVAILABLE IN DATA'
            GO TO 990
            END IF
         END IF
      JERR = 0
C                                       Antenna list
      ALLANT = T
      DESEL = F
      DO 100 I = 1,50
         ANTENS(I) = 0
         ALLANT = ALLANT .AND. (ABS (XANT(I)).LE.1.0E-10)
         DESEL = DESEL .OR. (XANT(I).LT.-0.5)
 100     CONTINUE
      NEXT = 1
      IF (ALLANT) GO TO 160
C                                       Not all selected - make list
C                                       ANTENNAS array.
         DO 150 I = 1,50
            IARG = ABS (XANT(I)) + 0.5
            IF (IARG.EQ.0) GO TO 150
C                                       See if already have
               LIMIT = NEXT - 1
               IF (LIMIT.LT.1) GO TO 140
               DO 130 J = 1,LIMIT
                  IF (IARG.EQ.ANTENS(J)) GO TO 150
 130              CONTINUE
C                                       New antenna
 140              ANTENS(NEXT) = IARG
                  NEXT = NEXT + 1
 150           CONTINUE
 160  DOAWNT = .NOT. DESEL
      NANTSL = NEXT - 1
C                                       Get source numbers:
C                                       Check if single-source file
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUN, BUFFER, WTABLE, WEXIST,
     *   WFITS, IERR)
      IF ((IERR.EQ.0).AND.WEXIST.AND.WTABLE) THEN
         CALL FNDSOU (DISKIN, CNOIN, XSOUR, BUFFER, NSOUWD, DOSWNT,
     *      SOUWAN, JERR)
         IF (JERR.NE.0) GO TO 999
      ELSE
         NSOUWD = 0
         END IF
C                                       Get antenna info
      CALL GETANT (DISKIN, CNOIN, SUBA, CATBLK, BUFFER, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Get IF information
      IIVER = 1
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *   NUMIF, FRQOFF, DUMMY, FINC, BNDCOD, FREQID, JERR)
      IF (JERR.NE.0) GO TO 999
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SNCLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
       END
      SUBROUTINE SNCUV (IERR)
C-----------------------------------------------------------------------
C   SNCUV is called from SNCOR. SNCUV reads throught the SN table,
C   passing the records selected to the correction routine SNCCOR.
C   Output: IERR  I    Return code, 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LUN, IRCODE, THSOU, LSTSOU, ANT, I, J, JERR, ISNRNO,
     *   NUMREC, LOOP, NUMNOD
      LOGICAL   MAT, F, ISAPL
      REAL      RANOD(25), DECNOD(25)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      DOUBLE PRECISION TIMBEG, TIMEND
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /29/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      FIXCNT = 0
C                                       Timerange
      TIMBEG = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   (XTIME(4) / (24.0*60.0*60.0))
      TIMEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   (XTIME(8) / (24.0*60.0*60.0))
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMEND.LT.1.0E-5)) TIMEND = 1.0E20
C                                       Open SN table, Read for info
      CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, LUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPL, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABIO ('CLOS', IRCODE, LOOP, SNRECR, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Reformat table?
      CALL SNREFM (DISKIN, CNOIN, SNVER, CATBLK, LUN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL SNINI ('WRIT', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, LUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
      IRCODE = 0
C                                       Set table pointers
      TIMSN = SNKOLS(SNDTIM)
      INTSN = SNKOLS(SNRTMI)
      SOUSN = SNKOLS(SNISID)
      ANTSN = SNKOLS(SNIANT)
      SUBSN = SNKOLS(SNISUB)
      FRQSN = SNKOLS(SNIFQI)
      IFRSN = SNKOLS(SNRIFR)
      NODSN = SNKOLS(SNINOD)
      RE1SN = SNKOLS(SNRRE1)
      IM1SN = SNKOLS(SNRIM1)
      RA1SN = SNKOLS(SNRRA1)
      DL1SN = SNKOLS(SNRDE1)
      WT1SN = SNKOLS(SNRWE1)
      RF1SN = SNKOLS(SNIRF1)
      RE2SN = SNKOLS(SNRRE2)
      IM2SN = SNKOLS(SNRIM2)
      RA2SN = SNKOLS(SNRRA2)
      DL2SN = SNKOLS(SNRDE2)
      WT2SN = SNKOLS(SNRWE2)
      RF2SN = SNKOLS(SNIRF2)
C                                       Initial call to SNCCOR
      CALL SNCCOR (1, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Update table
      DO 500 LOOP = 1,NUMREC
         ISNRNO = LOOP
         CALL TABIO ('READ', IRCODE, ISNRNO, SNRECR, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 900
         IF (IERR.LT.0) GO TO 500
C                                       Check data
C                                       Time:
         IF ((SNRECD(TIMSN).LT.TIMBEG) .OR.
     *       (SNRECD(TIMSN).GT.TIMEND)) GO TO 500
C                                       Subarray
         IF ((SNRECI(SUBSN).NE.SUBA) .AND. (SNRECI(SUBSN).GT.0))
     *      GO TO 500
C                                       Freq id
         IF ((SNRECI(FRQSN).NE.FREQID) .AND. (SNRECI(FRQSN).GT.0) .AND.
     *      (FREQID.GT.0)) GO TO 500
         IF (NSOUWD.LE.0) GO TO 70
C                                       Check list
         THSOU = SNRECI(SOUSN)
         DO 60 J = 1,NSOUWD
            IF ((THSOU.EQ.SOUWAN(J)) .AND. DOSWNT) GO TO 70
            IF ((THSOU.EQ.SOUWAN(J)) .AND. (.NOT.DOSWNT)) GO TO 500
 60         CONTINUE
         IF (DOSWNT) GO TO 500
 70      LSTSOU = THSOU
C                                       See if all antennas desired
         KANT = SNRECI(ANTSN)
         IF (NANTSL.LE.0) GO TO 130
            ANT = SNRECI(ANTSN)
            MAT = F
            DO 120 I = 1,NANTSL
               MAT = MAT .OR. (ANT.EQ.ANTENS(I))
 120           CONTINUE
C                                       Check for match selected.
         IF (DOAWNT .AND. MAT) GO TO 130
C                                       Check for match excluded
         IF (.NOT.DOAWNT .AND. MAT) GO TO 500
C                                       If inclusion ignore
         IF (DOAWNT) GO TO 500
C                                       Correct record.
 130     CALL SNCCOR (2, JERR)
         IF (JERR.NE.0) GO TO 500
C                                       Rewrite record
         CALL TABIO ('WRIT', IRCODE, ISNRNO, SNRECR, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 900
 500     CONTINUE
C                                       Final call to SNCCOR
         CALL SNCCOR (3, JERR)
CC                                       Update GMMOD
C      IF (ABS (GMMOD-1.0).LE.1.0E-5) GO TO 600
C      CALL TABKEY ('WRIT', KEYWRD, 1, BUFFER, 1, GMMOD, 2, IERR)
C                                       Close table.
 600  CALL TABIO ('CLOS', IRCODE, LOOP, SNRECR, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 900
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IERR
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('TABIO ERROR',I3,' CORRECTING SN TABLE')
      END
      SUBROUTINE SNCCOR (IOP, IERR)
C-----------------------------------------------------------------------
C   SNCCOR applies corrections to the SN record passed thru common
C   /SNRECC/.
C   Input:
C    IOP        I    Operation code, 1=init, 2=process, 3=finish
C   Input from common:
C    SNRECI(*)  I    The SN table record to be corrected.
C    BIF        I    First IF number
C    EIF        I    Highest IF number
C    ISTOK      I    Stokes number, 0=both, 1=first, 2=second.
C    OPCODE     C*4  Operation code.
C    ICODE      I    Operation code number, set on first call.
C    BPARM(30)  R    parameters.
C    XPARM(30)  R        "
C   Output in common:
C    SNRECI(*)  I    Modified record.
C   Output:
C    IERR       I    Error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   IOP, IERR
C
      INTEGER   NOP
      PARAMETER (NOP = 17)
      CHARACTER  OPS(NOP)*4
      INTEGER   I, IANT, ITEMP, JTEMP, IROUND
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA OPS /'AVRT','CLPA', 'CLPP', 'XFER', 'ZPHS', 'ZRAT',
     *   'MULA', 'CLPD', 'CLPR', 'REFP', 'ZDEL', 'CPRT', 'CPSN',
     *   'PCOP', 'PNEG', 'CLPW', 'NORM'/
C-----------------------------------------------------------------------
C                                       Determine operation
      IF (IOP.EQ.2) GO TO 200
      IF (IOP.EQ.3) GO TO 900
C                                       Initialize - find OPCODE
      ICODE = -1
      DO 30 I = 1,NOP
         IF (OPS(I).EQ.OPCODE) ICODE = I
 30      CONTINUE
      IF (ICODE.GT.0) GO TO 40
         IERR = 1
         WRITE (MSGTXT,1030) OPCODE
         GO TO 990
C                                       History - OPCODE
 40      NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2040) OPCODE
C                                       Setup
      GO TO (45,50,55,90,60,65,70,75,80,85,95,100,105,110,115,120,125),
     *   ICODE
C                                       OPCODE='AVRT'(1)
C                                       Average Rates - no special
C                                       parameters needed.
 45      CONTINUE
         GO TO 195
C                                       OPCODE='CLPA'(2)
C                                       Clip Amplitudes
 50      CONTINUE
C                                       Save range
         PARM(1) = MIN (BPARM(1), BPARM(2))
         PARM(2) = MAX (BPARM(1), BPARM(2))
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2050) PARM(1), PARM(2)
         GO TO 195
C                                       OPCODE='CLPP'(3)
C                                       Clip phases
 55      CONTINUE
C                                       Save range
         PARM(1) = MIN (BPARM(1), BPARM(2)) / 57.296
         PARM(2) = MAX (BPARM(1), BPARM(2)) / 57.296
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2055) BPARM(1), BPARM(2)
         GO TO 195
C                                       OPCODE='ZPHS'(5)
C                                       Zero phases.
 60      CONTINUE
         GO TO 195
C                                       OPCODE='ZRAT'(6)
C                                       Zero rates.
 65      CONTINUE
C                                       No History records.
         GO TO 195
C                                       OPCODE='MULA'(7)
C                                       Multiply amplitudes
 70      CONTINUE
         PARM(1) = BPARM(1)
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2060) BPARM(1)
         GO TO 195
C                                       OPCODE='CLPD'(8)
C                                       Clip Delays
C                                       Save range in sec.
 75      PARM(1) = MIN (BPARM(1), BPARM(2)) * 1.0E-9
         PARM(2) = MAX (BPARM(1), BPARM(2)) * 1.0E-9
         BPARM(1) = PARM(1) * 1.0E9
         BPARM(2) = PARM(2) * 1.0E9
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2075) BPARM(1), BPARM(2)
         GO TO 195
C                                       OPCODE='CLPR'(9)
C                                       Clip Rates
C                                       Save range in sec/sec at ref.
C                                       freq.
 80      PARM(1) = MIN (BPARM(1), BPARM(2)) * 1.0E-3 / FREQ
         PARM(2) = MAX (BPARM(1), BPARM(2)) * 1.0E-3 / FREQ
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2080) BPARM(1), BPARM(2)
         GO TO 195
C                                       OPCODE='REFP'(10)
C                                       Reference phases to one IF.
 85      ITEMP = BPARM(1) + 0.5
         IF (ITEMP.LE.0) ITEMP = 1
         IF ((JLOCIF.GE.0) .AND. (ITEMP.GT.CATBLK(KINAX+JLOCIF)))
     *      ITEMP = 1
         PARM(1) = ITEMP
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2085) ITEMP
         GO TO 195
C                                       OPCODE='XFER'(4)
C                                       Transfer phases
 90      DO 91 I = 1, 30, 3
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2090) BPARM(I), BPARM(I+1),
     *         BPARM(I+2)
 91         CONTINUE
         DO 92 I = 1, 30, 3
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),2095) XPARM(I), XPARM(I+1),
     *         XPARM(I+2)
 92         CONTINUE
         GO TO 195
C                                       OPCODE='ZDEL'(11)
C                                       Zero delays
 95      CONTINUE
         GO TO 195
C                                       OPCODE='CPRT'(12)
C                                       Copy IF rate to other IFs
  100    ITEMP = BPARM(1) + 0.5
         IF (ITEMP.LE.0) ITEMP = 1
         IF ((JLOCIF.GE.0) .AND. (ITEMP.GT.CATBLK(KINAX+JLOCIF)))
     *      ITEMP = 1
         PARM(1) = ITEMP
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2100) ITEMP
         GO TO 195
C                                       OPCODE='CPSN'(13)
C                                       Copy whole soultion from one IF
C                                       to all others.
  105    ITEMP = BPARM(1) + 0.5
         IF (ITEMP.LE.0) ITEMP = 1
         IF ((JLOCIF.GE.0) .AND. (ITEMP.GT.CATBLK(KINAX+JLOCIF)))
     *      ITEMP = 1
         PARM(1) = ITEMP
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2105) ITEMP
         GO TO 195
C                                       OPCODE='PCOP'(14)
C                                       Copy soln from one
C                                       polzn to the other.
 110     CONTINUE
         ITEMP = IROUND (BPARM(1))
         PARM(1) = 1
         IF (ITEMP.EQ.2) PARM(1) = 2
         PARM(2) = 3 - PARM(1)
         ITEMP = IROUND (PARM(1))
         JTEMP = IROUND (PARM(2))
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2110) PARM(1), ITEMP, JTEMP
         GO TO 195
C                                       OPCODE='PNEG'(15)
C                                       Change sign of gain
C                                       phase
 115     CONTINUE
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2115)
         GO TO 195
C                                       OPCODE='CLPW' (16)
C                                       Clip by weight
  120    CONTINUE
         PARM(1) = MIN (BPARM(1), BPARM(2))
         PARM(2) = MAX (BPARM(1), BPARM(2))
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2120) PARM(1), PARM(2)
         GO TO 195
C                                       OPCODE='NORM' (17)
C                                       Normalize amplitude to 1
  125    CONTINUE
C                                       History
         NUMHIS = NUMHIS + 1
         WRITE (HISCRD(NUMHIS),2130)
         GO TO 195
C
 195     CONTINUE
         GO TO 999
C                                       Process record
 200  FIXCNT = FIXCNT + 1
      IANT = SNRECI(ANTSN)
      GO TO (250,260,270,275,280,290,300,310,320,330,340,350,360,
     *   370,380,390,400), ICODE
C                                       'AVRT'(1)
C                                       Average fringe rate.
 250     CALL AVGRAT (IERR)
         GO TO 999
C                                       OPCODE='CLPA'(2)
C                                       Clip Amplitudes
 260     CALL CLPA (IERR)
         GO TO 999
C                                       OPCODE='CLPP'(3)
C                                       Clip phases
 270     CALL CLPP (IERR)
         GO TO 999
C                                       OPCODE='XFER'(4)
C                                       Transfer phases
 275     CALL XFER(IERR)
         GO TO 999
C                                       OPCODE='ZPHS'(5)
C                                       Zero phases.
 280     CALL ZPHS (IERR)
         GO TO 999
C                                       OPCODE='ZRAT'(6)
C                                       Zero rates.
 290     CALL ZRAT (IERR)
         GO TO 999
C                                       OPCODE='MULA'(7)
C                                       Multiply amplitudes
 300     CALL MULA (IERR)
         GO TO 999
C                                       OPCODE='CLPD'(8)
C                                       Clip delays
 310     CALL CLPD (IERR)
         GO TO 999
C                                       OPCODE='CLPR'(9)
C                                       Clip rates
 320     CALL CLPR (IERR)
         GO TO 999
C                                       OPCODE='REFP'(10)
C                                       Reference phases to one IF.
 330     CALL REFP (IERR)
         GO TO 999
C                                       OPCODE='ZDEL'(11)
C                                       Zero delays.
 340     CALL ZDEL (IERR)
         GO TO 999
C                                       OPCODE='CPRT'(12)
C                                       Copy rates.
 350     CALL CPRT (IERR)
         GO TO 999
C                                       OPCODE='CPRT'(13)
C                                       Copy solution.
 360     CALL CPSN (IERR)
         GO TO 999
C                                       OPCODE='PCOP'(14)
C                                       Copy soln. from one polzn.
C                                       to the other
 370     CALL CPOLZN (IERR)
         GO TO 999
C                                       OPCODE='PNEG'(15)
C                                       Change sign of gain phase
 380     CALL PHSNEG (IERR)
         GO TO 999
C                                       OPCODE='CLPW' (16)
C                                       Clip by weight
  390    CALL CLPW (IERR)
         GO TO 999
C                                       OPCODE='NORM' (17)
C                                       Normalize amplitude to 1
  400    CALL NORM (IERR)
         GO TO 999
C                                       Finish - number changed.
 900  NUMHIS= NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2900) FIXCNT
      WRITE (MSGTXT,2901) FIXCNT
      CALL MSGWRT (6)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('ERROR: UNKNOWN OPCODE: ',A4)
 1040 FORMAT ('ERROR: UNKNOWN CORRECTION MODE: ',F3.0)
 2040 FORMAT ('OPCODE = ''',A4,''' / Operation code')
 2050 FORMAT ('SNCORPRM =',2F10.5,' / Valid range of ampl')
 2055 FORMAT ('SNCORPRM =',2F10.5,' / Valid range of phase')
 2060 FORMAT ('SNCORPRM =',F10.5,' / Multiplication factor')
 2075 FORMAT ('SNCORPRM =',2F10.5,' / Valid range of delay')
 2080 FORMAT ('SNCORPRM =',2F10.5,' / Valid range of rate')
 2085 FORMAT ('SNCORPRM =',I5,' / Reference IF for phases')
 2090 FORMAT ('SNCORPRM = ',3(F10.5,1X))
 2095 FORMAT ('PHASPRM = ',3(F10.5,1X))
 2100 FORMAT ('SNCORPRM =',I5,' / Reference IF for copying rates')
 2105 FORMAT ('SNCORPRM =',I5,' / Reference IF for copying solution')
 2110 FORMAT ('SNCORPRM =',F10.1,' / Copy polzn ',I2,' to polzn ',I2)
 2115 FORMAT ('SNCORPRM ignored / Change sign of gain phase')
 2120 FORMAT ('SNCORPRM =',2F10.5,' / Valid range of weight')
 2130 FORMAT ('Amplitudes are normolized to 1')
 2900 FORMAT (' / ',I6,' Records modified')
 2901 FORMAT (I6,' Records modified')
      END
      SUBROUTINE SNCLHI
C-----------------------------------------------------------------------
C   SNCLHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, CTIME(2)*12, LABEL*8
      INTEGER   LUN1, IERR, I, TIME(3), DATE(3), LIMIT, LIMIT2, J
      REAL      TIMBEG, TIMEND
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1 /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HIOPEN (LUN1, DISKIN, FCNO(NCFILE), BUFFER, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
C                                       Task message
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Sources
         IF (NSOUWD.LE.0) THEN
            WRITE (HILINE,3000) TSKNAM
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         ELSE
C                                       Included or excluded?
            WRITE (HILINE,3001) TSKNAM
            IF (DOSWNT) WRITE (HILINE,3002) TSKNAM
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
C                                       1st 2 and label.
            WRITE (HILINE,3003) TSKNAM, XSOUR(1), XSOUR(2)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            IF (NSOUWD.LE.2) GO TO 25
C                                       Rest of sources
            DO 20 I = 1,NSOUWD,2
               WRITE (HILINE,3004) TSKNAM, XSOUR(I), XSOUR(I+1)
               CALL HIADD (LUN1, HILINE, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 100
 20            CONTINUE
            END IF
C                                       Antennas
 25      IF (NANTSL.LE.0) THEN
            WRITE (HILINE,3005) TSKNAM
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         ELSE
C                                       Included or excluded?
            WRITE (HILINE,3006) TSKNAM
            IF (DOAWNT) WRITE (HILINE,3007) TSKNAM
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
C                                       1st 12 and label.
            LIMIT = MIN (12, NANTSL)
            WRITE (HILINE,3008) TSKNAM, (ANTENS(J),J=1,LIMIT)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            IF (NANTSL.LE.12) GO TO 35
C                                       Rest of antennas
            DO 30 I = 13,NANTSL,12
               LIMIT = I
               LIMIT2 = I + 11
               LIMIT2 = MIN (NANTSL, LIMIT2)
               WRITE (HILINE,3009) TSKNAM, (ANTENS(J),J=LIMIT,LIMIT2)
               CALL HIADD (LUN1, HILINE, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 100
 30            CONTINUE
            END IF
C                                       Timerange
 35   TIMBEG = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   (XTIME(4) / (24.0*60.0*60.0))
      TIMEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   (XTIME(8) / (24.0*60.0*60.0))
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMEND.LT.1.0E-5)) TIMEND = 1.0E20
      CALL HITIME (TIMBEG, TIMEND, LUN1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Stokes'
      WRITE (HILINE,2005) TSKNAM, XSTOK
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       IF range
      WRITE (HILINE,2004) TSKNAM, BIF, EIF
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       SUBARRAY, SNVER
      WRITE (HILINE,2002) TSKNAM, SUBA, SNVER
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                      Add any other history.
         IF (NUMHIS.LE.0) GO TO 100
         WRITE (LABEL,1011) TSKNAM
         HILINE(1:8) = LABEL(1:8)
         DO 90 I = 1,NUMHIS
            HILINE(9:72) = HISCRD(I)(1:64)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
 90         CONTINUE
C                                       Close HI file
 100  CALL HICLOS (LUN1, T, BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SNCLHI: ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 1011 FORMAT (A6,'  ')
 2002 FORMAT (A6, ' SUBARRAY =',I3,' GAINVER = ',I4,' /SN table')
 2004 FORMAT (A6,' BIF =',I4,', EIF =',I4,'/ IF range')
 2005 FORMAT (A6,' STOKES = ''',A4,''' / Stokes type')
 3000 FORMAT (A6,' SOURCES = ''''     /All sources selected')
 3001 FORMAT (A6,' /Sources excluded:')
 3002 FORMAT (A6,' /Sources included:')
 3003 FORMAT (A6,' SOURCES = ''',A16,''',''',A16,'''')
 3004 FORMAT (A6,'          ,''',A16,''',''',A16,'''')
 3005 FORMAT (A6,' ANTENNAS = 0     /All antennas selected')
 3006 FORMAT (A6,' /Antennas excluded:')
 3007 FORMAT (A6,' /Antennas included:')
 3008 FORMAT (A6,' ANTENNAS = ',12(I3,' '))
 3009 FORMAT (A6,'            ',12(I3,' '))
      END
      SUBROUTINE AVGRAT (IERR)
C-----------------------------------------------------------------------
C   Routine to average selected fringe rates and replace the values in
C   the SN record in common with the average.
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I, COUNT
      REAL      SUMRAT, RATAVG
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Average rate:
      COUNT = 0
      SUMRAT = 0.0
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         DO 100 I = BIF,EIF
            IF (SNRECR(RA1SN+I-1).NE.FBLANK) THEN
               COUNT = COUNT + 1
               SUMRAT = SUMRAT + SNRECR(RA1SN+I-1)
               END IF
 100        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 200 I = BIF,EIF
            IF (SNRECR(RA2SN+I-1).NE.FBLANK) THEN
               COUNT = COUNT + 1
               SUMRAT = SUMRAT + SNRECR(RA2SN+I-1)
               END IF
 200        CONTINUE
         END IF
      IF (COUNT.LE.0) GO TO 999
C                                       Average and correct:
      RATAVG = SUMRAT / COUNT
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         DO 500 I = BIF,EIF
            IF (SNRECR(RA1SN+I-1).NE.FBLANK) SNRECR(RA1SN+I-1) = RATAVG
 500        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 600 I = BIF,EIF
            IF (SNRECR(RA2SN+I-1).NE.FBLANK) SNRECR(RA2SN+I-1) = RATAVG
 600        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE CLPA (IERR)
C-----------------------------------------------------------------------
C   Routine to flag gains with amplitudes outside of the range given by
C   PARM(1) - PARM(2).
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I
      REAL      AMPL
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         DO 500 I = BIF,EIF
            IF (SNRECR(RE1SN+I-1).NE.FBLANK) THEN
               AMPL = SQRT ((SNRECR(RE1SN+I-1)**2) +
     *            (SNRECR(IM1SN+I-1)**2))
               IF ((AMPL.LT.PARM(1)).OR.(AMPL.GT.PARM(2))) THEN
                  SNRECR(RE1SN+I-1) = FBLANK
                  SNRECR(IM1SN+I-1) = FBLANK
                  END IF
               END IF
 500        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 600 I = BIF,EIF
            IF (SNRECR(RE2SN+I-1).NE.FBLANK) THEN
               AMPL = SQRT ((SNRECR(RE2SN+I-1)**2) +
     *            (SNRECR(IM2SN+I-1)**2))
               IF ((AMPL.LT.PARM(1)).OR.(AMPL.GT.PARM(2))) THEN
                  SNRECR(RE2SN+I-1) = FBLANK
                  SNRECR(IM2SN+I-1) = FBLANK
                  END IF
               END IF
 600        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE XFER (IERR)
C-----------------------------------------------------------------------
C   Routine to transfer phases between two frequencies and to scale
C   them for ionospheric delays.  Instrumental phase offsets btween
C   the two frequencies can be adjusted and antenna amplitudes of for
C   output frequency can be set.
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C   XFER Requires 60 Parameters:
C   External to Program:  CLCORPRM(30),PHASPRM(30)
C   Internal to Program these parameters become: BPARM(30) and XPARM(30)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I
      REAL      RATO, AMPLI
      DOUBLE PRECISION XPTLST(28), YPTLST(28), PHAS
      SAVE XPTLST, YPTLST
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA XPTLST /28 * 0.0D0/, YPTLST /28 * 0.0D0/
C-----------------------------------------------------------------------
      IERR = 0
      RATO=XPARM(29)/XPARM(30)
      I = BPARM(30) + 2
C
      GO TO (401,402,403) I
C
 401  AMPLI = BPARM(KANT)
      GO TO 450
C
 402  AMPLI = -1.0
      GO TO 450
C
 403  AMPLI = 1.0
C                                       First poln.
 450  IF (ISTOK.NE.2) THEN
         DO 500 I = BIF,EIF
            PHAS = ATAN2 (SNRECR(IM1SN+I-1),
     *         SNRECR(RE1SN+I-1)+1.0E-20)
 550        IF(ABS(PHAS-XPTLST(KANT)) .GE. PI) THEN
               PHAS=PHAS-DSIGN(TWOPI,PHAS-XPTLST(KANT))
               GO TO 550
               END IF
            XPTLST(KANT)=PHAS
            PHAS=RATO*PHAS + XPARM(KANT)
            IF(AMPLI.EQ.-1.0) AMPLI=SQRT(SNRECR(RE1SN+I-1)**2
     *         + SNRECR(IM1SN+I-1)**2)
            SNRECR(RE1SN+I-1) = AMPLI*COS(PHAS)
            SNRECR(IM1SN+I-1) = AMPLI*SIN(PHAS)
 500        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 600 I = BIF,EIF
            PHAS = ATAN2 (SNRECR(IM2SN+I-1),
     *         SNRECR(RE2SN+I-1)+1.0E-20)
 650        IF(ABS(PHAS-YPTLST(KANT)) .GE. PI) THEN
               PHAS=PHAS-DSIGN(TWOPI,PHAS-YPTLST(KANT))
               GO TO 650
               END IF
            YPTLST(KANT)=PHAS
            PHAS=RATO*PHAS + XPARM(KANT)
            IF(AMPLI.EQ.-1.0) AMPLI=SQRT(SNRECR(RE2SN+I-1)**2
     *         + SNRECR(IM2SN+I-1)**2)
            SNRECR(RE2SN+I-1) = AMPLI*COS(PHAS)
            SNRECR(IM2SN+I-1) = AMPLI*SIN(PHAS)
 600        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE CLPP (IERR)
C-----------------------------------------------------------------------
C   Routine to flag gains with phases outside of the range given by
C   PARM(1) - PARM(2).
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I
      REAL      PHAS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         DO 500 I = BIF,EIF
            IF (SNRECR(RE1SN+I-1).NE.FBLANK) THEN
               PHAS = ATAN2 (SNRECR(IM1SN+I-1),
     *            SNRECR(RE1SN+I-1)+1.0E-20)
               IF (PHAS.GT.PI) PHAS = PHAS - TWOPI
               IF (PHAS.LT.-PI) PHAS = PHAS + TWOPI
               IF ((PHAS.LT.PARM(1)).OR.(PHAS.GT.PARM(2))) THEN
                  SNRECR(RE1SN+I-1) = FBLANK
                  SNRECR(IM1SN+I-1) = FBLANK
                  END IF
               END IF
 500        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 600 I = BIF,EIF
            IF (SNRECR(RE2SN+I-1).NE.FBLANK) THEN
               PHAS = ATAN2 (SNRECR(IM2SN+I-1),
     *            SNRECR(RE2SN+I-1)+1.0E-20)
               IF (PHAS.GT.PI) PHAS = PHAS - TWOPI
               IF (PHAS.LT.-PI) PHAS = PHAS + TWOPI
               IF ((PHAS.LT.PARM(1)).OR.(PHAS.GT.PARM(2))) THEN
                  SNRECR(RE2SN+I-1) = FBLANK
                  SNRECR(IM2SN+I-1) = FBLANK
                  END IF
               END IF
 600        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE ZPHS (IERR)
C-----------------------------------------------------------------------
C   Sets gain phases to zero.
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I
      REAL      AMPL
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         DO 500 I = BIF,EIF
            IF (SNRECR(RE1SN+I-1).NE.FBLANK) THEN
               AMPL = SQRT ((SNRECR(RE1SN+I-1)**2) +
     *            (SNRECR(IM1SN+I-1)**2))
               SNRECR(RE1SN+I-1) = AMPL
               SNRECR(IM1SN+I-1) = 0.0
               END IF
 500        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 600 I = BIF,EIF
            IF (SNRECR(RE2SN+I-1).NE.FBLANK) THEN
               AMPL = SQRT ((SNRECR(RE2SN+I-1)**2) +
     *            (SNRECR(IM2SN+I-1)**2))
               SNRECR(RE2SN+I-1) = AMPL
               SNRECR(IM2SN+I-1) = 0.0
               END IF
 600        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE ZRAT (IERR)
C-----------------------------------------------------------------------
C   Sets residual fringe rates to zero.
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         DO 500 I = BIF,EIF
            IF (SNRECR(RA1SN+I-1).NE.FBLANK) THEN
               SNRECR(RA1SN+I-1) = 0.0
               END IF
 500        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 600 I = BIF,EIF
            IF (SNRECR(RA2SN+I-1).NE.FBLANK) THEN
               SNRECR(RA2SN+I-1) = 0.0
               END IF
 600        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE ZDEL (IERR)
C-----------------------------------------------------------------------
C   Sets residual delays to zero.
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         DO 500 I = BIF,EIF
            IF (SNRECR(DL1SN+I-1).NE.FBLANK) THEN
               SNRECR(DL1SN+I-1) = 0.0
               END IF
 500        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 600 I = BIF,EIF
            IF (SNRECR(DL2SN+I-1).NE.FBLANK) THEN
               SNRECR(DL2SN+I-1) = 0.0
               END IF
 600        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE MULA (IERR)
C-----------------------------------------------------------------------
C   Routine to multiply selected amplitudes by PARM(1)
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         DO 500 I = BIF,EIF
            IF (SNRECR(RE1SN+I-1).NE.FBLANK) THEN
               SNRECR(RE1SN+I-1) = SNRECR(RE1SN+I-1) * PARM(1)
               SNRECR(IM1SN+I-1) = SNRECR(IM1SN+I-1) * PARM(1)
               END IF
 500        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 600 I = BIF,EIF
            IF (SNRECR(RE2SN+I-1).NE.FBLANK) THEN
               SNRECR(RE2SN+I-1) = SNRECR(RE2SN+I-1) * PARM(1)
               SNRECR(IM2SN+I-1) = SNRECR(IM2SN+I-1) * PARM(1)
               END IF
 600        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE NORM (IERR)
C-----------------------------------------------------------------------
C   Routine to normalize amplitudes to 1
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I
      REAL  RE, IM, AMP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         DO 500 I = BIF,EIF
            IF (SNRECR(RE1SN+I-1).NE.FBLANK) THEN
               RE = SNRECR(RE1SN+I-1)
               IM = SNRECR(IM1SN+I-1)
               AMP = SQRT(RE*RE + IM*IM)
               IF (AMP .NE. 0) THEN
                  SNRECR(RE1SN+I-1) = SNRECR(RE1SN+I-1) / AMP
                  SNRECR(IM1SN+I-1) = SNRECR(IM1SN+I-1) / AMP
                  END IF
               END IF
 500        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 600 I = BIF,EIF
            IF (SNRECR(RE2SN+I-1).NE.FBLANK) THEN
               RE = SNRECR(RE2SN+I-1)
               IM = SNRECR(IM2SN+I-1)
               AMP = SQRT(RE*RE + IM*IM)
               IF (AMP .NE. 0) THEN
                  SNRECR(RE2SN+I-1) = SNRECR(RE2SN+I-1) / AMP
                  SNRECR(IM2SN+I-1) = SNRECR(IM2SN+I-1) / AMP
                  END IF
               END IF
 600        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE CLPD (IERR)
C-----------------------------------------------------------------------
C   Routine to flag gains with delays outside of the range given by
C   PARM(1) - PARM(2) in Seconds.
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I
      REAL      DELAY
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         DO 500 I = BIF,EIF
            DELAY = SNRECR(DL1SN+I-1)
            IF ((DELAY.NE.FBLANK) .AND.
     *         ((DELAY.LT.PARM(1)).OR.(DELAY.GT.PARM(2)))) THEN
               SNRECR(DL1SN+I-1) = FBLANK
               SNRECR(RE1SN+I-1) = FBLANK
               SNRECR(IM1SN+I-1) = FBLANK
               END IF
 500        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 600 I = BIF,EIF
            DELAY = SNRECR(DL2SN+I-1)
            IF ((DELAY.NE.FBLANK) .AND.
     *         ((DELAY.LT.PARM(1)).OR.(DELAY.GT.PARM(2)))) THEN
               SNRECR(DL2SN+I-1) = FBLANK
               SNRECR(RE2SN+I-1) = FBLANK
               SNRECR(IM2SN+I-1) = FBLANK
               END IF
 600        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE CLPR (IERR)
C-----------------------------------------------------------------------
C   Routine to flag gains with rates outside of the range given by
C   PARM(1) - PARM(2) in Seconds/second.
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I
      REAL      RATE
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         DO 500 I = BIF,EIF
            RATE = SNRECR(RA1SN+I-1)
            IF ((RATE.NE.FBLANK) .AND.
     *         ((RATE.LT.PARM(1)).OR.(RATE.GT.PARM(2)))) THEN
               SNRECR(RA1SN+I-1) = FBLANK
               SNRECR(RE1SN+I-1) = FBLANK
               SNRECR(IM1SN+I-1) = FBLANK
               END IF
 500        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 600 I = BIF,EIF
            RATE = SNRECR(RA2SN+I-1)
            IF ((RATE.NE.FBLANK) .AND.
     *         ((RATE.LT.PARM(1)).OR.(RATE.GT.PARM(2)))) THEN
               SNRECR(RA2SN+I-1) = FBLANK
               SNRECR(RE2SN+I-1) = FBLANK
               SNRECR(IM2SN+I-1) = FBLANK
               END IF
 600        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE REFP (IERR)
C-----------------------------------------------------------------------
C   References phases to IF number PARM(1)
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I, IFNO
      REAL      TRE, TIM, ROTRE, ROTIM, AMPL
      LOGICAL   BADPHS
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IFNO = PARM(1) + 0.5
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         BADPHS = (SNRECR(RE1SN+IFNO-1).EQ.FBLANK) .OR.
     *      (SNRECR(IM1SN+IFNO-1).EQ.FBLANK)

         IF (BADPHS) THEN
C                                       Blank
            DO 200 I = BIF,EIF
               SNRECR(RE1SN+I-1) = FBLANK
               SNRECR(IM1SN+I-1) = FBLANK
 200           CONTINUE
         ELSE
C                                       Refererence phase
            ROTRE = SNRECR(RE1SN+IFNO-1)
            ROTIM = -SNRECR(IM1SN+IFNO-1)
C                                       Normalize
            AMPL = ROTRE*ROTRE + ROTIM*ROTIM
            IF (AMPL.GT.1.0E-30) THEN
               AMPL = SQRT (AMPL)
            ELSE
               AMPL = 1.0
               END IF
            ROTRE = ROTRE / AMPL
            ROTIM = ROTIM / AMPL
            DO 300 I = BIF,EIF
               IF (SNRECR(RE1SN+I-1).NE.FBLANK) THEN
                  TRE = SNRECR(RE1SN+I-1)
                  TIM = SNRECR(IM1SN+I-1)
                  SNRECR(RE1SN+I-1) = TRE*ROTRE - TIM*ROTIM
                  SNRECR(IM1SN+I-1) = TRE*ROTIM + TIM*ROTRE
                  END IF
 300           CONTINUE
            END IF
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         BADPHS = (SNRECR(RE2SN+IFNO-1).EQ.FBLANK) .OR.
     *      (SNRECR(IM2SN+IFNO-1).EQ.FBLANK)

         IF (BADPHS) THEN
C                                       Blank
            DO 400 I = BIF,EIF
               SNRECR(RE2SN+I-1) = FBLANK
               SNRECR(IM2SN+I-1) = FBLANK
 400           CONTINUE
         ELSE
C                                       Refererence phase
            ROTRE = SNRECR(RE2SN+IFNO-1)
            ROTIM = -SNRECR(IM2SN+IFNO-1)
C                                       Normalize
            AMPL = ROTRE*ROTRE + ROTIM*ROTIM
            IF (AMPL.GT.1.0E-30) THEN
               AMPL = SQRT (AMPL)
            ELSE
               AMPL = 1.0
               END IF
            ROTRE = ROTRE / AMPL
            ROTIM = ROTIM / AMPL
            DO 500 I = BIF,EIF
               IF (SNRECR(RE2SN+I-1).NE.FBLANK) THEN
                  TRE = SNRECR(RE2SN+I-1)
                  TIM = SNRECR(IM2SN+I-1)
                  SNRECR(RE2SN+I-1) = TRE*ROTRE - TIM*ROTIM
                  SNRECR(IM2SN+I-1) = TRE*ROTIM + TIM*ROTRE
                  END IF
 500           CONTINUE
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE CPRT (IERR)
C-----------------------------------------------------------------------
C   Copy rates from IF PARM(1) to other specified IFs
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I, IFNO
      REAL      REFRAT
      LOGICAL   BADRAT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IFNO = PARM(1) + 0.5
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         REFRAT = SNRECR(RA1SN+IFNO-1)
         BADRAT = (REFRAT.EQ.FBLANK)
C
         IF (BADRAT) THEN
C                                       Blank
            DO 200 I = BIF,EIF
               SNRECR(RA1SN+I-1) = FBLANK
  200          CONTINUE
            END IF
C
         IF (.NOT.BADRAT) THEN
C                                       Copy
            DO 300 I = BIF,EIF
               SNRECR(RA1SN+I-1) = REFRAT
  300          CONTINUE
            END IF
         END IF
C
C                                       Second poln.
C
      IF (ISTOK.NE.1) THEN
         REFRAT = SNRECR(RA2SN+IFNO-1)
         BADRAT = (REFRAT.EQ.FBLANK)
C
         IF (BADRAT) THEN
C                                       Blank
            DO 400 I = BIF,EIF
               SNRECR(RA2SN+I-1) = FBLANK
  400          CONTINUE
            END IF
C
         IF (.NOT.BADRAT) THEN
C                                       Copy
            DO 600 I = BIF,EIF
               SNRECR(RA2SN+I-1) = REFRAT
  600          CONTINUE
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE CPSN (IERR)
C-----------------------------------------------------------------------
C   Copy whole solution (delay, rate, re, im, weight, refant) from IF
C   PARM(1) to other specified IFs
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I, IFNO, REFANT
      REAL      REFRAT, REFDEL, REFRE, REFIM, REFWT
      LOGICAL   BADSOL
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IFNO = PARM(1) + 0.5
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         REFRAT = SNRECR(RA1SN+IFNO-1)
         REFDEL = SNRECR(DL1SN+IFNO-1)
         REFRE  = SNRECR(RE1SN+IFNO-1)
         REFIM  = SNRECR(IM1SN+IFNO-1)
         REFWT  = SNRECR(WT1SN+IFNO-1)
         REFANT = SNRECI(RF1SN+IFNO-1)
         BADSOL = (REFRAT.EQ.FBLANK) .OR. (REFDEL.EQ.FBLANK) .OR.
     *      (REFRE.EQ.FBLANK) .OR. (REFIM.EQ.FBLANK)
C
         IF (BADSOL) THEN
C                                       Blank
            DO 200 I = BIF,EIF
               SNRECR(RA1SN+I-1) = FBLANK
               SNRECR(DL1SN+I-1) = FBLANK
               SNRECR(RE1SN+I-1) = FBLANK
               SNRECR(IM1SN+I-1) = FBLANK
  200          CONTINUE
            END IF
C
         IF (.NOT.BADSOL) THEN
C                                       Copy
            DO 300 I = BIF,EIF
               SNRECR(RA1SN+I-1) = REFRAT
               SNRECR(DL1SN+I-1) = REFDEL
               SNRECR(RE1SN+I-1) = REFRE
               SNRECR(IM1SN+I-1) = REFIM
               SNRECR(WT1SN+I-1) = REFWT
               SNRECI(RF1SN+I-1) = REFANT
  300          CONTINUE
            END IF
         END IF
C
C                                       Second poln.
C
      IF (ISTOK.NE.1) THEN
         REFRAT = SNRECR(RA2SN+IFNO-1)
         REFDEL = SNRECR(DL2SN+IFNO-1)
         REFRE  = SNRECR(RE2SN+IFNO-1)
         REFIM  = SNRECR(IM2SN+IFNO-1)
         REFWT  = SNRECR(WT2SN+IFNO-1)
         REFANT = SNRECI(RF2SN+IFNO-1)
         BADSOL = (REFRAT.EQ.FBLANK) .OR. (REFDEL.EQ.FBLANK) .OR.
     *      (REFRE.EQ.FBLANK) .OR. (REFIM.EQ.FBLANK)
C
         IF (BADSOL) THEN
C                                       Blank
            DO 400 I = BIF,EIF
               SNRECR(RA2SN+I-1) = FBLANK
               SNRECR(DL2SN+I-1) = FBLANK
               SNRECR(RE2SN+I-1) = FBLANK
               SNRECR(IM2SN+I-1) = FBLANK
  400          CONTINUE
            END IF
C
         IF (.NOT.BADSOL) THEN
C                                       Copy
            DO 600 I = BIF,EIF
               SNRECR(RA2SN+I-1) = REFRAT
               SNRECR(DL2SN+I-1) = REFDEL
               SNRECR(RE2SN+I-1) = REFRE
               SNRECR(IM2SN+I-1) = REFIM
               SNRECR(WT2SN+I-1) = REFWT
               SNRECI(RF2SN+I-1) = REFANT
  600          CONTINUE
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE CPOLZN (IERR)
C----------------------------------------------------------------------
C   Copy the solution from one polarization to the other. This
C   includes the gain, rate, delay, weight and reference antenna.
C   Input/output via common:
C      SNRECR   I    SN record to update (and pointers in /SNRECC/)
C      PARM     R(2) Copy from polzn PARM(1) to polzn PARM(2). [1,2]
C      BIF      I    First IF to process.
C      EIF      I    Highest IF to process.
C   Output:
C      IERR     I    Error code (always 0)
C----------------------------------------------------------------------
      INTEGER IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INTEGER I, ITEMP
      INTEGER IROUND
C----------------------------------------------------------------------
      IERR = 0
C                                       Do nothing if only one
C                                       polarization in data.
      IF (NUMPOL.GE.2) THEN
         ITEMP = IROUND (PARM(1))
C                                       Process all IF's.
         DO 100 I = BIF,EIF
C                                       Copy polzn 1 to polzn. 2
            IF (ITEMP.EQ.1) THEN
               SNRECR(RE2SN+I-1) = SNRECR(RE1SN+I-1)
               SNRECR(IM2SN+I-1) = SNRECR(IM1SN+I-1)
               SNRECR(RA2SN+I-1) = SNRECR(RA1SN+I-1)
               SNRECR(DL2SN+I-1) = SNRECR(DL1SN+I-1)
               SNRECR(WT2SN+I-1) = SNRECR(WT1SN+I-1)
               SNRECR(RF2SN+I-1) = SNRECR(RF1SN+I-1)
               ENDIF
C                                       Copy polzn 2 to polzn. 1
            IF (ITEMP.EQ.2) THEN
               SNRECR(RE1SN+I-1) = SNRECR(RE2SN+I-1)
               SNRECR(IM1SN+I-1) = SNRECR(IM2SN+I-1)
               SNRECR(RA1SN+I-1) = SNRECR(RA2SN+I-1)
               SNRECR(DL1SN+I-1) = SNRECR(DL2SN+I-1)
               SNRECR(WT1SN+I-1) = SNRECR(WT2SN+I-1)
               SNRECR(RF1SN+I-1) = SNRECR(RF2SN+I-1)
               END IF
 100        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE PHSNEG (IERR)
C-----------------------------------------------------------------------
C   Flip the sign of the gain phase (ignores SNCORPRM in this case)
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         DO 500 I = BIF,EIF
            IF ((SNRECR(RE1SN+I-1).NE.FBLANK).AND.
     *         (SNRECR(IM1SN+I-1).NE.FBLANK)) THEN
               SNRECR(IM1SN+I-1) = -SNRECR(IM1SN+I-1)
               END IF
 500        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 600 I = BIF,EIF
            IF ((SNRECR(RE2SN+I-1).NE.FBLANK).AND.
     *         (SNRECR(IM2SN+I-1).NE.FBLANK)) THEN
               SNRECR(IM2SN+I-1) = -SNRECR(IM2SN+I-1)
               END IF
 600        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE CLPW (IERR)
C-----------------------------------------------------------------------
C   Routine to flag gains with weights outside of the range given by
C   PARM(1) - PARM(2).
C   Input: (in common)
C      ISTOK    I    Polarization to correct, 1=RCP, 2=LCP, 0 = both
C      BIF      I    First IF to process
C      EIF      I    Highest IF to process
C   Output:
C      IERR     I    Error code (always 0)
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   I
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       First poln.
      IF (ISTOK.NE.2) THEN
         DO 500 I = BIF,EIF
            IF (SNRECR(WT1SN+I-1).NE.FBLANK) THEN
               IF ((SNRECR(WT1SN+I-1).LT.PARM(1))
     *              .OR. (SNRECR(WT1SN+I-1).GT.PARM(2))) THEN
                  SNRECR(RE1SN+I-1) = FBLANK
                  SNRECR(IM1SN+I-1) = FBLANK
                  SNRECR(DL1SN+I-1) = FBLANK
                  SNRECR(RA1SN+I-1) = FBLANK
                  END IF
               END IF
 500        CONTINUE
         END IF
C                                       Second poln.
      IF (ISTOK.NE.1) THEN
         DO 600 I = BIF,EIF
            IF (SNRECR(WT2SN+I-1).NE.FBLANK) THEN
               IF ((SNRECR(WT2SN+I-1).LT.PARM(1))
     *             .OR. (SNRECR(WT2SN+I-1).GT.PARM(2))) THEN
                  SNRECR(RE2SN+I-1) = FBLANK
                  SNRECR(IM2SN+I-1) = FBLANK
                  SNRECR(DL2SN+I-1) = FBLANK
                  SNRECR(RA2SN+I-1) = FBLANK
                  END IF
               END IF
 600        CONTINUE
         END IF
C
 999  RETURN
      END

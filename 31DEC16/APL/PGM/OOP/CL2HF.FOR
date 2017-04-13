LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
C                                       NPARMS=no. adverbs passed.
      PARAMETER (NPARMS=14)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
C
LOCAL INCLUDE 'INPUTDATA.INC'
C                                     DATA statments defining input parameters.
C                                       NOTE: Uses values in PAOOF.INC
C                                       Adverb names
C                      1         2          3        4         5
      DATA AVNAME / 'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
C          6           7           8           9           10
     *  'IN2VERS ', 'SNVER   ', 'SUBARRAY', 'OUTVERS ', 'TIMERANG',
C          11         12        13       14
     *  'BADDISK', 'INFILE', 'APARM', 'DPARM' /
C                                       Adverb data types (PAOOF.INC)
C                    1       2       3       4       5
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
C          6       7       8       9       10
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOARE ,
C          11      12      13     14
     *   OOAINT, OOACAR, OOARE, OOARE/
C                                       Adverb dimensions (as 2D)
C                   1     2     3     4     5
      DATA AVDIM /12,1,  6,1,  1,1,  1,1,  1,1,
C          6     7     8     9     10
     *    1,1,  1,1,  1,1,  1,1,  8,1,
C          11    12    13    14
     *   10,1, 48,1, 10,1, 10,1/
LOCAL END
C
LOCAL INCLUDE 'TIMETAGS.INC'
C        HTIME(2000)  - Array of allowed time tags in fractions of a day.
C        KYEAR        - Year at start of experiment - 1900.
C        KMONTH       - Month at start of experiment (1 - 12).
C        KDAY         - Day of month at start of experiment.
C        ITAG(2000,3) - Array of allowed time tags in hour, minute, second.
C        SRC(2000)    - Sources observed at the corresponding time tag.
C        NHTIME       - Number of valid entries in HTIME, ITAG, and SRC arrays.
      CHARACTER*8 SRC(2000)
      INTEGER ITAG(2000,3), KYEAR, KMONTH, KDAY
      INTEGER NHTIME
      DOUBLE PRECISION HTIME(2000)
C
      COMMON /TMTGSD/ HTIME
      COMMON /TMTGSI/ ITAG, KYEAR, KMONTH, KDAY, NHTIME
      COMMON /TMTGSC/ SRC
LOCAL END
C
LOCAL INCLUDE 'ANSUFQINFO'
      CHARACTER ANAME(MXGANT)*8, OBSDAT*8
      DOUBLE PRECISION REFREQ
      REAL DATUTC
      INTEGER SUBAR
      COMMON /ANINC/ ANAME, OBSDAT
      COMMON /ANIND/ REFREQ
      COMMON /ANINR/ DATUTC
      COMMON /ANINIC/ SUBAR
C                                       SUMAX = max number of source names
C                                               that can be stored
      INTEGER SUMAX
      PARAMETER (SUMAX=2000)
      CHARACTER*16 SUNAME(SUMAX)
      COMMON /SUINFC/ SUNAME
C                                       MAXIF = MAX NUMBER OF IFs
      DOUBLE PRECISION FOFF(MAXIF)
      INTEGER NUMIF, FREQNO
      REAL TBW(MAXIF)
      COMMON /FQINFD/ FOFF
      COMMON /FQINFI/ NUMIF, FREQNO
      COMMON /FQINFR/ TBW
LOCAL END
C
LOCAL INCLUDE 'CLINFO.INC'
      INTEGER NSNSIZ, MXGANT
      PARAMETER (NSNSIZ=1000)
      PARAMETER (MXGANT=20)
C                                       SN table time, 2-minute window,
C                                        and time tag
      DOUBLE PRECISION TSN(NSNSIZ,5)
C                                       SN, CL subarray #'s; Source #
      INTEGER ISUB1(NSNSIZ,2), ISRC1(NSNSIZ)
C                                       Time tag in Y, M, D, H, M, S
      INTEGER IC02(NSNSIZ,6)
C                                       Total number SN scans
      INTEGER IKOUNT
C                                       SN, CL, PC record #'s vs Ant. #
      INTEGER IRECS1(NSNSIZ,MXGANT,3)
      COMMON /XSNCL/ TSN
      COMMON /KSNCL/ ISUB1, ISRC1, IRECS1, IC02, IKOUNT
      INTEGER JREC(MXGANT,3), HFANTS(MXGANT), IANTS
      COMMON /MREC/ JREC, HFANTS, IANTS
LOCAL END
C
      PROGRAM CL2HF
C-----------------------------------------------------------------------
C! Convert CL table to HF table
C# Task VLBI Calibration Utility OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999, 2001-2003, 2005, 2009, 2012-2013
C;  Copyright (C) 2015
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
C   Convert CL table to HF table
C   28-Feb-2004 version of GSFC/Fomalont changes
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, POPS*36, MOTAB*36, PCTAB*36, SNTAB*36,
     *   UVDATA*36, HFTAB*36
      INTEGER  IRET, BUFF1(256)
      DATA PRGM   /'CL2HF '/
      DATA POPS   /'Input POPS adverbs'/
      DATA UVDATA /'Input UV data'/
      DATA MOTAB  /'Input CL table'/
      DATA PCTAB  /'Input PC table'/
      DATA SNTAB  /'Input SN table'/
      DATA HFTAB /'Output HF table'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL CL2HIN (PRGM, POPS,
     *     MOTAB, PCTAB, SNTAB, UVDATA, HFTAB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Process table
      CALL CLTOHF (MOTAB, PCTAB, SNTAB, UVDATA, HFTAB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       History
      CALL CL2HHI (POPS, HFTAB)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
C
      SUBROUTINE CL2HIN (PRGN, POPS, MOTAB, PCTAB, SNTAB, UVDATA,
     *   HFTAB, IRET)
C-----------------------------------------------------------------------
C   CL2HIN gets input parameters for CL2HF and creates the input and
C   output objects
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      MOTAB   C*   Name of input table object.
C      PCTAB   C*   Name of input phase cal table object.
C      SNTAB   C*   Name of input SN table
C      UVDATA  C*   Name of input uv data
C      HFTAB  C*   Name of output table object.
C      IRET    I    Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS file
C
C   Inputs from MOTAB:
C     APARM  R(10) --> Control parameters
C           APARM(1) - Parameter to control determination of the UTC
C                      Time Tag.
C                      = 0, (default), use CL table time rounded to
C                           nearest second.
C                      = 1, use SN table time rounded to nearest second.
C                      = 2, use input file of time tags in the form
C                           year, month, day, hour, minute, second
C                           (6(I2,1X)). Must specify input file name as
C                           'INFILE' adverb.
C                      = 3, use an input Mark III schedule file and
C                           compute time tags ala Haystack convention.
C                           Must specify input schedule file as 'INFILE'
C                           adverb.
C                      = 4, use timerange listing from Task 'LISTR'.
C           APARM(2) - Elevation angle cutoff, in degrees.
C
C     DPARM  R(10) -->
C           DPARM(1)/DPARM(2) - Parameters to correct SNR if channels
C                      were dropped in FRING'ing. Enter 0, 0 if no
C                      channels were dropped. DPARM(1) is the number
C                      of channels used by FRING. DPARM(2) is the total
C                      number of channels. Use 14 16, for example, if
C                      the end channels were dropped.
C                      Defaults: DPARM(2) = 16, DPARM(1) = DPARM(2)
C
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, POPS*(*), MOTAB*(*), PCTAB*(*), SNTAB*(*),
     *   UVDATA*(*), HFTAB*(*), LFILE*48
      CHARACTER FNAME*256
      REAL      APARM(10), DPARM(10), TR(8)
      INTEGER TI(8), I
      DOUBLE PRECISION TSTART, TSTOP
      COMMON /APRM/ APARM, DPARM
      CHARACTER TBCHAR*2
      INTEGER   DIM(3), TYPE, TBVER, FDUM(16)

      LOGICAL LDUMMY
C
      INTEGER  LUNIT, SUBNO
C
      INTEGER  NKEY3
C                                       NKEY3=# POPS adverbs to copy to HFTAB
      PARAMETER (NKEY3=6)
      CHARACTER INK3(NKEY3)*8, OUTK3(NKEY3)*32, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'CLINFO.INC'
      INCLUDE 'ANSUFQINFO'
      INCLUDE 'INPUTDATA.INC'
C
C                                       Copy from POPS object to UVDATA object
      DATA INK3  /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INVERS',
     *   'SUBARRAY'/
      DATA OUTK3 /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'CLUSE',
     *   'SUBARR'/
C-----------------------------------------------------------------------
C                                       Startup, returns "Input" object
CC                                       containing POPS adverbs
      CALL AV2INP (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, POPS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       version messages _must_ come
C                                       after initial call to get POPS
C                                       adverbs
      MSGTXT = 'Version history:'
      CALL MSGWRT(4)
      MSGTXT = '19980811: GSFC version merged into system version'
      CALL MSGWRT(4)
      MSGTXT = '19980814: Correctly processes 4 digit dates'
      CALL MSGWRT(4)
      MSGTXT = '19990131: Added subarray adverb + bug fixes'
      CALL MSGWRT(4)
      MSGTXT = '19991005: Different CL times + many mods'
      CALL MSGWRT(4)
C                                       Set BADDISK to avoid disk i/o problems
      CALL OGET (POPS, 'BADDISK', TYPE, DIM, IBAD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Process APARM array
      CALL OGET (POPS, 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (APARM(1) .LE. 0.) APARM(1) = 0.
      IF (APARM(1) .GE. 5.) APARM(1) = 0.
      IF (APARM(2) .LE. 1.) APARM(2) = 1.
C                                       Accum period integration time
      IF (APARM(3).LE.0.) APARM(3) = 4.0D0
      APARM(3) = APARM(3) * 0.983D0
      CALL OPUT (POPS, 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
C                                       Process DPARM array
      CALL OGET (POPS, 'DPARM', TYPE, DIM, DPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (DPARM(2) .LE. 0.) DPARM(2) = 16.
      IF (DPARM(1) .LE. 0.) DPARM(1) = DPARM(2)
C       print *, ' Dparm1/Dparm2 ', DPARM(1)/DPARM(2)
      IF (DPARM(1) .GT. DPARM(2)) THEN
        WRITE(6,165)
        WRITE (MSGTXT,165)
        CALL MSGWRT(4)
        GO TO 999
 165    FORMAT (/,' DRARM(1) must not be greater than DPARM(2)!!!')
      ENDIF
      CALL OPUT (POPS, 'DPARM', TYPE, DIM, DPARM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
C                                       Create Input UV data object
      CALL CREATE (UVDATA, 'UVDATA', IRET)
C                                       Copy adverbs to object
      CALL IN2OBJ (POPS, NKEY3, INK3, OUTK3, UVDATA, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Require calibration to be on
      CALL OGET (UVDATA, 'DOCAL', TYPE, DIM, LDUMMY, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      LDUMMY = .TRUE.
      CALL OPUT (UVDATA, 'DOCAL', TYPE, DIM, LDUMMY, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
C                                       prep Model table object
      CALL OGET (POPS, 'INVERS', TYPE, DIM, TBVER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      TBCHAR = 'CL'
      CALL UV2TAB (UVDATA, MOTAB, TBCHAR, TBVER, IRET)
      IF (IRET.NE.0) GO TO 999
      WRITE (MSGTXT,1019) 'Resids & model from table ', TBCHAR, TBVER
      CALL MSGWRT(4)
C
C                                       prep Phase Cal table object
      CALL OGET (POPS, 'IN2VERS', TYPE, DIM, TBVER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      TBCHAR = 'CL'
      CALL UV2TAB (UVDATA, PCTAB, TBCHAR, TBVER, IRET)
      IF (IRET.NE.0) GO TO 999
      WRITE (MSGTXT,1019) 'Phase cal info from table ', TBCHAR, TBVER
      CALL MSGWRT(4)
C
C                                       prep SN table object
      CALL OGET (POPS, 'SNVER', TYPE, DIM, TBVER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      TBCHAR = 'SN'
      CALL UV2TAB (UVDATA, SNTAB, TBCHAR, TBVER, IRET)
      IF (IRET.NE.0) GO TO 999
      WRITE (MSGTXT,1019) 'Solution times from table', TBCHAR, TBVER
      CALL MSGWRT(4)
C
C                                       prep HF table output object
      CALL OGET (POPS, 'OUTVERS', TYPE, DIM, TBVER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      TBCHAR = 'HF'
      CALL UV2TAB (UVDATA, HFTAB, TBCHAR, TBVER, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (HFTAB, 'VER', TYPE, DIM, TBVER, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      WRITE (MSGTXT,1019) 'HF output stored in table', TBCHAR, TBVER
      CALL MSGWRT(4)
C
C                                       get info from AN, SQ, and FQ tables
      CALL OGET (POPS, 'SUBARRAY', TYPE, DIM, SUBNO, CDUMMY, IRET)
C       print *, ' SUBNO   ', SUBNO
C       print *, ' SUBAR   ', SUBAR
C       print *, ' SUBARR  ', SUBARR
      IF (IRET.NE.0) GO TO 999
      CALL DSCINF (UVDATA, IRET, SUBNO, ANAME, OBSDAT, REFREQ, DATUTC,
     *     SUMAX, SUNAME, FREQNO, NUMIF, FOFF, TBW)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SNTAB, 'SUBNO', TYPE, DIM, SUBNO, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
C                                       Process schedule file name
C                                       to get time tags if so requested
      IF ( (APARM(1).EQ.2.0) .OR.
     *     (APARM(1).EQ.3.0) .OR.
     *     (APARM(1).EQ.4.0)     ) THEN
C                                       Get schedule file name
         CALL OGET (POPS, 'INFILE', TYPE, DIM, FDUM, LFILE, IRET)
C         WRITE(6,'(" LFILE, IRET: ",A48, I4)') LFILE, IRET
         IF(IRET.NE.0) GO TO 999
C                                       Convert to full path name
         CALL ZFULLN(LFILE, ' ', '     ', FNAME, IRET)
         IF(IRET.NE.0) GO TO 999
         DO 50 I = 1,127
            IF (FNAME(I:I).EQ.' ') THEN
               FNAME(I:I) = '/'
C              WRITE(6,'(" FNAME: ",A)') FNAME
               GO TO 60
               END IF
 50         CONTINUE
 60      CONTINUE
C        NCH = I
C
C                                       Get time tag files
         LUNIT = 24
         OPEN (UNIT=LUNIT, STATUS='OLD', FILE=FNAME)
         IF (APARM(1) .EQ. 2.) THEN
            CALL TAGS(LUNIT)
         ENDIF
C
         IF (APARM(1) .EQ. 3.) THEN
            CALL MK3SC(LUNIT)
         ENDIF
C
         IF (APARM(1) .EQ. 4.) THEN
            CALL TSCAN(LUNIT)
         ENDIF
         CLOSE (LUNIT)
      ENDIF
C
C                                       process timerange if given
      CALL OGET (POPS, 'TIMERANG', TYPE, DIM, TR, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      TSTART = TR(1) + TR(2) / 24.0D0 + TR(3) / 1440.0D0 +
     *         TR(4) / 86400.0D0
      TSTOP  = TR(5) + TR(6) / 24.0D0 + TR(7) / 1440.0D0 +
     *         TR(8) / 86400.0D0
      IF (TSTART.LT.1.0D-7) TSTART = -1000.0D0
      IF (TSTOP.LT.1.0D-7) TSTOP = -1000.0D0
      IF (TSTOP.GT.0.0D0) THEN
         DO 127 I=1,8
            TI(I) = INT(TR(I))
 127        CONTINUE
         WRITE (MSGTXT, 1200) (TI(I),I=1,8)
      ELSE
         WRITE (MSGTXT, 1201)
         END IF
      CALL MSGWRT(4)
C                                       save timerang
      TYPE = OOADP
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (MOTAB, 'TSTART', TYPE, DIM, TSTART, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SNTAB, 'TSTART', TYPE, DIM, TSTART, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (PCTAB, 'TSTART', TYPE, DIM, TSTART, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (MOTAB, 'TSTOP', TYPE, DIM, TSTOP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (SNTAB, 'TSTOP', TYPE, DIM, TSTOP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OPUT (PCTAB, 'TSTOP', TYPE, DIM, TSTOP, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
C       print *, ' SUBAR   ', SUBAR
C                                       initialize CL row info arrays
C     CALL CLINIT
C
 999  RETURN
 1019 FORMAT (A36,' ',A2,' # ', I2)
 1200 FORMAT ('Time range', I4,'/ ',3I3,' to', I4,'/ ',3I3)
 1201 FORMAT ('No time range specified, accept all times')
      END
      SUBROUTINE CLTOHF (MOTAB, PCTAB, SNTAB, UVDATA, HFTAB, IERR)
C-----------------------------------------------------------------------
C   Convert CL to HF table.
C    Currently uses a CL table as the phase cal table.
C    Uses SN table to get actual times of the data.
C   Inputs:
C      MOTAB   C*?   Name of input table object.
C      PCTAB   C*?   Name of input phase cal table object.
C      SNTAB   C*?   Name of input SN table
C      UVDATA  C*?   Name of input uv data
C      HFTAB  C*?   Name of output table object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
C     INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'INCS:DGLB.INC'
C
C                                       antenna, source, and frequency
C                                       info come from ANSUFQINFO
      INCLUDE 'CLINFO.INC'
      INCLUDE 'ANSUFQINFO'
C
      CHARACTER MOTAB*(*), PCTAB*(*), SNTAB*(*), UVDATA*(*),
     *     HFTAB*(*)
      INTEGER   TYPE, DIM(3), ANTMAX, NPOLN, NTERMS, NUMNOD, HFROW,
     *   PCROW, SNROW, MOROW, NSNROW, I, NZERO, SNMTCH, SNANT(MXGANT),
     *   HFNUM , IERR, IC2(6), J, ISCAN, ISCN, IC3(6)
      LOGICAL ISAPPL
      DOUBLE PRECISION ANTDLY(9,MXGANT), CURTIM(MXGANT),
     *   SNTIMC(MXGANT), TTAG
      REAL  ANTCLK(2,MXGANT), ANTDCK(2,MXGANT), ANTATM(MXGANT),
     *   ANTDAM(MXGANT), ANTMBD(2,MXGANT), ANTSBD(2,MXGANT),
     *   ANTRAT(2,MXGANT), ANTPHS(MAXIF,2,MXGANT),
     *   ANTGAI(MAXIF,2,MXGANT), ANTSNR(2,MXGANT),
     *   AIFSNR(MAXIF,2,MXGANT), ELEV(MXGANT), INTER,
     *   ANTPC(MAXIF,2,MXGANT), ANTPCR(MAXIF,2,MXGANT),
     *   ANTPCA(MAXIF,2,MXGANT),
     *   SNINT(MXGANT), RANOD(25), DECNOD(25),
     *   CLRAT(2,MXGANT), CLSBD(2,MXGANT)
      CHARACTER  SNAM*16, CDUM*1
      INTEGER PCPOLN, MOPOLN, SNPOLN, PCNIF, MONIF, SNNIF,
     *     PCNTRM, MONTRM, SOUNO, SUBNO
      DOUBLE PRECISION SNTIME, TIMNO
      REAL PCGMOD, MOGMOD, SNGMOD, SNDUR
C
      DATA NZERO /0/
C-----------------------------------------------------------------------
C                                       Sort input tables
      WRITE (MSGTXT, 1000)
      CALL MSGWRT(4)
      CALL TBLSRT (PCTAB, 'TIME     ', 'ANTENNA NO.', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TBLSRT (MOTAB, 'TIME     ', 'ANTENNA NO.', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TBLSRT (SNTAB, 'TIME     ', 'ANTENNA NO.', IERR)
      IF (IERR.NE.0) GO TO 999
C
C                                       Create/Open input tables
      CALL OCLINI (PCTAB, 'READ', PCROW, ANTMAX, PCPOLN, PCNIF,
     *   PCNTRM, PCGMOD, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLINI (MOTAB, 'READ', MOROW, ANTMAX, MOPOLN, MONIF,
     *   MONTRM, MOGMOD, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OSNINI (SNTAB, 'READ', SNROW, ANTMAX, SNPOLN, SNNIF,
     *   NUMNOD, SNGMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       some sanity checks
      IERR = 1
      IF (PCPOLN.NE.MOPOLN) GO TO 999
      IF (PCNTRM.NE.MONTRM) GO TO 999
      IF (PCNIF .NE.MONIF ) GO TO 999
      NPOLN  = PCPOLN
      NUMIF  = PCNIF
      NTERMS = PCNTRM
      IERR = 0
C
C                                       Create/Open output table
      CALL OHFINI (HFTAB, 'WRIT', HFROW, IERR)
      IF (IERR.NE.0) GO TO 999
C
C                                       Get number of SN table entries
      CALL OGET (SNTAB, 'NROW', TYPE, DIM, NSNROW, CDUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get subarray number
      CALL OGET (SNTAB, 'SUBNO', TYPE, DIM, SUBNO, CDUM, IERR)
      IF (IERR.NE.0) GO TO 999
C
C                                       initialize SN/CL row info arrays
      CALL CLINIT
      CALL SNSORT (SNTAB, NSNROW, SUBNO)
      CALL UTCTAG
      CALL CLSORT (MOTAB, SUBNO, FREQNO, NTERMS, NPOLN, NUMIF, IERR)
      CALL PCSORT (PCTAB, SUBNO, FREQNO, NTERMS, NPOLN, NUMIF, IERR)
C
C                                       Get processing time (year, month,
C                                       day, hour, minute, second).
      CALL ZDATE(IC3(1))
      CALL ZTIME(IC3(4))
C
C                                       Loop over time here to process
C                                       all the data.
C
      DO 101 ISCAN = 1, IKOUNT
C
       ISCN = ISCAN
C
         CALL SCPLOD(ISCN)
          IF(IANTS.LT.2) GO TO 101
C
C                                       Init arrays and counters for
C                                       SN, CL, PC variables
         CALL HFINIT (SNTIMC, ANTMBD, ANTSNR, SNINT, ANTPHS, ANTGAI,
     *        AIFSNR, SNTIME, SNDUR, SNMTCH, TIMNO, SOUNO, SNANT,
     *        HFNUM, CURTIM, ANTDLY, ANTCLK, ANTDCK,
     *        ANTATM, ANTDAM, ANTRAT, ANTSBD, ANTPC, ANTPCR,
     *        ANTPCA, NTERMS, MAXIF)
C
C                                       Load arrays for record numbers
          TTAG = TSN(ISCN,4)
C                                       Time tag in Y/M/D/H/M/S
        DO 90 J = 1,6
          IC2(J) = IC02(ISCN,J)
  90    CONTINUE
C
C                                       Get solution info from SN table
         CALL SNGET (SNTAB, NSNROW, NPOLN, NUMIF, FOFF, SUBNO, SNROW,
     *     SOUNO, ANTMBD, ANTRAT, ANTSBD, ANTPHS, ANTGAI, ANTSNR,
     *     AIFSNR, SNTIMC, SNINT, SNANT, IERR )
         IF (IERR.NE.0) GO TO 800
C                                       Get source name string
         SNAM = SUNAME(SOUNO)
C
C                                       Get model info from CL table
         CALL CLGET (MOTAB, SUBNO, SOUNO, CURTIM, INTER,
     *        NTERMS, NPOLN, NUMIF, ANTMAX, ANTDLY, ANTCLK,
     *        ANTDCK, ANTATM, ANTDAM, ELEV, IERR,
     *         CLRAT,  CLSBD,  ANTGAI, ANTSNR, AIFSNR )
C        IF (IERR.NE.0) GO TO 999
         IERR = 0
C                                       Get phase cals from specified
C                                       CL table
         CALL PCGET (PCTAB, SUBNO, NPOLN, NUMIF, ANTPC, ANTPCR, ANTPCA,
     *      IERR )
C        IF (IERR.GT.0) GO TO 999
         IERR = 0
C                                       Convert time to UTC
         DO 200 I = 1,ANTMAX
            CURTIM(I) = CURTIM(I) - DATUTC/86400.D0
            SNTIMC(I) = SNTIMC(I) - DATUTC/86400.D0
 200     CONTINUE
C                                       Write HF entries
         CALL HFPUT (UVDATA, HFTAB, HFROW, CURTIM, TTAG, IC2, INTER,
     *      NTERMS, NPOLN, ANTMAX, ANTDLY, ANTCLK, ANTDCK, ANTATM,
     *      ANTDAM, ANTMBD, ANTSBD, ANTRAT, ANTPHS, ANTGAI, ANTPC,
     *      ANTPCR, ANTPCA, ANTSNR, AIFSNR, ELEV, SNAM, ANAME, OBSDAT,
     *      REFREQ, NUMIF, FOFF, TBW, SNTIMC, SNINT, DATUTC, NZERO, IC3,
     *      IERR )
         IF (IERR.NE.0) GO TO 999
C                                       Loop for next time
 101   CONTINUE
C        GO TO 100
C                                       all done!
C
C                                       Close tables
 800  CONTINUE
      CALL OCLOSE (MOTAB, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (PCTAB, IERR)
C     IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (HFTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
C                                       Make sure some written
      IF (HFROW.GT.1) THEN
         WRITE (MSGTXT,1800) HFROW-1
         CALL MSGWRT (4)
         IF (NZERO .NE. 0) THEN
            WRITE (MSGTXT, 1802) NZERO
            CALL MSGWRT(4)
         END IF
      ELSE
C                                       No data written, write message !
C                                       and keep going
         MSGTXT = 'WARNING: No HF entries written'
         CALL MSGWRT (8)
         IERR = 0
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Sorting Input tables')
 1800 FORMAT ('Wrote ',I6,' HF table entries')
 1802 FORMAT (I4, ' HF entries skipped for zero delay')
      END
C-----------------------------------------------------------------------
      SUBROUTINE CL2HHI (POPS, HFTAB)
C-----------------------------------------------------------------------
C   Routine to write history file to output table object.  This assumes
C   that a previous history exists and merely adds the information from
C   the current task.
C   Inputs:
C      HFTAB  C*?  Output table object
C-----------------------------------------------------------------------
      CHARACTER HFTAB*(*), POPS*(*)
C
      INTEGER   NADV
      PARAMETER (NADV=8)
      CHARACTER LIST(NADV)*8
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INVERS', 'IN2VERS',
     *   'SNVER', 'OUTVERS', 'TIMERANG'/
C-----------------------------------------------------------------------
C                                       Add task label to history
      CALL OHTIME (HFTAB, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy adverb values.
      CALL OHLIST (POPS, LIST, NADV, HFTAB, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // HFTAB
      CALL MSGWRT (4)
 999  RETURN
      END
      SUBROUTINE DSCINF (UVDATA, IERR, SUBNO, ANAME, OBSDAT, REFREQ,
     *     DATUTC, SUMAX, SUNAME, FREQNO, NUMIF, FOFF, TBW)
C-----------------------------------------------------------------------
C   Returns descriptive info from the AN, SU, and FQ tables
C   Inputs:
C      UVDATA  C*?     UV data object
C      SUMAX   I       Max number of source names allowed
C      SUBNO   I       Subarray number
C   Output:
C      ANAME   C(*)*8  Antenna names
C      OBSDAT  C*8     Observing date 'yyyymmdd' in 15OCT98 AIPS,
C                       'dd/mm/yy' in earlier versions
C      REFREQ  D       Reference frequency (Hz)
C      DATUTC  R       Data time - UTC (sec)
C      SUNAME  C(*)*16 Source names
C      NUMIF   I       Number of IFs
C      FOFF    D(*)    IF frequency offsets
C      TBW     R(*)    IF bandwidths
C      IERR    I       Return code 0=>OK.
C-----------------------------------------------------------------------
      INTEGER SUMAX, SUBNO
      CHARACTER UVDATA*(*), ANAME(*)*8, OBSDAT*8, SUNAME(SUMAX)*16
      INTEGER IERR
      INTEGER NUMIF, FREQNO
      REAL TBW(*), DATUTC
      DOUBLE PRECISION FOFF(*), REFREQ, DDUMMY
      CHARACTER CDUM*1, TMPTAB*36, TBCHAR*2
      INTEGER  I, COLS(2), TYPE, DIM(3), DUM, TBVER, NROW, IROW
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'CLINFO.INC'
      INTEGER   ISBAND(MAXIF)
      REAL      BW(MAXIF)
      CHARACTER SUCLAB(2)*24, ANCLAB(2)*24, BNDCOD(MAXIF)*8
      DATA SUCLAB /'ID. NO.' , 'SOURCE'/
      DATA ANCLAB /'NOSTA', 'ANNAME'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       prep for AN table
      TBCHAR = 'AN'
      TBVER = SUBNO
      TMPTAB = 'Temporary AN table object'
      CALL UV2TAB (UVDATA, TMPTAB, TBCHAR, TBVER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open AN table
      CALL OOPEN (TMPTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get reference date
      CALL OGET (TMPTAB, 'KEY.RDATE', TYPE, DIM, DUM, OBSDAT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get reference frequency
      CALL OGET (TMPTAB, 'KEY.FREQ', TYPE, DIM, REFREQ, CDUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get data time - UTC
      CALL OGET (TMPTAB, 'KEY.DATUTC', TYPE, DIM, DDUMMY, CDUM, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (TYPE.EQ.OOARE) THEN
         CALL OGET (TMPTAB, 'KEY.DATUTC', TYPE, DIM, DATUTC, CDUM, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         DATUTC = DDUMMY
         END IF
C                                       Get number of entries
      CALL OGET (TMPTAB, 'NROW', TYPE, DIM, NROW, CDUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       prep antenna name look up table
      DO 10 I = 1,MXGANT
         ANAME(I) = ' '
 10      CONTINUE
C                                       Find column numbers
      CALL TABCOL (TMPTAB, 2, ANCLAB, COLS, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
C                                       Make sure all columns found
      DO 20 I = 1,2
         IF (COLS(I).LE.0) THEN
            MSGTXT = 'AN TABLE MISSING COLUMN ' // ANCLAB(I)
            CALL MSGWRT (9)
            IERR = 7
          END IF
 20   CONTINUE
      IF (IERR.NE.0) GO TO 999
C                                       Loop over table
      DO 30 I = 1,NROW
         CALL TABDGT (TMPTAB, I, COLS(1), TYPE, DIM, IROW, CDUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT (TMPTAB, I, COLS(2), TYPE, DIM, DUM, ANAME(IROW),
     *        IERR)
         IF (IERR.NE.0) GO TO 999
 30      CONTINUE
C                                       Close and Delete AN table object
      CALL OCLOSE (TMPTAB, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL DESTRY (TMPTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
C                                       prep for SU table
      TBCHAR = 'SU'
      TBVER = 1
      TMPTAB = 'Temporary SU table object'
      CALL UV2TAB (UVDATA, TMPTAB, TBCHAR, TBVER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       init source name look up table
      DO 110 I = 1,SUMAX
         SUNAME(I) = ' '
 110     CONTINUE
C                                       Open SU table and find column names
      CALL OOPEN (TMPTAB, 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TABCOL (TMPTAB, 2, SUCLAB, COLS, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
C                                       all columns found?
      DO 120 I = 1,2
         IF (COLS(I).LE.0) THEN
            MSGTXT = 'SU TABLE MISSING COLUMN ' // SUCLAB(I)
            CALL MSGWRT (9)
            IERR = 7
          END IF
 120   CONTINUE
      IF (IERR.NE.0) GO TO 999
C                                       Get number of entries
      CALL OGET (TMPTAB, 'NROW', TYPE, DIM, NROW, CDUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Loop over all rows
      DO 130 I = 1,NROW
         CALL TABDGT(TMPTAB, I, COLS(1), TYPE, DIM, IROW, CDUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL TABDGT(TMPTAB, I, COLS(2), TYPE, DIM, DUM, SUNAME(IROW),
     *      IERR)
         IF (IERR.NE.0) GO TO 999
 130     CONTINUE
C                                       Close and Delete SU table object
      CALL OCLOSE (TMPTAB, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL DESTRY (TMPTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
C                                       Now process FQ table
      TBCHAR = 'FQ'
      TBVER = 1
      TMPTAB = 'Temporary FQ table object'
      CALL UV2TAB (UVDATA, TMPTAB, TBCHAR, TBVER, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OFQINI (TMPTAB, 'READ',  NROW, NUMIF, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OTABFQ (TMPTAB, 'READ', NROW, NUMIF, FREQNO, FOFF, BW, TBW,
     *   ISBAND, BNDCOD, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OCLOSE (TMPTAB, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL DESTRY (TMPTAB, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE SNSORT (SNTAB, NSNROW, SUBNO)
C-----------------------------------------------------------------------
C
C  Read complete SN table. Set up arrays of SN table times, sources,
C   subarray numbers, stations, and record numbers.
C
C-----------------------------------------------------------------------
      INCLUDE 'CLINFO.INC'
C
      CHARACTER SNTAB*(*)
      INTEGER NSNROW, NPOLN, IERR, SUBNO, SOUNO, SNMTCH, KSN, SUBBA
      DOUBLE PRECISION TIMNO
C     REAL SNDUR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
C     INCLUDE 'INCS:DDCH.INC'
      DOUBLE PRECISION TIMEA, TSTART, TSTOP
      REAL   TIMEI, IFR, MBDELY(2), CREAL(2,MAXIF), CIMAG(2,MAXIF),
     *   DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF), DISP(2),
     *   DDISP(2)
      INTEGER  ANTA, SUBA, FREQID, REFA(2,MAXIF), SOUA,
     *   IROW, JROW, NODENO
      INTEGER TYPE, DIM(3)
      CHARACTER CDUM*8
C
C-----------------------------------------------------------------------
      IERR = 0
C
      CALL OGET (SNTAB, 'TSTART', TYPE, DIM, TSTART, CDUM, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (SNTAB, 'TSTOP', TYPE, DIM, TSTOP, CDUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Read input table accumulating
C                                       info.
       KSN = 0
       SNMTCH = 0
       TIMNO  = -9999.D0
       SOUNO  = -9999
       SUBBA  = -9999
      DO 100 JROW = 1,NSNROW
C                                       Read data for this row
         IROW = JROW
         CALL OTABSN (SNTAB, 'READ', IROW, NPOLN, TIMEA, TIMEI, SOUA,
     *      ANTA, SUBA, FREQID, IFR, NODENO, MBDELY, DISP, DDISP, CREAL,
     *      CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C                                       trap errors
         IF (IERR.LT.0) THEN
            IERR = 0
            GO TO 100
            END IF
         IF (IERR.NE.0) GO TO 999
C                                       trap start/stop times
         IF (TSTOP.GT.0.0D0) THEN
            IF (TIMEA .LT. TSTART) GO TO 100
            IF (TIMEA .GT. TSTOP) GO TO 999
         END IF
C                                       trap subarray number
         IF ((SUBA.NE.SUBNO).AND.(SUBA*SUBNO.NE.0)) GO TO 100
C                                       trap short scans (<10 sec]
         IF (TIMEI*86400. .LT. 10.) GO TO 100
C                                       Check for new time/source
C                                       when time or source mismatch,
C                                       save JROW and leave
         IF ( (DABS(TIMNO-TIMEA).GT.1.D-12) .OR. (SOUNO.NE.SOUA)
     *        .OR. (SUBBA.NE.SUBA) ) THEN
C
           KSN = KSN + 1
           SNMTCH = 1
           TSN(KSN,1) = TIMEA
           TSN(KSN,5) = TIMEI
           ISUB1(KSN,1) = SUBA
           ISRC1(KSN)  = SOUA
           IRECS1(KSN,ANTA,1) = JROW
           TIMNO = TIMEA
           SOUNO = SOUA
           SUBBA = SUBA
C
           GO TO 100
         END IF
C
C                                       If record matches previous
C                                       time/source/subarray
         IF ( (DABS(TIMNO-TIMEA).LT.1.D-12) .AND. (SOUNO.EQ.SOUA)
     *        .AND. (SUBBA.EQ.SUBA) ) THEN
C
           SNMTCH =SNMTCH + 1
           IRECS1(KSN,ANTA,1) = JROW
           GO TO 100
         ENDIF
 100  CONTINUE
C
 999  CONTINUE
        IKOUNT = KSN
C
C       DO 301 I = 1, IKOUNT
C        WRITE(6,1011) I, TSN(I,1), TSN(I,2), TSN(I,3), ISUB1(I,1),
C    *                 ISRC1(I), (IRECS1(I,J,1), J=1,10)
C301    CONTINUE
C1011   FORMAT(I4,3F12.8,I3,I4,10I6)
C
      RETURN
      END
      SUBROUTINE UTCTAG
C-----------------------------------------------------------------------
C  Match SN and CL table rows
C
C   Inputs:
C      OBSDAT  C*8   Observing reference data (dd/mm/yy)
C
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CLINFO.INC'
      INCLUDE 'ANSUFQINFO'
C
      DOUBLE PRECISION TTAG, SNTIME, SNDUR
      INTEGER J, K, IWNDW, IC2(6), ISRC, NN, IQN
C     REAL SNDUR
      CHARACTER SNAM*16, CC25*8
      REAL      APARM(10), DPARM(10)
      COMMON /APRM/ APARM, DPARM
C
C-----------------------------------------------------------------------
C
C Get the Time tag or UTC EPOCH, IC2, depending on the value of APARM(1). We
C  require it to be on an integer second. This time tag will be the reference
C  time for the observed delay, rate, and phase for the current scan.
C
      DO 100 K = 1, IKOUNT

       ISRC = ISRC1(K)
       SNAM = SUNAME(ISRC)
       CC25 = SNAM(1:8)
       SNTIME = TSN(K,1)
       SNDUR  = TSN(K,5)
C
C        For APARM(1)=1, time tag is average SN time rounded to nearest second
C        For APARM(1)=0 it will be the CL time, but we must initially set it
C                       to the SN time.
      IF (APARM(1).LE.1.) THEN
         NN = 2
         IQN = 1
         CALL HAYDY2 (NN, OBSDAT, SNTIME, CC25, IQN, IC2, TTAG)
         END IF
C
C        For APARM(1)=2, 3, or 4, find time tag in TMTGS Common Block
      IF((APARM(1) .GE. 2.) .AND. (APARM(1) .LE. 4.)) THEN
C         print *,'UTCTAG:OBSDAT,SNTIME,CC25 ', OBSDAT,SNTIME,CC25
         NN = 1
         IQN = 10
         CALL HAYDY2 (NN, OBSDAT, SNTIME, CC25, IQN, IC2, TTAG)
C
C                                       if time tag comes back invalid
         IF (DABS(TTAG-SNTIME).GT.(SNDUR/2.D0)) THEN
C                                       then complain
            WRITE(MSGTXT,1010) TTAG, SNTIME, SNDUR
 1010 FORMAT ('Time tag TTAG outside SN interval SNTIME +/- SNDUR',
     *             3F10.7)
            CALL MSGWRT (4)
C                                       and revert to SN time
            NN = 2
            IQN = 10
            CALL HAYDY2 (NN, OBSDAT, SNTIME, CC25, IQN, IC2, TTAG)
         END IF
C
      END IF
C
       TSN(K,4) = TTAG
C                                       2 min. window
        IWNDW = TTAG * 720.D0
        TSN(K,2) = IWNDW / 720.D0
        TSN(K,3) = (IWNDW+1) / 720.D0
C
C                                       Time tag in Y/M/D/H/M/S
        DO 90 J = 1,6
          IC02(K,J) = IC2(J)
  90    CONTINUE
C
 100  CONTINUE
C
      RETURN
      END
      SUBROUTINE CLSORT (MOTAB, SUBNO, FREQNO, NTERMS, NPOLN, NUMIF,
     *                   IERR)
C-----------------------------------------------------------------------
C   Read through the CL table and save row numbers for matching with the
C    SN table.
C
C   Inputs:
C      MOTAB   C*?  Input CL table object, should be open.
C      SUBNO   I    Subarray number to process
C      NTERMS  I    Number of terms in delay polynomial
C      NPOLN   I    Number of polarizations (1 or 2)
C
C   Output:
C
C      IERR    I    Return code, -1 => data all processed, 0=>OK,
C                   else failed.
C-----------------------------------------------------------------------
      CHARACTER MOTAB*(*)
      INTEGER SUBNO, FREQNO, NTERMS, NPOLN, NUMIF, IERR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
C     INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'CLINFO.INC'
C
      INTEGER NROW, JROW, IROW
      DOUBLE PRECISION TSTART, TSTOP, GEODLY(9), TIMEA
      REAL TIMEI, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2), CLOCK(2),
     *   DCLOCK(2), DISP(2), DDISP(2), CREAL(2,MAXIF), CIMAG(2,MAXIF),
     *   DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF), IFR
      INTEGER SOUA, ANTNO, SUBA, FQA, REFA(2,MAXIF), J, K, NUM, IZERO
      INTEGER TYPE, DIM(3)
      CHARACTER CDUM*8
C-----------------------------------------------------------------------
      IERR = 0
      NUM = 0
      CALL OGET (MOTAB, 'TSTART', TYPE, DIM, TSTART, CDUM, IERR)
      IF (IERR.NE.0) GO TO 997
      CALL OGET (MOTAB, 'TSTOP', TYPE, DIM, TSTOP, CDUM, IERR)
      IF (IERR.NE.0) GO TO 997
      CALL OGET (MOTAB, 'NROW', TYPE, DIM, NROW, CDUM, IERR)
      IF (IERR.NE.0) GO TO 997
C
      DO 1000 JROW = 1, NROW
C                                       Read data for this row
         IROW = JROW
         CALL OTABCL (MOTAB, 'READ', IROW, NPOLN, NUMIF, TIMEA,
     *       TIMEI, SOUA, ANTNO, SUBA, FQA, IFR, GEODLY, DOPOFF,
     *       ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP, DDISP, CREAL,
     *       CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C                                       trap errors
         IF (IERR.LT.0) GO TO 1000
         IF (IERR.NE.0) GO TO 997
C                                       trap start/stop times
         IF (TSTOP.GT.0.0D0) THEN
            IF (TIMEA .LT. TSTART) GO TO 1000
            IF (TIMEA .GT. TSTOP)  GO TO 997
         END IF
C                                       trap subarray number
         IF ((SUBNO.NE.SUBA).AND.(SUBNO*SUBA.NE.0)) GO TO 1000
C                                       trap frequency id
         IF ((FQA.NE.FREQNO).AND.(FQA*FREQNO.NE.0)) GO TO 1000
C                                       trap missing model coefficients
         IZERO = 0
         DO 22 J = 1, NTERMS
            IF (GEODLY(J) .EQ. 0.0D0) IZERO = IZERO + 1
 22         CONTINUE
         IF (IZERO .GE. 2) GO TO 1000
C                                       trap invalid source id
         IF (SOUA.LE.0) GO TO 1000
C
       DO 33 K = 1,IKOUNT
C                                       Find correct 2-minute window and source
C
        IF ( (TIMEA .GE. TSN(K,2)) .AND. (TIMEA .LT. TSN(K,3)) .AND.
     *       (SOUA .EQ. ISRC1(K)) )  THEN
C
C                                       Skip if no SN record here
          IF (IRECS1(K,ANTNO,1) .EQ. 0) GO TO 40
C
C                                       Skip if a best match already found
          IF (IRECS1(K,ANTNO,2) .GT. 0) GO TO 40
C
C                                       First time match
          IF (IRECS1(K,ANTNO,2) .EQ. 0) THEN
            IRECS1(K,ANTNO,2) = JROW
            NUM = NUM + 1
C                                       Leave positive if best possible match
C                                       Set negative otherwise
            IF ( (DABS(TIMEA-TSN(K,4))) .GT. TIMEI/2.0 )
     *             IRECS1(K,ANTNO,2) = -IRECS1(K,ANTNO,2)
            GO TO 40
          ENDIF
C                                       Matched but not best match
          IF (IRECS1(K,ANTNO,2) .LT. 0) THEN
C                                       Replace with best match
            IF ( (DABS(TIMEA-TSN(K,4))) .LE. TIMEI/2.0 )  THEN
              IRECS1(K,ANTNO,2) = JROW
              GO TO 40
            ENDIF
C                                       Replace with better match
            IF ( (DABS(TIMEA-TSN(K,4))) .LE. TIMEI )  THEN
              IRECS1(K,ANTNO,2) = -JROW
              GO TO 40
            ENDIF
          ENDIF
C
        ENDIF
C
  40   CONTINUE
C                                       Time to quit?
            IF ( (TSN(K,3) - TIMEA) .GT. .015D0 ) GO TO 1000
C
  33   CONTINUE
C
 1000    CONTINUE
C
  997 CONTINUE
C                                       no valid CL table entries found
      IF (NUM.EQ.0) IERR = 1
C
C       DO 301 I = 1, IKOUNT
C       DO 301 I = 1, 10
C        WRITE(6,1011) I,TSN(I,1),TSN(I,2),TSN(I,3),TSN(I,4),ISUB1(I,1),
C    *                 ISRC1(I), (IRECS1(I,J,1), J=1,10),
C    *                           (IRECS1(I,J,2), J=1,10)
C301    CONTINUE
C1011   FORMAT(I4,4F10.6,I3,I4,10I6,/,51X,10I6)
C
      RETURN
      END
      SUBROUTINE PCSORT (PCTAB, SUBNO, FREQNO, NTERMS, NPOLN, NUMIF,
     *                   IERR)
C-----------------------------------------------------------------------
C   Read through the PC/CL table and save row numbers for matching with the
C    SN table.
C
C   Inputs:
C      PCTAB   C*?  Input PC/CL table object, should be open.
C      SUBNO   I    Subarray number to process
C      NTERMS  I    Number of terms in delay polynomial
C      NPOLN   I    Number of polarizations (1 or 2)
C
C   Output:
C
C      IERR    I    Return code, -1 => data all processed, 0=>OK,
C                   else failed.
C-----------------------------------------------------------------------
      CHARACTER PCTAB*(*)
      INTEGER SUBNO, FREQNO, NTERMS, NPOLN, NUMIF, IERR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
C     INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'INCS:DGLB.INC'
C
      INTEGER NROW, JROW, IROW
      DOUBLE PRECISION TSTART, TSTOP, GEODLY(9), TIMEA
      REAL TIMEI, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *     CLOCK(2), DCLOCK(2), DISP(2), DDISP(2), CREAL(2,MAXIF),
     *     CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *     WEIGHT(2,MAXIF), IFR
      INTEGER SOUA, ANTNO, SUBA, FQA, REFA(2,MAXIF), J, K, NUM, IZERO
      INTEGER TYPE, DIM(3)
      CHARACTER CDUM*8
      INCLUDE 'CLINFO.INC'
C-----------------------------------------------------------------------
      IERR = 0
      NUM = 0
      CALL OGET (PCTAB, 'TSTART', TYPE, DIM, TSTART, CDUM, IERR)
      IF (IERR.NE.0) GO TO 997
      CALL OGET (PCTAB, 'TSTOP', TYPE, DIM, TSTOP, CDUM, IERR)
      IF (IERR.NE.0) GO TO 997
      CALL OGET (PCTAB, 'NROW', TYPE, DIM, NROW, CDUM, IERR)
      IF (IERR.NE.0) GO TO 997
C
      DO 1000 JROW = 1, NROW
C                                       Read data for this row
         IROW = JROW
         CALL OTABCL (PCTAB, 'READ', IROW, NPOLN, NUMIF, TIMEA,
     *       TIMEI, SOUA, ANTNO, SUBA, FQA, IFR, GEODLY, DOPOFF,
     *       ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP, DDISP, CREAL,
     *       CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C                                       trap errors
         IF (IERR.LT.0) GO TO 1000
         IF (IERR.NE.0) GO TO 997
C                                       trap start/stop times
         IF (TSTOP.GT.0.0D0) THEN
            IF (TIMEA .LT. TSTART) GO TO 1000
            IF (TIMEA .GT. TSTOP)  GO TO 997
         END IF
C                                       trap subarray number
         IF ((SUBNO.NE.SUBA).AND.(SUBNO*SUBA.NE.0)) GO TO 1000
C                                       trap frequency id
         IF ((FQA.NE.FREQNO).AND.(FQA*FREQNO.NE.0)) GO TO 1000
C                                       trap missing model coefficients
         IZERO = 0
         DO 22 J = 1, NTERMS
            IF (GEODLY(J) .EQ. 0.0D0) IZERO = IZERO + 1
 22         CONTINUE
         IF (IZERO .GE. 2) GO TO 1000
C                                       trap invalid source id
         IF (SOUA.LE.0) GO TO 1000
C
C      IF (JROW.LE.300) PRINT *,'JROW,TIMEA,SOUA,ANTNO,IKOUNT',
C    *                           JROW,TIMEA,SOUA,ANTNO,IKOUNT
C
       DO 33 K = 1,IKOUNT
C                                       Find correct 2-minute window and source
C       IF ( (TIMEA .GE. TSN(K,2)) .AND. (TIMEA .LT. TSN(K,3)) )
C    *     PRINT *, 'Match: Sources, Ant ', SOUA, ISRC1(K), ANTNO
C
        IF ( (TIMEA .GE. TSN(K,2)) .AND. (TIMEA .LT. TSN(K,3)) .AND.
     *       (SOUA .EQ. ISRC1(K)) )  THEN
C
C        PRINT *,' JROW,etc. ', JROW,K,TIMEA,TSN(K,4),TSN(K,2),TSN(K,3),
C    *                           SOUA,IRECS1(K,ANTNO,1)
C                                       Skip if no SN record here
          IF (IRECS1(K,ANTNO,1) .EQ. 0) GO TO 40
C
C                                       Skip if a best match already found
          IF (IRECS1(K,ANTNO,3) .GT. 0) GO TO 40
C
C                                       First time match
          IF (IRECS1(K,ANTNO,3) .EQ. 0) THEN
            IRECS1(K,ANTNO,3) = JROW
            NUM = NUM + 1
C                                       Leave positive if best possible match
C                                       Set negative otherwise
            IF ( (DABS(TIMEA-TSN(K,4))) .GT. TIMEI/2.0 )
     *             IRECS1(K,ANTNO,3) = -IRECS1(K,ANTNO,3)
C           PRINT *, 'NUM,IRECS1(3),ANTNO ', NUM,IRECS1(K,ANTNO,3),ANTNO
            GO TO 40
          ENDIF
C                                       Matched but not best match
          IF (IRECS1(K,ANTNO,3) .LT. 0) THEN
C                                       Replace with best match
            IF ( (DABS(TIMEA-TSN(K,4))) .LE. TIMEI/2.0 )  THEN
              IRECS1(K,ANTNO,3) = JROW
              GO TO 40
            ENDIF
C                                       Replace with better match
            IF ( (DABS(TIMEA-TSN(K,4))) .LE. TIMEI )  THEN
              IRECS1(K,ANTNO,3) = -JROW
              GO TO 40
            ENDIF
          ENDIF
C
        ENDIF
C
  40   CONTINUE
C                                       Time to quit?
            IF ( (TSN(K,3) - TIMEA) .GT. .015D0 ) GO TO 1000
C
  33   CONTINUE
C
 1000    CONTINUE
C
  997 CONTINUE
C                                       no valid CL table entries found
      IF (NUM.EQ.0) IERR = 1
C
C       DO 301 I = 1, IKOUNT
C       DO 301 I = 1, 10
C        WRITE(6,1011) I,TSN(I,1),TSN(I,2),TSN(I,3),TSN(I,4),ISUB1(I,1),
C    *                 ISRC1(I), (IRECS1(I,J,1), J=1,10),
C    *                           (IRECS1(I,J,2), J=1,10),
C    *                           (IRECS1(I,J,3), J=1,10)
C301    CONTINUE
C 1011   FORMAT(I4,4F10.6,I3,I4,10I6,/,51X,10I6,/,51X,10I6)
C
      RETURN
      END
      SUBROUTINE SCPLOD(ISCN)
C-----------------------------------------------------------------------
C   Load record number arrays for the next scan
C
C   Inputs:
C      ISCN    I    Scan number
C
C   Output:
C-----------------------------------------------------------------------
      INCLUDE 'CLINFO.INC'
      INTEGER ISCN
      INTEGER IM, I, M
C-----------------------------------------------------------------------
C
      DO 100 M = 1, MXGANT
      DO 110 I = 1, 3
       JREC(M,I) = 0
 110  CONTINUE
       HFANTS(M) = 0
 100  CONTINUE
C
       IM = 0
      DO 200 M = 1, MXGANT
       IF( (IRECS1(ISCN,M,1) .NE. 0) .AND. (IRECS1(ISCN,M,2) .NE. 0)
     *     .AND. (IRECS1(ISCN,M,3) .NE. 0) )  THEN
        HFANTS(M) = M
         IM = IM + 1
        JREC(IM,1) = IABS(IRECS1(ISCN,M,1))
        JREC(IM,2) = IABS(IRECS1(ISCN,M,2))
        JREC(IM,3) = IABS(IRECS1(ISCN,M,3))
       ENDIF
 200  CONTINUE
C                                       Number of antennas this scan
       IANTS = IM
C
      RETURN
      END
      SUBROUTINE TAGS(LUNIT)
C-----------------------------------------------------------------------
C
C     Reads a file of input time tags. Used when APARM(1) = 2.
C     The file name must be specified via INFILE
C     Added 12 Sept. 1996, D. Gordon, NASA/GSFC
C
C-----------------------------------------------------------------------
      CHARACTER*8 ISRC
      INTEGER IYR, IMON, IDAY, IHR, IMIN, ISEC, IDOY, KDOY
      INTEGER I, LCAL(12), LUNIT
C
      INCLUDE 'TIMETAGS.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA LCAL /0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335/
C
      NHTIME = 0
C
      DO 10 I = 1, 2000
         READ (LUNIT,1010, END=20, ERR=999) IYR, IMON, IDAY,
     *                                      IHR, IMIN, ISEC, ISRC
 1010    FORMAT (6(I2,1X),A8)
C                                       Get day of year number
         IDOY = LCAL(IMON) + IDAY
C                                       remove a day if its not a leap year
         IF ((MOD(IYR,4).NE.0) .AND. (IDOY.GT.59)) IDOY = IDOY-1
C                                       Save first year, month, day, and doy
         IF (I .EQ. 1) THEN
            KYEAR = IYR
            KMONTH = IMON
            KDAY = IDAY
            KDOY = IDOY
         ENDIF
C
         ITAG(I,1) = IHR + (IDOY - KDOY)*24
         ITAG(I,2) = IMIN
         ITAG(I,3) = ISEC
         SRC(I)    = ISRC
         HTIME(I) = ITAG(I,1)/24.0D0 + ITAG(I,2)/1440.0D0
     *            + ITAG(I,3)/86400.0D0
C
 10   CONTINUE
 20   NHTIME = I-1
      GO TO 990
C
 999  CONTINUE
      WRITE (MSGTXT,1020) I
      CALL MSGWRT(4)
C
 990  CONTINUE
      RETURN
 1020 FORMAT ( 'Subroutine TAGS Read Error: I = ',I4)
      END
      SUBROUTINE MK3SC(LUNIT)
C-----------------------------------------------------------------------
C     Reads a Mark III/IV schedule file and computes the observation
C     time tags. The time tag will be the midpoint of the shortest
C     scheduled observation, rounded to the nearest integer second.
C     Used when APARM(1) = 3. The file name of the schedule  must be
C     specified using the INFILE adverb.
C     Added 12 Sept. 1996, D. Gordon, NASA/GSFC
C     Modified 23 Sept. 1996, D. Gordon, NASA/GSFC
C-----------------------------------------------------------------------
      CHARACTER*500 IBUF
      CHARACTER IBF*6, XSRC*8
      CHARACTER STIME*11
      INTEGER LCAL(12), KDOY, LUNIT
      INTEGER I, J, K, IYR, IDOY, IHR, IMIN, ISEC, IMON, IDAY, ICNT,
     *        IFRST, ILAST, IX, IXX, N1, N2, SCRDOY
      REAL    XDUR(20), MINTM
C
      INCLUDE 'TIMETAGS.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA LCAL /31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335,366/
C-----------------------------------------------------------------------
      NHTIME = 0
C
C  Read until '$SKED' found
 38   CONTINUE
      READ(LUNIT, 39, ERR=999, END=999) IBF
 39   FORMAT (A6)
      IF (IBF(1:5) .EQ. '$SKED' ) GO TO 40
      GO TO 38
C
 40   CONTINUE
C
      DO 100 I=1,2000
C
         READ(LUNIT, 1001, END=990, ERR=999) IBUF
         IF( IBUF(1:1) .EQ. '$' ) GO TO 990
C
 1001    FORMAT (A500)
C                                       Get Source name
         XSRC = IBUF(1:8)
C                                       Problem with short source names
          DO 27 J=2,7
           IF (XSRC(J:J).EQ.' ') THEN
             DO 28 K=J+1,8
              XSRC(K:K) = ' '
  28         CONTINUE
            GO TO 29
           ENDIF
  27      CONTINUE
  29      CONTINUE
         SRC(I) = XSRC
C                                       Get start time string
         IXX = 0
         IXX = INDEX(IBUF,'PREOB')
         N1 = IXX + 7
         N2 = N1 + 10
         STIME = IBUF(N1:N2)
C         STIME = IBUF(25:35)
         READ (STIME,1002, ERR=999) IYR, IDOY, IHR, IMIN, ISEC
 1002 FORMAT (I2,I3,3I2)
C
C                                       If first time through, save day
C                                       number and initialize year,
C                                       month, and day of month
      IF (I .EQ. 1) THEN
C                                       make a scratch copy of day of year
         SCRDOY = IDOY
C                                       if its not a leap year, and DOY>59
C                                       add a day [feb now has 29 days!]
C                                       [this test not valid for 2100,2200,2300]
         IF ((MOD(IYR,4).NE.0).AND.(IDOY.GT.59)) SCRDOY = SCRDOY+1

C                                       Get month and day of month
C                                       [now always a leap year]
C        IF( MOD(IYR,4) .NE. 0) THEN
            DO 101 J = 1, 12
               IF (SCRDOY .LE. LCAL(J)) THEN
                  IMON = J
                  IF (J .GT. 1) THEN
                     IDAY = SCRDOY - LCAL(J-1)
                  ELSE
                     IDAY = SCRDOY
                  ENDIF
                  GO TO 42
               ENDIF
 101        CONTINUE
C        ENDIF
C
 42      CONTINUE
         KYEAR = IYR
         KMONTH = IMON
         KDAY = IDAY
         KDOY = IDOY
      ENDIF
C
C  Find scheduled observation durations for this scan, up to 20 stations.
C   Read backwards because the durations are at the end of a long line.
C
      ICNT = 0
      K = 501
C
 55   CONTINUE
C
      K = K-1
      IF (K. LE. 60) GO TO 67
      IF      ( (IBUF(K:K).EQ.'0') .OR. (IBUF(K:K).EQ.'1')
     *     .OR. (IBUF(K:K).EQ.'2') .OR. (IBUF(K:K).EQ.'3')
     *     .OR. (IBUF(K:K).EQ.'4') .OR. (IBUF(K:K).EQ.'5')
     *     .OR. (IBUF(K:K).EQ.'6') .OR. (IBUF(K:K).EQ.'7')
     *     .OR. (IBUF(K:K).EQ.'8') .OR. (IBUF(K:K).EQ.'9') )
     *     GO TO 60
      GO TO 55
C
 60   CONTINUE
      ILAST = K
C
 65   CONTINUE
      K = K-1
      IF     ( (IBUF(K:K).EQ.'0') .OR. (IBUF(K:K).EQ.'1')
     *     .OR. (IBUF(K:K).EQ.'2') .OR. (IBUF(K:K).EQ.'3')
     *     .OR. (IBUF(K:K).EQ.'4') .OR. (IBUF(K:K).EQ.'5')
     *     .OR. (IBUF(K:K).EQ.'6') .OR. (IBUF(K:K).EQ.'7')
     *     .OR. (IBUF(K:K).EQ.'8') .OR. (IBUF(K:K).EQ.'9') )
     *     GO TO 65
C
      IF (IBUF(K:K).EQ.' ') THEN
         IFRST = K+1
         ICNT = ICNT + 1
         READ(IBUF(IFRST:ILAST),*) IX
         XDUR(ICNT) = IX
         GO TO 55
      ENDIF
C
 67   CONTINUE
C                                       We get here after all
C                                       observation durations have been
C                                       accumulated.
C                                       Now find the minimum observation
C                                       time
      MINTM = XDUR(1)
      DO 103 J = 2, ICNT
         IF (XDUR(J) .LT. MINTM) MINTM = XDUR(J)
 103  CONTINUE
C                                       Add half of minimum duration
      ISEC = ISEC + MINTM/2.
C                                       Compute a 'Haystack' Time Tag:
C                                       Round seconds down to the
C                                       nearest multiple of 4 seconds.
C                                       [Turned off. Turn back on if you
C                                       need to match Haystack
C                                       correlator.]
CC     ISEC = ISEC/4
CC     ISEC = ISEC*4
C                                       Increase minutes if necessary
      IF (ISEC .LT. 60) GO TO 110
      IMIN = IMIN + ISEC/60
      ISEC = MOD(ISEC,60)
C                                       Increase hours if necessary
      IF (IMIN .LT. 60) GO TO 110
      IHR = IHR + IMIN/60
      IMIN = MOD(IMIN,60)
C                                       Increase DOY if necessary
      IF (IHR  .LT. 24) GO TO 110
      IDOY = IDOY + IHR/24
      IHR = MOD(IHR,24)
C
 110  CONTINUE
C
      ITAG(I,1) = IHR + (IDOY-KDOY)*24
      ITAG(I,2) = IMIN
      ITAG(I,3) = ISEC
      HTIME(I) = ITAG(I,1)/24.0D0 + ITAG(I,2)/1440.0D0
     *     + ITAG(I,3)/86400.0D0
C
 100  CONTINUE
      GO TO 990
C
 999  CONTINUE
      WRITE (MSGTXT,1020) I
      CALL MSGWRT(4)
 990  CONTINUE
      NHTIME = I - 1
C
C     WRITE (6,1075) KYEAR, KMONTH, KDAY, NHTIME
C1075 FORMAT(' MK3SC: KYEAR, KMONTH, KDAY, NHTIME ', 4I5,/,
C    *       'ITAG1,ITAG2,ITAG3,HTIME,SRC:')
C     DO 1070 I=1,NHTIME
C     WRITE (6,1076) ITAG(I,1), ITAG(I,2), ITAG(I,3), HTIME(I), SRC(I)
C1076 FORMAT(3I5,F12.6,2X,A8)
C1070 CONTINUE
C
      RETURN
 1020 FORMAT ( 'Subroutine MK3SC Read Error: I = ',I4)
      END
      SUBROUTINE TSCAN(LUNIT)
C-----------------------------------------------------------------------
C     Reads a file of scan time ranges produced by Task LISTR using
C     OPTYPE = 'SCAN'. Used when APARM(1) = 4. The file name
C     must be specified using the INFILE adverb.
C     Added 27 Dec. 1996, D. Gordon, NASA/GSFC
C
C  Output to common block TMTGS:
C-----------------------------------------------------------------------
      INCLUDE 'TIMETAGS.INC'
C
      INTEGER ITIM1(4), ITIM2(4)
C     DOUBLE PRECISION TIM1, TIM2, TIMMID, TEMP1, TEMP2, TEMP3
      DOUBLE PRECISION TIM1, TIM2, TIMMID
      CHARACTER*8 ISRC
      INTEGER I, LUNIT
C
      INCLUDE 'INCS:DMSG.INC'
C
      NHTIME = 0
C
      DO 10 I = 1, 2000
         READ (LUNIT, 1010, END=20, ERR=999) ISRC, ITIM1, ITIM2
 1010    FORMAT (5X,A8,27X,4(I2,1X),3X,4(I2,1X)  )
C                                       Blank out year, month, day, and
C                                       day of year
         IF (I .EQ. 1) THEN
            KYEAR  = 0
            KMONTH = 0
            KDAY   = 0
         ENDIF
C                                       Find midpoint time
         TIM1 = ITIM1(1) + ITIM1(2)/24.D0 + ITIM1(3)/1440.D0 +
     *          ITIM1(4)/86400.D0
         TIM2 = ITIM2(1) + ITIM2(2)/24.D0 + ITIM2(3)/1440.D0 +
     *          ITIM2(4)/86400.D0
C                                       Compute midpoint of range
         TIMMID = (TIM1 + TIM2) / 2.D0
C                                       Convert back to integer hours,
C                                       minutes, seconds.
C                                       Hours (0 -> 23 on day 0, 24 ->
C                                       47 on day 1, etc.)
         ITAG(I,1) = TIMMID * 24.0D0
         TIMMID = TIMMID*24.0D0 - ITAG(I,1)
C                                       Minutes
         ITAG(I,2) = TIMMID * 60.0D0
         TIMMID = TIMMID*60.0D0 - ITAG(I,2)
C                                       Seconds
         ITAG(I,3) = TIMMID * 60.0D0 + 0.1D0
C                                       Re-assemble time as well
         HTIME(I) = ITAG(I,1)/24.0D0 + ITAG(I,2)/1440.0D0
     *        + ITAG(I,3)/86400.0D0
C
         SRC(I) = ISRC
C
 10   CONTINUE
 20   NHTIME = I-1
      GO TO 990
C
 999  CONTINUE
      WRITE (MSGTXT,1020) I
      CALL MSGWRT(4)
C
 990  CONTINUE
      RETURN
 1020 FORMAT ( 'Subroutine TSCAN Read Error: I = ',I4)
      END
      SUBROUTINE CLINIT
C-----------------------------------------------------------------------
      INCLUDE 'CLINFO.INC'
      INTEGER I, J
C
        IKOUNT = 0
       DO 200 I=1,1000
          TSN(I,1) = 0.D0
          TSN(I,2) = 0.D0
          TSN(I,3) = 0.D0
          TSN(I,4) = 0.D0
          TSN(I,5) = 0.D0
          ISUB1(I,1) = 0
          ISUB1(I,2) = 0
          ISRC1(I)   = 0
         DO 250 J = 1,MXGANT
           IRECS1(I,J,1) = 0
           IRECS1(I,J,2) = 0
           IRECS1(I,J,3) = 0
 250     CONTINUE
 200   CONTINUE
C
      RETURN
      END
      SUBROUTINE HFINIT (SNTIMC, ANTMBD, ANTSNR, SNINT, ANTPHS,
     *     ANTGAI, AIFSNR, SNTIME, SNDUR, SNMTCH, TIMNO, SOUNO, SNANT,
     *     HFNUM, CURTIM, ANTDLY, ANTCLK, ANTDCK, ANTATM,
     *     ANTDAM, ANTRAT, ANTSBD, ANTPC, ANTPCR, ANTPCA, NTERMS, MAXIF)
C-----------------------------------------------------------------------
C
      INCLUDE 'CLINFO.INC'
C
      INTEGER MAXIF, NTERMS
      REAL ANTMBD(2,*), ANTSNR(2,*), SNINT(*), ANTPHS(MAXIF,2,*),
     *     ANTGAI(MAXIF,2,*), AIFSNR(MAXIF,2,*), ANTPC(MAXIF,2,*),
     *     ANTPCR(MAXIF,2,*), SNDUR, ANTCLK(2,*), ANTDCK(2,*),
     *     ANTPCA(MAXIF,2,*), ANTATM(*), ANTDAM(*), ANTRAT(2,*),
     *     ANTSBD(2,*)
      DOUBLE PRECISION SNTIMC(*), SNTIME, TIMNO, CURTIM(*),
     *  ANTDLY(NTERMS,*)
      INTEGER SNMTCH, SOUNO, SNANT(MXGANT),
     *     HFNUM, I, J, K, NUMIF
C     INCLUDE 'DDCH.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Initialize output
      DO 23 I = 1, MXGANT
         DO 16 J=1,2
            ANTMBD(J,I) = FBLANK
            ANTSNR(J,I) = 0.0
            ANTCLK(J,I) = FBLANK
            ANTDCK(J,I) = FBLANK
            ANTRAT(J,I) = FBLANK
            ANTSBD(J,I) = FBLANK
            DO 17 K = 1,NUMIF
               ANTPHS(K,J,I) = FBLANK
               ANTGAI(K,J,I) = FBLANK
               AIFSNR(K,J,I) = 0.0
               ANTPC(K,J,I) = FBLANK
               ANTPCR(K,J,I) = FBLANK
               ANTPCA(K,J,I) = FBLANK
 17         CONTINUE
 16      CONTINUE
         DO 18 J=1,NTERMS
            ANTDLY(J,I) = DBLANK
 18      CONTINUE
         CURTIM(I) = 0.0D0
         SNTIMC(I) = DBLANK
         SNINT(I)  = FBLANK
         ANTATM(I) = FBLANK
         ANTDAM(I) = FBLANK
 23   CONTINUE
C
C                                       init  arrays
      DO 22  I = 1, MXGANT
         SNANT(I) = 0
 22      CONTINUE
C
      SNTIME = 0.0D0
      SNDUR = 0.0
      SNMTCH = 0
      TIMNO = 0.D0
      SOUNO = 0
      HFNUM = 0
      RETURN
      END
      SUBROUTINE SNGET (SNTAB, NSNROW, NPOLN, NUMIF, FOFF, SUBNO, SNROW,
     *   SOUNO, ANTMBD, ANTRAT, ANTSBD, ANTPHS, ANTGAI, ANTSNR, AIFSNR,
     *   SNTIMC, SNINT, SNANT, IERR)
C-----------------------------------------------------------------------
C   Read actual data times a given time and source from an SN table.
C   Inputs:
C      SNTAB   C*?  Input phase cal table object, should be open.
C      NSNROW  I    Total number of rows.
C      NPOLN   I    Number of polarizations (1 or 2)
C      NUMIF   I    Number of IFs
C      FOFF    D(*) IF frequency offsets from reference frequency.
C      SUBNO   I    Subarray number to process
C   Input/output:
C      SNROW   I    First row in SN table to read.
C   Output:
C      SOUNO   I    Source ID number
C      ANTMBD  R(2,?) Antenna multiband delay residuals 1/poln
C      ANTSBD  R(2,?) Average antenna single band delay 1/poln
C      ANTRAT  R(2,?) Average antenna rate 1/poln
C      ANTPHS  R(?,2,?) Antenna phases at ref freq. 1/IF/poln.
C      ANTGAI  R(?,2,?) Antenna gains at ref freq. 1/IF/poln.
C      ANTSNR  R(2,?) Average antenna SNR 1/poln
C      AIFSNR  R(?,2,?) Average antenna SNR 1/IF/poln.  Also weight.
C      SNTIMC  D(*)   Data center times from SN table per antenna (days)
C      SNINT   R(*)   Data interval from SN table (days)
C      SNANT   I(20) Array of antenna numbers for current scan
C      IERR    I    Return code, 0=>OK, else failed.
C-----------------------------------------------------------------------
      INCLUDE 'CLINFO.INC'
      CHARACTER SNTAB*(*)
      INTEGER NSNROW, NPOLN, NUMIF, IERR, SUBNO, SNROW, SOUNO,
     *        SNANT(MXGANT)
      DOUBLE PRECISION FOFF(*)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DOUBLE PRECISION SNTIMC(*), TWOPI
      REAL SNINT(*), ANTSBD(2,*), ANTRAT(2,*),
     *     ANTGAI(MAXIF,2,*), ANTSNR(2,*), AIFSNR(MAXIF,2,*),
     *     ANTMBD(2,*), ANTPHS(MAXIF,2,*), THRESH
      REAL AVERAG, KVERAG
C     DOUBLE PRECISION TIMEA, TSTART, TSTOP
      DOUBLE PRECISION TIMEA
      REAL      TIMEI, IFR, MBDELY(2), CREAL(2,MAXIF), CIMAG(2,MAXIF),
     *   DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF), DISP(2),
     *   DDISP(2)
      INTEGER  ANTA, SUBA, FREQID, REFA(2,MAXIF), SOUA, IPOLN,
     *   I, IROW, JROW, NODENO
C     INTEGER TYPE, DIM(3)
C     CHARACTER CDUM*8
C
      DATA TWOPI /6.2831853071795865D0/
C-----------------------------------------------------------------------
      IERR = 0
C
C     CALL OGET (SNTAB, 'TSTART', TYPE, DIM, TSTART, CDUM, IERR)
C     IF (IERR.NE.0) GO TO 999
C     CALL OGET (SNTAB, 'TSTOP', TYPE, DIM, TSTOP, CDUM, IERR)
C     IF (IERR.NE.0) GO TO 999
C                                       Read input table accumulating
C                                       info.
      DO 100 JROW = 1, IANTS
C                                       Read data for this row
          IROW = JREC(JROW,1)
          CALL OTABSN (SNTAB, 'READ', IROW, NPOLN, TIMEA, TIMEI, SOUA,
     *       ANTA, SUBA, FREQID, IFR, NODENO, MBDELY, DISP, DDISP,
     *       CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C                                       trap errors
         IF (IERR.LT.0) THEN
            IERR = 0
            GO TO 100
            END IF
         IF (IERR.NE.0) GO TO 999
C                                       trap subarray number
C         print *, ' SNGET: SUBA, SUBNO ', SUBA, SUBNO
         IF ((SUBA.NE.SUBNO).AND.(SUBA*SUBNO.NE.0)) GO TO 100
C
C                                       process SN record
         SNANT(ANTA)  = ANTA
         SNTIMC(ANTA) = TIMEA
         SNINT(ANTA)  = TIMEI
         SOUNO = SOUA
C
C        SNTIME = SNTIME + TIMEA
C        SNDUR = SNDUR + TIMEI
C
         DO 96 IPOLN = 1,2
C                                       Multiband delays
            ANTMBD(IPOLN,ANTA) = MBDELY(IPOLN)
C                                       Don't get SBD's or rates here,
C                                       for dual frequency data they
C                                       will be wrong.
C
C                                       Average IF variable values.
C                                       Phase
            DO 60 I = 1,NUMIF
               IF ((CREAL(IPOLN,I).NE.FBLANK) .AND.
     *              (CIMAG(IPOLN,I).NE.FBLANK)       ) THEN
                  ANTPHS(I,IPOLN,ANTA) = ATAN2(CIMAG(IPOLN,I),
     *                 CREAL(IPOLN,I)+1.0E-20)
     *                 - MBDELY(IPOLN) * (FOFF(I)-FOFF(1)) * TWOPI
                  ANTGAI(I,IPOLN,ANTA) =
     *                 SQRT(CIMAG(IPOLN,I)**2 + CREAL(IPOLN,I)**2)
               ELSE
                  ANTPHS(I,IPOLN,ANTA) = FBLANK
C                 ANTGAI(I,IPOLN,ANTA) = FBLANK
C                                       Blank delay and rate
                  DELAY(IPOLN,I)       = FBLANK
                  RATE(IPOLN,I)        = FBLANK
               END IF
 60         CONTINUE
C                                       save SNRs
            THRESH = 1.0E-10
            ANTSNR(IPOLN,ANTA) = KVERAG(NUMIF,WEIGHT(IPOLN,1),2,THRESH)
            ANTSBD(IPOLN,ANTA) = AVERAG(NUMIF,DELAY(IPOLN,1),2)
            ANTRAT(IPOLN,ANTA) = AVERAG(NUMIF, RATE(IPOLN,1),2)
C
C      IF(IPOLN.EQ.1) THEN
C      write(6,1017) IPOLN, ANTA, SNTIMC(ANTA), ANTSBD(IPOLN,ANTA),
C    *               ANTRAT(IPOLN,ANTA), ANTMBD(IPOLN,ANTA)
C      write(6,1018) (DELAY(IPOLN,KK), KK=1,6)
C      write(6,1019) ( RATE(IPOLN,KK), KK=1,8)
C      ENDIF
C1017  format('SN: IPOL,ANT,TIME,SBD,RATE,MBD: ',I2,I3,F10.7,3E15.6)
C1018  format('Delays: ',8E15.6)
C1019  format(' Rates: ',8E15.6)
C
            DO 80 I = 1,NUMIF
               AIFSNR(I,IPOLN,ANTA) = WEIGHT(IPOLN,I)
 80         CONTINUE
 96      CONTINUE
C
C           write(6,1027) (ANTGAI(I,1,ANTA), I=1,NUMIF)
C1027       FORMAT('SNGET: ANTGAI ',8F10.4)
C                                       get source elevation
 100  CONTINUE
      IERR = 0
C                                       Finished table if we get here
      SNROW = NSNROW + 1
C
 999  CONTINUE
C                                       processed no valid records!
C     IF (SNMTCH.EQ.0) IERR = 1
C     IF (SNMTCH.GT.0) THEN
C        SNTIME = SNTIME / SNMTCH
C        SNDUR = SNDUR / SNMTCH
C        END IF
      RETURN
      END
      SUBROUTINE CLGET (MOTAB, SUBNO, SOUNO, CURTIM, INTER,
     *   NTERMS, NPOLN, NUMIF, ANTMAX, ANTDLY, ANTCLK,
     *   ANTDCK, ANTATM, ANTDAM, ELEV, IERR,
     *   ANTRAT, ANTSBD, ANTGAI, ANTSNR, AIFSNR )
C-----------------------------------------------------------------------
C   Read set of CL table info for next time
C   Inputs:
C      MOTAB   C*?  Input CL table object, should be open.
C      SUBNO   I    subarray number
C      SOUNO   I    source id
C      NTERMS  I    Number of terms in delay polynomial
C      NPOLN   I    Number of polarizations (1 or 2)
C      NUMIF   I    Number of IFs
C   Input/output:
C      ANTMAX  I    Maximum antenna number
C   Output:
C      CURTIM  D    Time of CL table values
C      INTER   R    Time interval of corresponding values.
C      ANTMAX  I    Maximum antenna number
C      ANTDLY  D(nterms,?) Antenna delay polynomials;
C                       If blanked then there is no data for this
C                       antenna.
C      ANTCLK  R(2,?) Antenna "clock" error 1/poln
C      ANTDCK  R(2,?) Antenna "clock" error rate 1/poln
C      ANTATM  R(?)   Antenna "atmosphere" error. (sec)
C      ANTDAM  R(?)   Antenna "atmosphere" error rate. (sec/sec)
C ???  IERR    I    Return code, -1 => data all processed, 0=>OK,
C                   else failed.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
C     INCLUDE 'INCS:DIMV.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'CLINFO.INC'
C
      CHARACTER MOTAB*(*)
      INTEGER SUBNO, SOUNO
      INTEGER NTERMS, NPOLN, NUMIF, ANTMAX, IERR
      DOUBLE PRECISION CURTIM(*), ANTDLY(NTERMS,*)
      REAL    INTER, ANTCLK(2,*), ANTDCK(2,*), ANTATM(*), ANTDAM(*)
      DOUBLE PRECISION TIMEA, GEODLY(9)
      REAL    TIMEI, IFRC, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2), CREAL(2,MAXIF),
     *   CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF),
     *   ANTRAT(2,*), AVERAG, ANTSBD(2,*), ELEV(*),
     *   ANTGAI(MAXIF,2,*), ANTSNR(2,*), AIFSNR(MAXIF,2,*), SNR(MAXIF),
     *   HA, AZ
      INTEGER  SOUA, ANTA, SUBA, FREQID, REFA(2,MAXIF), I, J,
     *   IROW, JROW
C-----------------------------------------------------------------------
C                                       Initialize output
      ANTMAX = 1
C                                       Read input table accumulating info.
      DO 100 JROW = 1, IANTS
C                                       skip this row?
         IROW = JREC(JROW,2)

         CALL OTABCL (MOTAB, 'READ', IROW, NPOLN, NUMIF, TIMEA,
     *        TIMEI, SOUA, ANTA, SUBA, FREQID, IFRC, GEODLY, DOPOFF,
     *        ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP, DDISP, CREAL,
     *        CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C                                       trap errors
C        IF (IERR.LT.0) GO TO 100
C        IF (IERR.NE.0) GO TO 999
C                                       trap subarray
C        IF ((SUBA.NE.SUBNO).AND.(SUBNO*SUBA.NE.0)) GO TO 100
C                                       trap source
C        IF (SOUA.NE.SOUNO) GO TO 100
C                                       warn if CL times are not identical
C        IF (CURTIM .NE. 0.D0) THEN
C           IF (DABS(CURTIM - TIMEA) .GT. 1.D-14) THEN
C              WRITE(MSGTXT,1098) (IROW-1), SOUA, ANTA
C              CALL MSGWRT(4)
C              WRITE(MSGTXT,1099) CURTIM, TIMEA
C              CALL MSGWRT(4)
C 1098 FORMAT('CLGET mismatch: Row ',I6,'SOURID ',I3,'ANTNO',I3)
C 1099 FORMAT('CURTIM, TIMEC = ',2E28.20)
C           END IF
C        END IF
C
         CURTIM(ANTA) = TIMEA
         INTER = TIMEI
         ANTMAX = MAX (ANTMAX, ANTA)
C                                       Get Geometric delay terms
         DO 50 I = 1,NTERMS
            ANTDLY(I,ANTA) = GEODLY(I)
 50      CONTINUE
C                                       Atmospheric terms
         ANTATM(ANTA) = ATMOS
         ANTDAM(ANTA) = DATMOS
C                                        SNR's (Not used - these overwrite
C                                        [SN table SNRs why?]
         DO 80 J=1,2
C                                       Clock terms
            ANTCLK(J,ANTA) =  CLOCK(J)
            ANTDCK(J,ANTA) = DCLOCK(J)
C                                        Rate
            ANTRAT(J,ANTA) = AVERAG (NUMIF,  RATE(J,1), 2)
            ANTSBD(J,ANTA) = AVERAG (NUMIF, DELAY(J,1), 2)
C
C      IF(J.EQ.1) THEN
C      write(6,1017) J, ANTA, CURTIM(ANTA), ANTSBD(J,ANTA),
C    *               ANTRAT(J,ANTA), MBDELY(J)
C      write(6,1018) (DELAY(J,KK), KK=1,8)
C      write(6,1019) ( RATE(J,KK), KK=1,8)
C      ENDIF
C1017  format('CL: IPOL,ANT,TIME,SBD,RATE,MBD: ',I2,I3,F10.7,3E15.6)
C1018  format('Delays: ',8E15.6)
C1019  format(' Rates: ',8E15.6)
C
            DO 90 I = 1,NUMIF
               IF (WEIGHT(J,I).GT.1.0E-10) THEN
                  SNR(I) = WEIGHT(J,I)
               ELSE
                  SNR(I) = FBLANK
               END IF
               AIFSNR(I,J,ANTA) = WEIGHT(J,I)
 90         CONTINUE
            ANTSNR(J,ANTA) = AVERAG (NUMIF, SNR, 1)
C
C                                       Gain
            DO 60 I = 1,NUMIF
               IF ( (CREAL(J,I).NE.FBLANK) .AND.
     *              (CIMAG(J,I).NE.FBLANK) ) THEN
                  ANTGAI(I,J,ANTA) =
     *                 SQRT(CIMAG(J,I)**2 + CREAL(J,I)**2)
               ELSE
                  ANTGAI(I,J,ANTA) = FBLANK
               END IF
 60         CONTINUE
C
 80      CONTINUE
C
C                                       get source elevation
         CALL OSUELV (MOTAB, ANTA, SUBNO, SOUNO, CURTIM(ANTA),
     *      HA, ELEV(ANTA), AZ, IERR)
         IF (IERR.NE.0) GO TO 999
 100  CONTINUE
      IERR = 0
 999  RETURN
      END
      SUBROUTINE PCGET (PCTAB, SUBNO, NPOLN, NUMIF, ANTPC, ANTPCR,
     *   ANTPCA, IERR )
C-----------------------------------------------------------------------
C   Read set of Phase cals for a given time and source.
C   Currently reads values from a CL table.
C   Inputs:
C      PCTAB   C*?  Input phase cal table object, should be open.
C      SUBNO   I    SUBARRAY number
C      NPOLN   I    Number of polarizations (1 or 2)
C      NUMIF   I    Number of IFs
C   Input/output:
C   Output:
C      ANTPC   R(?,2,?) Antenna phase cals  1/IF/poln. (rad)
C      ANTPCR  R(?,2,?) Antenna phase cal rates  1/IF/poln. (rad/s)
C      ANTPCA  R(?,2,?) Antenna phase cal amplitudes 1/IF/poln. (rad/s)
C      IERR    I    Return code, -1 => data all processed, 0=>OK,
C                   else failed.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'CLINFO.INC'
      CHARACTER PCTAB*(*)
      INTEGER   SUBNO, NPOLN, NUMIF, IERR
      REAL      ANTPC(MAXIF,2,*), ANTPCR(MAXIF,2,*), ANTPCA(MAXIF,2,*)
      DOUBLE PRECISION TIME, GEODLY(9)
      REAL      TIMEI, IFR, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2), CREAL(2,MAXIF),
     *   CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF)
      INTEGER SOUA, SUBA
      INTEGER  ANTA, FQA, REFA(2,MAXIF), I, J, JROW, IROW
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Read input table accumulating info.
      DO 100 JROW = 1, IANTS
C                                       Read data for this row
         IROW = JREC(JROW,3)
         CALL OTABCL (PCTAB, 'READ', IROW, NPOLN, NUMIF, TIME,
     *        TIMEI, SOUA, ANTA, SUBA, FQA, IFR, GEODLY, DOPOFF,
     *        ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP, DDISP, CREAL,
     *        CIMAG, DELAY, RATE, WEIGHT, REFA, IERR)
C                                       trap errors
C         IF (IERR.LT.0) GO TO 100
C         IF (IERR.NE.0) GO TO 999
C                                       match subarrays
         IF ((SUBA.NE.SUBNO).AND.(SUBA*SUBNO.NE.0)) GO TO 100
C
C                                       match source ids
C                                       IF (SOUA.NE.SOUNO) GO TO 100
C         IF (SOUA.NE.SOUNA) GO TO 100
C
         DO 70 J=1,NPOLN
C                                       Phase
            DO 60 I = 1,NUMIF
               IF ((CREAL(J,I).NE.FBLANK).AND.(CIMAG(J,I).NE.FBLANK))
     *              THEN
                  ANTPC(I,J,ANTA)=ATAN2(CIMAG(J,I),CREAL(J,I)+1.E-20)
C                                       No rate info
                  ANTPCR(I,J,ANTA) = 0.0
                  ANTPCA(I,J,ANTA) =
     *                 SQRT(CIMAG(J,I)**2 + CREAL(J,I)**2)
               ELSE
                  ANTPC(I,J,ANTA) = FBLANK
                  ANTPCR(I,J,ANTA) = FBLANK
                  ANTPCA(I,J,ANTA) = FBLANK
               ENDIF
 60         CONTINUE
 70      CONTINUE
 100  CONTINUE
      IERR = 0
 999  RETURN
      END
      REAL FUNCTION AVERAG (N, ARRAY, INC)
C-----------------------------------------------------------------------
C   Returns the average of the valid values of a magic value blanked
C   array.   Returns FBLANK if no valid data.
C   Inputs:
C      N       I    Number of elements in ARRAY to process (this is
C                   unaffected by INC).
C      ARRAY   R(*) Array of values
C      INC     I    Stride in ARRAY for access
C   Output:
C      AVERAG  R    Average value or FBLANK if no good data.
C-----------------------------------------------------------------------
      INTEGER   N, INC
      REAL      ARRAY(*)
C
      INTEGER   I, J, COUNT
      REAL      SUM
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      AVERAG = FBLANK
      IF (N.LE.0) GO TO 999
      COUNT = 0
      SUM = 0.0
      J = 1 - INC
      DO 100 I = 1,N
         J = J + INC
         IF (ARRAY(J).NE.FBLANK) THEN
            COUNT = COUNT + 1
            SUM = SUM + ARRAY(J)
            END IF
 100     CONTINUE
      IF (COUNT.GT.0) AVERAG = SUM / COUNT
C
 999  RETURN
      END
      REAL FUNCTION KVERAG (N, ARRAY, INC, THRESH)
C-----------------------------------------------------------------------
C   Returns the average of the valid values of a magic value blanked
C   array.   Returns FBLANK if no valid data.
C   Inputs:
C      N       I    Number of elements in ARRAY to process (this is
C                   unaffected by INC).
C      ARRAY   R(*) Array of values
C      INC     I    Stride in ARRAY for access
C      TRESH   R    Threshold value to exclude from average
C   Output:
C      AVERAG  R    Average value or FBLANK if no good data.
C-----------------------------------------------------------------------
      INTEGER   N, INC
      REAL      ARRAY(*), THRESH
C
      INTEGER   I, J, COUNT
      REAL      SUM
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      KVERAG = FBLANK
      IF (N.LE.0) GO TO 999
      COUNT = 0
      SUM = 0.0
      J = 1 - INC
      DO 100 I = 1,N
         J = J + INC
         IF ((ARRAY(J).NE.FBLANK).AND.(ARRAY(J).GT.THRESH)) THEN
            COUNT = COUNT + 1
            SUM = SUM + ARRAY(J)
            END IF
 100     CONTINUE
      IF (COUNT.GT.0) KVERAG = SUM / COUNT
C
 999  RETURN
      END
      SUBROUTINE HFPUT (UVDATA, HFTAB, HFROW, CURTIM, TTAG, IC2, INTER,
     *   NTERMS, NPOLN, ANTMAX, ANTDLY, ANTCLK, ANTDCK, ANTATM, ANTDAM,
     *   ANTMBD, ANTSBD, ANTRAT, ANTPHS, ANTGAI, ANTPC, ANTPCR, ANTPCA,
     *   ANTSNR, AIFSNR, ELEV, SNAME, ANAME, OBSDAT, REFREQ,
     *   NUMIF, FOFF, TBW, SNTIMC, SNINT, DATUTC, NZERO,
     *   IC3, IERR )
C-----------------------------------------------------------------------
C   Write HF table entries for this time stamp.  Do both polarizations
C   if present.
C
C   Inputs:
C      UVDATA  C*?  Name of input uv data
C      HFTAB   C*?  Output HF table object
C      NTERMS  I    Number of terms in delay polynomial
C      NPOLN   I    Number of polarizations (1 or 2)
C      ANTMAX  I    Maximum antenna number
C      CURTIM  D?   Time of CL table values as UTC days
C      INTER   R    Time interval of corresponding values.
C      ANTDLY  D(nterms,?) Antenna delay polynomials;
C                   If blanked then there is no data for this antenna.
C      ANTCLK  R(2,?) Antenna "clock" error 1/poln (sec)
C      ANTDCK  R(2,?) Antenna "clock" error rate 1/poln (sec/sec)
C      ANTATM  R(?)   Antenna "atmosphere" error. (sec)
C      ANTDAM  R(?)   Antenna "atmosphere" error rate. (sec/sec)
C      ANTMBD  R(2,?) Antenna multiband delay residuals 1/poln
C      ANTSBD  R(2,?) Average antenna single band delay 1/poln
C      ANTRAT  R(2,?) Average antenna rate 1/poln
C      ANTPHS  R(?,2,?) Antenna phase 1/IF/poln. from SN table
C      ANTGAI  R(?,2,?) Antenna gains 1/IF/poln. as correction.
C      ANTPC   R(?,2,?) Phase cal. phase 1/IF/poln. as correction (rad)
C      ANTPCR  R(?,2,?) Phase cal rate 1/IF/poln. as correction. (rad/s)
C      ANTPCA  R(?,2,?) Phase cal amplitude  1/IF/poln. (units?)
C      ANTSNR  R(2,?) Average antenna SNR 1/poln
C      AIFSNR  R(?,2,?) Average antenna SNR 1/IF/poln.  Also weight
C      ELEV    R(?)   Source elevations at each antenna
C      SNAME   C*16   Source name (only 1st 8 written to HF)
C      ANAME   C(*)*8 Antenna names
C      OBSDAT  C*8    Observing reference date (dd/mm/yy)
C      REFREQ  D      Reference frequency (Hz)
C      NUMIF   I      Number of IFs
C      FOFF    D(*)   IF frequency offsets from REFREQ (Hz)
C      BW      R      IF bandwidth (Hz)
C      SNTIMC  D(*)   Data center times from SN table per antenna (days)
C      SNINT   R(*)   Data interval from SN table (days)
C      DATUTC  R      Data time-UTC (time sec.) used for selecting vis
C                     data.
C   Input/output:
C      HFROW   I    Next HF row to write
C   Output:
C      NZERO   I    Number with Zero delays
C      IERR    I    Return code, -1 => data all processed, 0=>OK, else
C                                failed.
C-----------------------------------------------------------------------
C
C  A 'brief' explanation of the various times and time offsets used in
C  HFPUT.
C
C Times:
C   CURTIM(N)  =  UTC time in the CL table for the current scan. (days)
C
C   SNTIMC(N)  =  UTC time for station N in the SN table for the current
C                  scan. (days)
C
C   TTAG       =  UTC time of the Time Tag chosen for the current scan.
C                 (days)
C
C   CEPTIM     =  UTC time of the Central Epoch for the current scan.
C                 (days)
C
C Offsets:
C   -TSOFF     =  Offset of the UTC Time tag from the CL table time,
C                  measured at the Earth center clock. (seconds)
C   +TEETAU    =  Offset of the UTC Time tag from the CL table time,
C                  measured at the Reference station clock. (seconds)
C
C   -TSNOFF    =  Offset of the UTC Time tag from the SN table time,
C                  measured at the Earth center clock. (seconds)
C                 = -TSOFF + CLSN
C   +TSNTG     =  Offset of the UTC Time tag from the SN table time,
C                  measured at the Reference station clock. (seconds)
C                 = +TEETAU + CLSN
C
C   -EPOFF     =  Offset of the Central Epoch from the CL table time,
C                  measured at the Earth center clock. (seconds)
C   +TCOFF     =  Offset of the Central Epoch from the CL table time,
C                  measured at the Reference station clock. (seconds)
C
C   -SNCO      =  Offset of the Central Epoch from the SN table time,
C                  measured at the Earth center clock. (seconds)
C   +TSNO      =  Offset of the Central Epoch from the SN table time,
C                  measured at the Reference station clock. (seconds)
C
C-----------------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'CLINFO.INC'
C
      CHARACTER UVDATA*(*), HFTAB*(*), SNAME*16, ANAME(*)*8, OBSDAT*8
      INTEGER HFROW, NTERMS, NPOLN, ANTMAX, NUMIF, NZERO, IERR
      DOUBLE PRECISION CURTIM(*), ANTDLY(NTERMS,*), REFREQ,
     *   FOFF(*), SNTIMC(*)
      REAL    INTER, ANTCLK(2,*), ANTDCK(2,*), ANTATM(*), ANTDAM(*),
     *   ANTMBD(2,*), ANTSBD(2,*), ANTRAT(2,*), ANTPHS(MAXIF,2,*),
     *   ANTGAI(MAXIF,2,*), ANTPC(MAXIF,2,*), ANTPCR(MAXIF,2,*),
     *   ANTPCA(MAXIF,2,*), ANTSNR(2,*), AIFSNR(MAXIF,2,*), ELEV(*),
     *   BW, SNINT(*), DATUTC, TBW(*), BWP
      CHARACTER CC1*2, CC25*8, CC26*8, CC27*8, CC28*6, CC29*8, CC30*8,
     *   CC31*6, CC32*8, CC33*1, CC34*2, CC35*6, CC36*6, CC37*8, CC38*8
      INTEGER   IC2(6), IC3(6), IC4(4), IC5, IC6, IC7, IC8, IC9(28),
     *   IC10(56), IC11, IC12(6), IC13, IC14, IC15, IC16, IC17,
     *   IC18(28), IC19(84), IC20(28), IC21(56), IC22(28), IC23, IC24
      REAL  RC51(28), RC52(2), RC53, RC54, RC55, RC56, RC57, RC58,
     *   RC59(2), RC60(2), RC61, RC62(2), RC63(6), RC64, RC65, RC66,
     *   RC67, RC68, RC69, RC70, RC71, RC72, RC73, RC74, RC75, RC76,
     *   RC77, XC73
      DOUBLE PRECISION DC39(14), DC40, DC41, DC42, DC43, DC44, DC45,
     *   DC46, DC47, DC48, DC49, DC50
      INTEGER   REF, REM, IPOL, I, NREC, NN, IQN
      DOUBLE PRECISION TCOFF, DPOLY, DERPOL, DR2POL, TSOFF,
     *   DLY1, DLY2, RAT1, RAT2, FRQFAC, TWOPI, MLDLY1, MLDLY2, MLRAT1,
     *   MLRAT2, MLPHS1, MLPHS2, MCDLY1, MCDLY2, MCRAT1, MCRAT2, MCPHS1,
     *   MCPHS2, TDLY, TPHS, PHS1, PHS2, PD1, PD2, MBDAMB, PDIFF,
     *   EPOFF, MBRES, MBAMB, TDP, DIFPCE, SNCO, FSHIFT, CLDIF, DTEMP,
     *   RRAT1, RRAT2, ORES1, ORES2, DORES1, DORES2
C     REAL SIGSBD
      REAL RESPHS, DIFPHS, DIFPH1
      REAL TIMISN, ELIM, FRACT
      REAL REFPHS, REMPHS, AVGPHS, MBERR
      REAL DUM57
      DOUBLE PRECISION  TIMSN, NREFQ, TTAG, MBRES1, MBRES2, CLSN,
     *   TSNOFF, T1, T2, BWRMS, CEPTIM, TSNTG, TCSN0, EPOCP1,TCOFP1,
     *   TCSNP1, EPOCM1, TCOFM1, TCSNM1
      DOUBLE PRECISION  TSOF2, MLDLY, MLRAT, MLACC1, MLACC2, TSOF1,
     *   MTAU1, MTAU2, DMTAU1, DMTAU2, MLDLG1, MLDLG2, MLRTG1,
     *   MLRTG2, SBRES1, SBRES2, SBREG1, SBREG2, XTAU1, DXTAU1
C     DOUBLE PRECISION  TEETA2, T21, T23, T13, TEETAU, TKLOFF, TKLOHF,
C    *   DR3POL
      INTEGER  MAXAN, MXIF, MINSMP, FIRST
C  MAXAN = largest antenna number expected.
      PARAMETER (MAXAN=20)
C  MXIF = maximum number of IF's.
      PARAMETER (MXIF=16)
      INTEGER   QUAL, MAXSMP, LIF, LIFNO(MXIF)
      REAL      TIMES(2), VIS(2,MXIF,MAXAN,MAXAN),
     *   AMPFRC(2,MAXAN,MAXAN), UVAVG(2,MAXAN,MAXAN), SCLAMP, COHER
C     REAL CLK1, CLK2
      REAL      APARM(10), DPARM(10)
      COMMON /APRM/ APARM, DPARM
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C
C  MINSMP = minimum number of time samples acceptable.
      DATA NREC /0/
      DATA FIRST /1/
      DATA MINSMP /3/
      DATA TWOPI /6.2831853071795865D0/
C-----------------------------------------------------------------------
C
C  Get visibility data averages
      QUAL = -1
      BW = TBW(1)
C                                       Correct IF bandwidth for any
C                                       channels dropped in FRING'ing
      BWP = BW * DPARM(1)/DPARM(2)
C
C                                       Get SN time range
      TIMSN = 0.0D0
      DO 20 I = 1, MXGANT
         IF (SNTIMC(I).NE.DBLANK) THEN
            TIMSN = SNTIMC(I)
            TIMISN = SNINT(I)
            END IF
 20      CONTINUE
C     IF (TIMSN .EQ. 0.0D0) THEN
C        TIMSN = CURTIM
C        TIMISN = INTER
C        END IF
      TIMES(1) = TIMSN - 0.5 * TIMISN + DATUTC/86400.D0
      TIMES(2) = TIMSN + 0.5 * TIMISN + DATUTC/86400.D0
C
C                                       Fill VIS array.
      CALL SCNHAY (UVDATA, SNAME, QUAL, TIMES, MAXAN, MXIF, VIS,
     *   AMPFRC, UVAVG, MAXSMP, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Is this data all bad?
      IF (MAXSMP.LT.MINSMP) GO TO 999
C                                       Initialize values
C
C                                       Source name
      CC25 = SNAME(1:8)
C                                       CORREL output file
      CC28 = '      '
C                                       Tape labels
      CC29 = 'unknown'
      CC30 = 'unknown'
C                                       FRING version
      CC31 = 'AIPS  '
C                                       Run code ('123-1234')
      CALL RUNCOD (OBSDAT, TIMSN - TIMISN*0.5D0, CC32)
C                                       Original CORREL file name
      CC35 = '      '
C                                       Tape Q code
      CC36 = '      '
C
      CALL FILL (4, 0, IC4)
C                                       Sampling rate
      IC5 = 2.0 * BW * 1.0E-3 + 0.5
      IC6 = 0
      IC7 = 0
      CALL FILL (28, 0, IC9)
      CALL FILL (56, 0, IC10)
      IC11 = 0
      IC13 = 0
      IC14 = 0
      IC15 = 0
      IC16 = 0
      IC17 = 0
C  CORELXINT
      CALL FILL (28, 0, IC18)
C  PROCUTC
      CALL FILL (28, 0, IC20)
C  ERRORATE
      CALL FILL (56, 0, IC21)
C  INDEX
      CALL FILL (28, 0, IC22)
      IC23 = 0
      IC24 = 0
C  Rate derivatives mHz/asec
      CALL RFILL (2, 0.0, RC62)
C                                       Search parameters
      CALL RFILL (6, 0.0, RC63)
C                                       Probability of false detection
      RC66 = 0.0
C                                       Loop over baselines
      DO 510 REF = 1, ANTMAX-1
        IF ( HFANTS(REF) .EQ. 0 ) GO TO 510
C
          IF (ANTDLY(1,REF).EQ.DBLANK) GO TO 510
          IF (SNTIMC(REF).EQ.DBLANK) GO TO 510
          IF (ANTDLY(1,REF).EQ.0.0D0) THEN
            NZERO = NZERO + 1
            GO TO 510
          ENDIF
C
         DO 500 REM = REF+1, ANTMAX
            IF ( HFANTS(REM) .EQ. 0 ) GO TO 500
            IF (ANTDLY(1,REM).EQ.DBLANK) GO TO 500
            IF (SNTIMC(REM).EQ.DBLANK) GO TO 500
            IF (ANTDLY(1,REM).EQ.0.0D0) THEN
               NZERO = NZERO + 1
               GO TO 500
             ENDIF
C
C                                       Loop over polarization
            DO 490 IPOL = 1,NPOLN
C                                       Ignore bad values
               IF ((ANTSNR(IPOL,REF).EQ.FBLANK) .OR.
     *            (ANTSNR(IPOL,REM).EQ.FBLANK) .OR.
     *            (ANTMBD(IPOL,REF).EQ.FBLANK) .OR.
     *            (ANTMBD(IPOL,REM).EQ.FBLANK)) GO TO 490
C  Check average amplitude
               IF (AMPFRC(1,REF,REM) .LE. 0.0) GO TO 490
C
C                                       Get offsets of Time Tag from
C                                       CL times (seconds).
       TSOFF      = (CURTIM(REF) - TTAG) * 86400.D0
       CLDIF = (CURTIM(REM) - CURTIM(REF)) * 86400.D0
C
C                                       Source elevations
        RC60(1) = ELEV(REF) * 57.29577951
        RC60(2) = ELEV(REM) * 57.29577951
        ELIM = APARM(2)
C                                       Skip if elev less than APARM(2)
C                                       degrees, but don't check if
C                                       APARM(2) .le. 0 degrees,
C                                       allows negative elevations
C                                       for satellite VLBI.
              IF (ELIM .GT. 0) THEN
               IF (RC60(1) .LE. ELIM) THEN
                  WRITE (MSGTXT,1450) REF, CC32, RC60(1), ELIM
 1450             FORMAT ('Antenna ',I2,' at ',A8,' at ',F4.1,
     *              ' degrees! Below elev limit of ', F4.1,' degrees.')
                  CALL MSGWRT(4)
                  GO TO 490
               ENDIF
C
               IF (RC60(2) .LE. ELIM) THEN
                  WRITE (MSGTXT,1450) REM, CC32, RC60(2), ELIM
                  CALL MSGWRT(4)
                  GO TO 490
               ENDIF
              ENDIF
C                                       Get number of IF's used
               LIF = 0
               DO 450 I = 1, NUMIF
                  IF ((AIFSNR(I,IPOL,REF) .LE. 0.0) .OR.
     *                (AIFSNR(I,IPOL,REM) .LE. 0.0)) GO TO 450
                  LIF = LIF + 1
                  LIFNO(LIF) = I
 450           CONTINUE
               IF (LIF .EQ. 0) GO TO 490
C                                       Reference frequency:
C                                       Using AN table frequency
C                                       [Users should now use 'RUN REFREQ'
C                                        setting adverb OFFSET to change
C                                        reference pixel and FQ table offsets]
               NREFQ = REFREQ + FOFF(LIFNO(1))
C#######################################################################
C                                       Frequency shift to move phase
C                                       residuals to frequency NREFQ
C                                       [Should now be zero. Code left in
C                                       for possible future comparison
C                                       experiments.]
C   RDV22 Comparisons !!!!!!!!!!!!!!!!!!
C##            FSHIFT = -4.0D6
C##            NREFQ = NREFQ + FSHIFT
C#######################################################################
               FSHIFT = 0.0D0
               FRQFAC = NREFQ * TWOPI
               CALL FRECOD (NREFQ, CC34)
C                                       Number of "channels"
               IC8 = LIF
C                                       Frequency table
        DO 460 I = 1,LIF
          DC39(I) = (REFREQ + FOFF(LIFNO(I)) ) * 1.0D-6
 460    CONTINUE
C                                       Reference frequency
        DC40 = NREFQ * 1.0D-6
C                                       Print out if first
               IF (FIRST .EQ. 1) THEN
                  FIRST = 0
                  WRITE (MSGTXT, 1470) LIF, DC40
 1470             FORMAT ('Number of good IFs ', I2,
     *               ':  Ref Freq (MHz)= ', F13.6)
                  CALL MSGWRT(4)
                  DO 470 I = 1, LIF
                     WRITE (MSGTXT, 1472) I, DC39(I)
 1472                FORMAT ('FREQ ', I2,':', F13.6)
                     CALL MSGWRT(4)
 470              CONTINUE
               ENDIF
C                                       Fill in proper freqs etc.
C
C                                       CALBYFRQ (Phase cals)
               CALL FILLPC (LIF, LIFNO, ANTPC(1,IPOL,REF),
     *              ANTPC(1,IPOL,REM), ANTPCA(1,IPOL,REF),
     *              ANTPCA(1,IPOL,REM), IC19)
C                                       Antenna names
          CC26 = ANAME(REF)
          CC27 = ANAME(REM)
C                                       Antenna codes and site occupation codes
          CALL XANCOD(CC26, CC27, CC1, CC37, CC38)
C                                       Quality code and ratio min/max
C                                       accepted samples
          CALL QCODE (LIF, LIFNO, VIS(1,1,REM,REF), MAXSMP, MINSMP,
     *         FRACT, CC33, RC75)
C                                       SN table offset from CL table (seconds)
C      CLSN = ( CURTIM(REF) - SNTIMC(REF) ) * 86400.D0
       CLSN = ( CURTIM(REF) - SNTIMC(REM) ) * 86400.D0
C                                       Offset of UTC Time Tag from SN
C                                       table time (seconds)
C      TSNOFF = TSOFF - CLSN
       TSNOFF = (SNTIMC(REM) - TTAG) * 86400.D0
C
C                                      Get the correction to shift from
C                                      center-of-Earth clock to the
C                                      reference station clock at the
C                                      UTC Time Tag (TTAG). [AIPS table
C                                      times are true Earth centered times.
C                                      We want 'Haystack' or reference
C                                      station labeled arrival times. The
C                                      difference is the reference station
C                                      delay at the UTC Time tag, the
C                                      delay from the reference station to
C                                      the center of the Earth.]
C
C                                      Get delays at epoch time represented
C                                      by TSOFF (offset from the CL table
C                                      time) as measured at the reference
C                                      station.
C                                      Compute Haystack frame offset
C*     CALL HFREF (REF, REM, NTERMS, IPOL, ANTDLY, ANTCLK,
C*   *             ANTDCK, ANTATM, ANTDAM, TSOFF, TEETAU )
C
C                                      TEETAU is the offset from the CL table
C                                      time to the observation Time Tag
C                                      corrected to the reference station.
C                                      -Use TEETAU for model values at the
C                                      UTC Time Tag.
C
C                                      Define TSNTG as the offset of the
C                                      UTC Time Tag to the SN table time,
C                                      corrected to the ref. station clock.
C                                      -Use TSNTG for residual values at
C                                      the UTC Time Tag.
C
C                                      Compute observed quantities at UTC
C                                      Time Tag, TTAG
C
C                                      Total Model delays (sec)
C*          TEETA2 = TEETAU - CLDIF
C*          TSNTG = TEETAU + CLSN
C*        MLDLY1 = DPOLY( NTERMS, ANTDLY(1,REF), TEETAU)
C*        MLDLY2 = DPOLY( NTERMS, ANTDLY(1,REM), (TEETAU-CLDIF) )
C         MLDLY2 = DPOLY( NTERMS, ANTDLY(1,REM), TEETA2 )
C       print *,'old: MLDLY1, MLDLY2 ', MLDLY1*1.D12, MLDLY2*1.D12
C
C                                      Total Model rates (sec/sec)
C*        MLRAT1 = DERPOL( NTERMS, ANTDLY(1,REF), TEETAU)
C*        MLRAT2 = DERPOL( NTERMS, ANTDLY(1,REM), (TEETAU-CLDIF) )
C       print *,'old: MLRAT1, MLRAT2 ', MLRAT1*1.D12, MLRAT2*1.D12
C
C                                      Total model phases (rad)
C*        MLPHS1 = MLDLY1 * NREFQ * TWOPI
C*          MLPHS1 = DMOD (MLPHS1, TWOPI)
C*        MLPHS2 = MLDLY2 * NREFQ * TWOPI
C*          MLPHS2 = DMOD (MLPHS2, TWOPI)
C       print *,'old: MLPHS1, MLPHS2 ', MLPHS1*57.29578, MLPHS2*57.29578
C
C                                      Multiband delay residuals at the
C                                      SN table time.
C*        MBRES1 = ANTMBD(IPOL,REF) + ANTRAT(IPOL,REF)*(TSNTG)
C*        MBRES2 = ANTMBD(IPOL,REM) + ANTRAT(IPOL,REM)*(TSNTG)
C       print *,'old: MBRES1, MBRES2 ', MBRES1*1.D12, MBRES2*1.D12
C
C                                      Multiband ambiguity spacing
          MBAMB = MBDAMB (NUMIF, FOFF, AIFSNR(1,IPOL,REF),
     *            AIFSNR(1,IPOL,REM))
C                                      Total Multiband (group) delay (usec)
C                                       (Model + Residual)
C                                      Total MBD residual
C*        MBRES = MBRES2 - MBRES1
C       print *,'old: MBRES          ', MBRES*1.D12
C                                      Total Group Delay (microsec)
C*        DC41 = (MLDLY2 - MLDLY1 - MBRES) * 1.0D6
C       print *,'old: DC41 (psec) ', DC41*1.D6
C
C                                      Total delay rate (microsec/sec)
C*        RAT1 = MLRAT1 - ANTRAT(IPOL,REF)
C*        RAT2 = MLRAT2 - ANTRAT(IPOL,REM)
C       print *,'old: RAT1, RAT2     ', RAT1*1.D12, RAT2*1.D12
C                                      Add together and correct for Doppler
C*        DC42 = (RAT2 - RAT1) / (1.0D0 + RAT1) * 1.0D6
C       print *,'old: DC42 (ps/s) ', DC42*1.D6
C
C                                      Single band delay (usec)
C                                       (Model + Residual)
C*       DLY1 = MLDLY1 - (ANTSBD(IPOL,REF) + ANTRAT(IPOL,REF)*(TSNTG))
C*       DLY2 = MLDLY2 - (ANTSBD(IPOL,REM) + ANTRAT(IPOL,REM)*(TSNTG))
C*       DC43 = (DLY2 - DLY1) * 1.0D6
C       print *,'old: DLY1, DLY2     ', DLY1*1.D12, DLY2*1.D12
C       print *,'old: DC43 (psec) ', DC43*1.D6
C
C  New code:
       TSOF1 = -TSOFF
       TSOF2 = -(TSOFF + CLDIF)
C                                      Geocentric model delays
          MLDLG1 =    DPOLY (NTERMS,ANTDLY(1,REF),TSOF1 )
     *         + ANTCLK(IPOL,REF) + ANTDCK(IPOL,REF)*TSOF1
C    *         - ANTATM(REF) - ANTDAM(REF)*TSOF1
          MLDLG2 =    DPOLY (NTERMS,ANTDLY(1,REM),TSOF2 )
     *         + ANTCLK(IPOL,REM) + ANTDCK(IPOL,REM)*TSOF2
C    *         - ANTATM(REM) - ANTDAM(REM)*TSOF2
C       print *,'new: MLDLG1, MLDLG2 ', MLDLG1*1.D12, MLDLG2*1.D12
C                                      Geocentric model rates
          MLRTG1 = DERPOL( NTERMS, ANTDLY(1,REF), TSOF1 )
C    *         + ANTDCK(IPOL,REF) - ANTDAM(REF)
     *         + ANTDCK(IPOL,REF)
          MLRTG2 = DERPOL( NTERMS, ANTDLY(1,REM), TSOF2 )
C    *         + ANTDCK(IPOL,REM) - ANTDAM(REM)
     *         + ANTDCK(IPOL,REM)
C       print *,'new: MLRTG1, MLRTG2 ', MLRTG1*1.D12, MLRTG2*1.D12
C                                      Second derivatives
          MLACC1 = DR2POL( NTERMS, ANTDLY(1,REF), TSOF1 )
          MLACC2 = DR2POL( NTERMS, ANTDLY(1,REM), TSOF2 )
C       print *,'new: MLACC1, MLACC2 ', MLACC1*1.D12, MLACC2*1.D12
C
C                                      Model delays and rates retarded
C                                      to reference station
C        MTAU1  = -(MLDLG1 - MLDLG1*MLRTG1)
C        DMTAU1 = -(MLRTG1 - MLRTG1**2 - MLDLG1*MLACC1)
C        MTAU2  =  (MLDLG2 + MTAU1*MLRTG2)
C        DMTAU2 =  (MLRTG2 + DMTAU1*MLRTG2 + MTAU1*MLACC2)
C*      print *,'new: MTAU1, MTAU2   ', MTAU1*1.D12, MTAU2*1.D12
C*      print *,'new: DMTAU1, DMTAU2 ', DMTAU1*1.D12, DMTAU2*1.D12
C
        MTAU1  = -(MLDLG1 - MLDLG1*MLRTG1 + .5D0*MLACC1*MLDLG1**2)
        DMTAU1 = -(MLRTG1 - MLRTG1**2 - MLDLG1*MLACC1 +
     *                MLACC1*MLDLG1*MLRTG1)
C*** Iterate: *****************
         XTAU1  = -(MLDLG1 + MTAU1 *MLRTG1 + .5D0*MLACC1*MTAU1**2)
         DXTAU1 = -(MLRTG1 + DMTAU1*MLRTG1 + MTAU1*MLACC1 +
     *                MLACC1*MTAU1*DMTAU1)
         MTAU1  = -(MLDLG1 + XTAU1 *MLRTG1 + .5D0*MLACC1*XTAU1 **2)
         DMTAU1 = -(MLRTG1 + DXTAU1*MLRTG1 + XTAU1*MLACC1 +
     *                MLACC1*XTAU1*DXTAU1)
C*********************************
         MTAU2  =  (MLDLG2 + MTAU1*MLRTG2 +.5D0*MLACC2*MTAU1**2)
         DMTAU2 =  (MLRTG2 + DMTAU1*MLRTG2 + MTAU1*MLACC2 +
     *                MLACC2*MTAU1*DMTAU1)
C
C                                      Total model delays and rates retarded
C                                      to reference station
         MLDLY1 = MTAU1
C    *          + ANTCLK(IPOL,REF) + ANTDCK(IPOL,REF)*TSOF1
     *          + ANTCLK(IPOL,REF) + ANTDCK(IPOL,REF)*(TSOF1+MTAU1)
C    *          - ANTATM(REF) - ANTDAM(REF)*TSOF1
         MLDLY2 = MTAU2
     *          - ANTCLK(IPOL,REM) - ANTDCK(IPOL,REM)*(TSOF2+MTAU1)
C    *          + ANTATM(REM) + ANTDAM(REM)*TSOF2
C        MLRAT1  = DMTAU1 + ANTDCK(IPOL,REF) - ANTDAM(REF)
C        MLRAT2  = DMTAU2 - ANTDCK(IPOL,REM) + ANTDAM(REM)
         MLRAT1  = DMTAU1 + ANTDCK(IPOL,REF)*(1.D0+DMTAU1)
         MLRAT2  = DMTAU2 - ANTDCK(IPOL,REM)*(1.D0+DMTAU1)
C       print *,'new: MLDLY1, MLDLY2 ', MLDLY1*1.D12, MLDLY2*1.D12
C       print *,'new: MLRAT1, MLRAT2 ', MLRAT1*1.D12, MLRAT2*1.D12
C
C                                      SN table offset
         TSNTG = -TSOFF + CLSN
C                                      Residual delays and rates from SN
C                                      table shifted to geocenter time tag
         MBRES1 = ANTMBD(IPOL,REF) + ANTRAT(IPOL,REF)*(TSNTG)
         MBRES2 = ANTMBD(IPOL,REM) + ANTRAT(IPOL,REM)*(TSNTG)
         RRAT1 =  ANTRAT(IPOL,REF)
         RRAT2 =  ANTRAT(IPOL,REM)
C       print *,'new: MBRES1, MBRES2 ', MBRES1*1.D12, MBRES2*1.D12
C       print *,'new: RRAT1,  RRAT2  ', RRAT1*1.D12, RRAT2*1.D12
C
C                                      Residual delays and rates retarded
C                                      to reference station
          ORES1 = -(MBRES1 + MTAU1 *RRAT1)
         DORES1 = -(RRAT1 + DMTAU1*RRAT1)
          ORES2 =  (MBRES2 + MTAU1*RRAT2)
         DORES2 =  (RRAT2 + DMTAU1*RRAT2)
C         ORES1 = -MBRES1
C        DORES1 = -RRAT1
C         ORES2 =  MBRES2
C        DORES2 =  RRAT2
C
           MBRES = ORES2 + ORES1
C       print *,'new: ORES1,  ORES2  ', ORES1*1.D12, ORES2*1.D12
C       print *,'new: DORES1, DORES2 ', DORES1*1.D12, DORES2*1.D12
C
C                                      Total delay at reference station
         MLDLY  = (MLDLY1 - ORES1) + (MLDLY2 - ORES2)
C       print *,'new: MLDLY          ', MLDLY*1.D12
C                                      Total rate at reference station
         MLRAT  =  (MLRAT1 - DORES1) + (MLRAT2 - DORES2)
C       print *,'new: MLRAT          ', MLRAT*1.D12
C     print *,'HFPUT: MBRES,MLDLY,MLRAT ', MBRES,MLDLY,MLRAT
C                                      Convert to Mark III data base units
         DC41 = MLDLY * 1.0D6
         DC42 = MLRAT * 1.0D6
C       print *,'new: DC41 (psec) ', DC41*1.D6
C       print *,'new: DC42 (ps/s) ', DC42*1.D6
C
C                                      Single band delay residuals
C                                        -At geocenter
         SBREG1 = ANTSBD(IPOL,REF) + ANTRAT(IPOL,REF)*(TSNTG)
         SBREG2 = ANTSBD(IPOL,REM) + ANTRAT(IPOL,REM)*(TSNTG)
C                                        -At reference station
         SBRES1 = -(SBREG1 + MTAU1 *RRAT1)
         SBRES2 = SBREG2 +  MTAU1*RRAT2
C        SBRES1 = -SBREG1
C        SBRES2 = SBREG2
C       print *,'new: SBREG1, SBREG2 ', SBREG1*1.D12, SBREG2*1.D12
C       print *,'new: SBRES1, SBRES2 ', SBRES1*1.D12, SBRES2*1.D12
C
C                                      Single band delay (usec)
C                                       (Model + Residual)
         DLY1 = MLDLY1 - SBRES1
         DLY2 = MLDLY2 - SBRES2
         DC43 = (DLY2 + DLY1) * 1.0D6
C       print *,'new: DLY1, DLY2     ', DLY1*1.D12, DLY2*1.D12
C       print *,'new: DC43 (psec) ', DC43*1.D6
C
C                                      Group delay ambiguity (microsec)
         DC44 = MBAMB * 1.0D6
C                                      Apriori Clock (usec)
         DLY1 = ANTCLK(IPOL,REF) + ANTDCK(IPOL,REF) * TSOF1
         DLY2 = ANTCLK(IPOL,REM) + ANTDCK(IPOL,REM) * TSOF2
         DC45 = (DLY2 - DLY1) * 1.0D6
C                                      EPOCH0, Reference station clock
C                                      epoch (microsec)
         DC46 = DLY1 * 1.0D3
C
C                                      Total model phases (rad) at ref
          MLPHS1 = MLDLY1 * NREFQ * TWOPI
            MLPHS1 = DMOD (MLPHS1, TWOPI)
          MLPHS2 = MLDLY2 * NREFQ * TWOPI
            MLPHS2 = DMOD (MLPHS2, TWOPI)
C       print *,'new: MLPHS1, MLPHS2 ', MLPHS1*57.29578, MLPHS2*57.29578
C
C                                      Calculate amplitudes and residual
C                                      phases by IF at SN table time and
C                                      place in RC51.
        CALL APHIF (LIF, LIFNO, VIS(1,1,REF,REM), VIS(1,1,REM,REF),
     *              ANTGAI, IPOL, REF, REM, RC51, SCLAMP, COHER)
C
      IF (APARM(5) .LE. 0.) THEN
C                                      Phases from the UV data:
C                                      Average of RC51 residual phases
C                                      at SN table time
        DIFPH1 = RESPHS (LIF, LIFNO, RC51, ANTPHS(1,IPOL,REF))
C       print *,' DIFPH1 ', DIFPH1*57.29578
      ELSE
C                                      Phases from the SN table:
C       print *,'REF-phases    ', ANTPHS(1,IPOL,REF)*57.29578,
C    *      ANTPHS(2,IPOL,REF)*57.29578, ANTPHS(3,IPOL,REF)*57.29578,
C    *      ANTPHS(4,IPOL,REF)*57.29578
        REFPHS = AVGPHS (LIF, LIFNO, ANTPHS(1,IPOL,REF))
        REMPHS = AVGPHS (LIF, LIFNO, ANTPHS(1,IPOL,REM))
        DIFPH1 = REFPHS - REMPHS
        CALL SNPHS (LIF, LIFNO, ANTPHS, FOFF, IPOL, REF, REM,
     *                 MAXIF, RC51, MBERR)
C#      print *,'REM-phases,DIFPH1 ', ANTPHS(1,IPOL,REM)*57.29578,
C#   *      ANTPHS(2,IPOL,REM)*57.29578, ANTPHS(3,IPOL,REM)*57.29578,
C#   *      ANTPHS(4,IPOL,REM)*57.29578, DIFPH1*57.29578
C       print *,'REFPHS,REMPHS,DIFPH1:', REFPHS*57.29578,
C    *           REMPHS*57.29578, DIFPH1*57.29578
      ENDIF
C                                      Shift phase residual from reference
C                                      pixel frequency to frequency NREFQ.
C                                      [FSHIFT normally zero.]
C##     DIFPH1 = DIFPH1 + FSHIFT*(-ANTSBD(IPOL,REM) +
C##  *          ANTSBD(IPOL,REF))*TWOPI
C                                      Correct the residual phase to the
C                                      UTC Time Tag epoch at the geocenter
        DIFPCE = DIFPH1 - ( (ANTRAT(IPOL,REM) - ANTRAT(IPOL,REF))
     *         * TSNTG * FRQFAC )
C       print *,'     DIFPCE    ', DIFPCE*57.29578
C                                      And correct to ref station
        DIFPHS = DIFPCE -
     *    (ANTRAT(IPOL,REM)*MTAU1 - ANTRAT(IPOL,REF)*MTAU1 ) * FRQFAC
C    *    (ANTRAT(IPOL,REM)*MTAU1 + ANTRAT(IPOL,REF)*MTAU1 ) * FRQFAC
C       print *,'     DIFPHS    ', DIFPHS*57.29578
C#     print *,'DIFPH1,DIFPCE,DIFPHS:',IC2, DIFPH1*57.29578,
C#   *              DIFPCE*57.29578, DIFPHS*57.29578
C
C                                      Total fringe phase (deg)
C                                       (Model + Residual)
C       PDIFF = (MLPHS2 - MLPHS1) + DIFPHS
        PDIFF = (MLPHS2 + MLPHS1) + DIFPHS
        RC58 = DMOD (PDIFF, TWOPI) * 57.29577951
C       print *,' RC58 ', RC58
C
C*****************************************************************************
C                                       Computations for the 'central' epoch
C                                       Central epoch = the integer second
C                                       nearest to the center of the data.
C                                       (SN time is the center time.)
C
C                                       Central epoch time, CEPTIM (days).
       IF (SNTIMC(REM).NE.DBLANK) THEN
          NN = 2
          IQN = 1
          CALL HAYDY2(NN, OBSDAT, SNTIMC(REM), CC25, IQN, IC12, CEPTIM)
C                                       Offset from SN time to Central
C                                       Epoch (Center-of-Earth, seconds)
          SNCO = (SNTIMC(REM) - CEPTIM) * 86400.D0
C                                       Offset from CL time to Central
C                                       Epoch (Center-of-Earth, seconds)
         EPOFF = (CURTIM(REF) - CEPTIM)*86400.0D0
       ELSE
C                                       No SN time(?), set central epoch
C                                       to CL time (nearest second)
          NN = 2
          IQN = 1
          CALL HAYDY2 (NN, OBSDAT, CURTIM(REF), CC25, IQN, IC12, CEPTIM)
          SNCO  = (CURTIM(REF) - CEPTIM) * 86400.D0
          EPOFF = SNCO
          END IF
C                                       Correction to shift from the
C                                       center-of-Earth clock to the ref
C                                       station clock at Central Epoch Time
C                                       (CEPTIM).
       CALL HFREF (REF, NTERMS, IPOL, ANTDLY, ANTCLK, ANTDCK, ANTATM,
     *    ANTDAM, EPOFF, TCOFF  )
        TCSN0 = TCOFF + CLSN
C                                       TCOFF = offset from CL time to
C                                        Central Epoch corrected to the
C                                        reference station clock. Use TCOFF
C                                        for model values at Central Epoch.
C                                       TCSN0 = offset from SN time to
C                                        Central Epoch corrected to the
C                                        reference station clock. Use TCSN0
C                                        for residual values at Central Epoch.
C
C                                       Model delay (sec) at central epoch
          MCDLY1 = DPOLY (NTERMS, ANTDLY(1,REF), TCOFF)
          MCDLY2 = DPOLY (NTERMS, ANTDLY(1,REM), (TCOFF-CLDIF) )
C
C                                       Model rate (sec/sec) at central epoch
          MCRAT1 = DERPOL (NTERMS, ANTDLY(1,REF), TCOFF)
          MCRAT2 = DERPOL (NTERMS, ANTDLY(1,REM), (TCOFF-CLDIF) )
C                                       Model plus Residual rates
          RAT1 = MCRAT1 - ANTRAT(IPOL,REF)
          RAT2 = MCRAT2 - ANTRAT(IPOL,REM)
C
C                                       Model phases (rad) at central epoch
          MCPHS1 = MCDLY1 * NREFQ * TWOPI
           MCPHS1 = DMOD (MCPHS1, TWOPI)
          MCPHS2 = MCDLY2 * NREFQ * TWOPI
           MCPHS2 = DMOD (MCPHS2, TWOPI)
C
C                                       Residual MBD's at central epoch
          MBRES1 = ANTMBD(IPOL,REF) + ANTRAT(IPOL,REF)*TCSN0
          MBRES2 = ANTMBD(IPOL,REM) + ANTRAT(IPOL,REM)*TCSN0
C                                       DEL OBSVM, Total delay at central
C                                       epoch (microsec)
          DC47 = ( (MCDLY2 - MCDLY1) - (MBRES2 - MBRES1) ) * 1.0D6
C
C                                       RAT OBSVM, Total rate at central
C                                       epoch (microsecsec/sec)
          DC48 = ( RAT2 - RAT1 ) * 1.0D6 / (1.D0 + RAT1)
C
C-----------------------------------------------------------------------
C                                       DLY2 phase delay at EPO + 1 sec
C                                       [Actually group delays are preferrred
C                                        and are calculated here]
C                                       Use TCOFP1 for model at epoch+1 sec
C                                       Use TCSNP1 for residuals
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C   Replace central epoch delay and rate with geocenter delay and
C    rate at the specified time tag on the geocenter clock.
C     Test version, 2001.07.05  -DG-
C
          MBRES1 = ANTMBD(IPOL,REF) + ANTRAT(IPOL,REF)*(TSNTG)
          MBRES2 = ANTMBD(IPOL,REM) + ANTRAT(IPOL,REM)*(TSNTG)
          DC47 = (MLDLG2 - MLDLG1) - (MBRES2 - MBRES1)
     *       - (ANTCLK(IPOL,REM) + ANTDCK(IPOL,REM)*TSOF2)
     *       + (ANTCLK(IPOL,REF) + ANTDCK(IPOL,REF)*TSOF1)
          DC47 = DC47 * 1.D6
          DC48 = (MLRTG2 - MLRTG1) - (RRAT2 - RRAT1)
     *       - ANTDCK(IPOL,REM) + ANTDCK(IPOL,REF)
          DC48 = DC48 * 1.D6
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
          EPOCP1 = EPOFF + 1.D0
          CALL HFREF (REF, NTERMS, IPOL, ANTDLY, ANTCLK, ANTDCK, ANTATM,
     *       ANTDAM, EPOCP1, TCOFP1 )
          TCSNP1 = TCOFP1 + CLSN
C
C                                       A priori delay model
          PD1 = DPOLY (NTERMS, ANTDLY(1,REF), TCOFP1)
          PD2 = DPOLY (NTERMS, ANTDLY(1,REM), (TCOFP1-CLDIF) )
          MBRES1 = ANTMBD(IPOL,REF) + ANTRAT(IPOL,REF)*TCSNP1
          MBRES2 = ANTMBD(IPOL,REM) + ANTRAT(IPOL,REM)*TCSNP1
          DC49 = ( (PD2 - PD1) - (MBRES2 - MBRES1) ) * 1.0D6
C-----------------------------------------------------------------------
C                                       DLY3 phase delay at EPO - 1 sec
C                                       [Actually group delays are preferrred
C                                        and are calculated here]
C                                       Use TCOFM1 for model at epoch+1 sec
C                                       Use TCSNM1 for residuals
        EPOCM1 = EPOFF - 1.D0
        CALL HFREF (REF, NTERMS, IPOL, ANTDLY, ANTCLK, ANTDCK, ANTATM,
     *     ANTDAM, EPOCM1, TCOFM1 )
        TCSNM1 = TCOFM1 + CLSN
C                                       A priori delay model
          PD1 = DPOLY (NTERMS, ANTDLY(1,REF), TCOFM1)
          PD2 = DPOLY (NTERMS, ANTDLY(1,REM), (TCOFM1-CLDIF) )
          MBRES1 = ANTMBD(IPOL,REF) + ANTRAT(IPOL,REF)*TCSNM1
          MBRES2 = ANTMBD(IPOL,REM) + ANTRAT(IPOL,REM)*TCSNM1
          DC50 = ( (PD2 - PD1) - (MBRES2 - MBRES1) ) * 1.0D6
C-----------------------------------------------------------------------
C                                       Phase cal rate by station
          IF (ANTPCR(1,IPOL,REF).NE.FBLANK) THEN
             RC52(1) = ANTPCR(1,IPOL,REF)
          ELSE
             RC52(1) = 0.0
          END IF
          IF (ANTPCR(1,IPOL,REM).NE.FBLANK) THEN
             RC52(2) = ANTPCR(1,IPOL,REM)
          ELSE
             RC52(2) = 0.0
          END IF
C                                       DELRESID, Delay residual (microsec)
          RC53 = - MBRES * 1.0E6
C                                       RATRESID rate residual (usec/sec)
          RC55 = -(ANTRAT(IPOL,REM) - ANTRAT(IPOL,REF)) * 1.0E6
C
C                                       Coherent correlation ???
C         RC57 = SQRT (AMPFRC(2,REF,REM)**2 + AMPFRC(2,REM,REF)**2)
          DUM57 = SQRT (AMPFRC(2,REF,REM)**2 + AMPFRC(2,REM,REF)**2)
          RC57 = COHER
C55         print *,'DUM57,RC57: ', DUM57,RC57
C
C                                       U, V cyc/asec
          DTEMP = NREFQ / REFREQ
          RC59(1) = UVAVG(1,REF,REM) / 206264.806 * DTEMP
          RC59(2) = UVAVG(2,REF,REM) / 206264.806 * DTEMP
C
C                                       Incoherent amp. avg.
          RC61 = SCLAMP
C                                       Single band delay residual
C       RC64 = ( -(ANTSBD(IPOL,REM) + ANTRAT(IPOL,REM)*TSNTG)
C    *       + (ANTSBD(IPOL,REF) + ANTRAT(IPOL,REF)*TSNTG) ) * 1.0E6
        RC64 = (SBRES1 + SBRES2) * 1.E6
C
C                                       Incoherent time averaged fringe
C                                       amplitude
          RC67 = AMPFRC(1,REF,REM) * 10000.0
C
C                                       EARP Total phase referred to
C                                       Earth center (degrees)
C                                       Use UTC Time Tag at center of Earth
C                                       Offset from CL table = -TSOFF
C                                       Offset from SN table = -TSOFF+CLSN
C
C                                       Reference station model at center
C                                       of Earth time offset -TSOFF
          TDLY = DPOLY (NTERMS, ANTDLY(1,REF), TSOF1)
          TPHS = TDLY * NREFQ * TWOPI
          PHS1 = DMOD (TPHS, TWOPI)
C                                       Remote station model at center
C                                       of Earth time offset -TSOFF
C                                       (=TSOF2 in REM CL table record)
          TDLY = DPOLY (NTERMS, ANTDLY(1,REM), TSOF2)
          TPHS = TDLY * NREFQ * TWOPI
          PHS2= DMOD (TPHS, TWOPI)
C                                       Residual phase at time offset
C                                        (-TSOFF+CLSN)
C                -- already have this --
C         DIFPCE = DIFPH1 - (ANTRAT(IPOL,REM) - ANTRAT(IPOL,REF))
C    *           * (-TSOFF+CLSN) * FRQFAC
C                                       Model plus residual
          PDIFF =  PHS2 - PHS1 + DIFPCE
C                                       EARP, Total phase at Earth center
          RC68 = DMOD (PDIFF, TWOPI) * 57.29577951
C         print *,' RC68 ', RC68
C
C                                       REARP, residual phase referred
C                                       to Earth center (deg)
          RC69 = DMOD (DIFPCE, TWOPI) * 57.29577951
C
C                                       Start/stop times in sec past the hour
C      IF (SNTIMC(REM).NE.DBLANK) THEN
          RC70 = (SNTIMC(REM) - SNINT(REM)*0.5D0 - IC2(4)/24.D0)*86400.
          RC71 = (SNTIMC(REM) + SNINT(REM)*0.5D0 - IC2(4)/24.D0)*86400.
            IF(SNTIMC(REM) .GE. 1.D0) THEN
              RC70 = AMOD (RC70,86400.)
              RC71 = AMOD (RC71,86400.)
            ENDIF
C      ELSE
C         RC70 = (CURTIM - SNINT(REM)*0.5D0 - IC2(4)/24.D0) * 86400.
C         RC71 = (CURTIM + SNINT(REM)*0.5D0 - IC2(4)/24.D0) * 86400.
C           IF(CURTIM .GE. 1.D0) THEN
C             RC70 = AMOD (RC70,86400.)
C             RC71 = AMOD (RC71,86400.)
C           ENDIF
C      ENDIF
C                                       Epoch offset from center in sec.
          IF (SNTIMC(REM).NE.DBLANK) THEN
             RC72 = TSNTG
          ELSE
             RC72 = 0.0
             END IF
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C                                       Effective duration in seconds.
          IF (SNINT(REM).NE.FBLANK) THEN
             XC73 = SNINT(REM) * 86400.0 * FRACT
             XC73 = XC73 * .95
          ELSE
             XC73 = INTER * 86400.0
             XC73 = XC73 * .95
             END IF
C
           RC73  = 0.0
          DO 127 I=1,LIF
           RC73 = RC73 + VIS(2,LIFNO(LIF),REM,REF) - 1.
 127      CONTINUE
           RC73 = (RC73/LIF) * APARM(3) * .95
C           print *,'APARM(3),XC73,RC73,ratio: ', APARM(3), XC73, RC73,
C    *               RC73/XC73
C          WRITE(6,129) TTAG,REF,REM,RC60(1),RC60(2),RC73/.95,XC73/.95,
C    *                  RC73/XC73
 129       FORMAT('RC73:',F10.6,2I4,2F8.2,2F10.2,F12.6)
           WRITE(MSGTXT,129) TTAG,REF,REM,RC60(1),RC60(2),RC73/.95,
     *                  XC73/.95, RC73/XC73
           CALL MSGWRT(1)
          IF (XC73 .LT. RC73) RC73 = XC73
             IF (RC73 .LT. 6.) GO TO 999
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C                                       Ratio min. to max accepted in %
C                                         -moved to QCODE subroutine
C         RC75 = 100.0 * (SNINT(REM)/INTER) * (AMPFRC(1,REM,REF)/MAXSMP)
C
C                                       % data discarded
               IF (MAXSMP .GT. 1) THEN
C                 RC76 = 100.0 * (1.0 - (AMPFRC(1,REM,REF) /MAXSMP))
C   Above equation gives wrong answer (always says 50% ?????)
                  RC76 = 100.0 * (1.0 - FRACT)
               ELSE
                  RC76 = 0.0
               END IF
C                                       TOTPM Total phase at central epoch
C    Residual phase at time offset (TCSN0)
          PDIFF = DIFPH1 - (ANTRAT(IPOL,REM) - ANTRAT(IPOL,REF))
     *          * TCSN0 * FRQFAC
          TDP = MCPHS2 - MCPHS1 + PDIFF
          RC77 = DMOD (TDP, TWOPI) * 57.29577951
C
C                                       SNR: Ignore values from FRING.
C                                       Use formula from Alan Whitney's thesis
       RC65  = RC57 * SQRT(2. * BWP * RC73 * LIF) / 1.75
C                                       Set quality code to zero for low SNR's
       IF (RC65.LT.6.0 .AND. CC33.NE.'A') CC33 = '0'
C
C                                       Compute the RMS Bandwidth, MHz
        T1 = 0.D0
        T2 = 0.D0
       DO 71 I = 1,LIF
        T1 = T1 + (DC39(I))**2
        T2 = T2 +  DC39(I)
 71    CONTINUE
        T1 = T1 / LIF
        T2 = T2 / LIF
        T2 = T2**2
        BWRMS = DSQRT (T1 - T2)
C                                       RATSIGMA, Rate uncertainty
C                                       (microseconds/sec!)
       RC56 = SQRT(12.) / (TWOPI * DSQRT(T1) * RC65 * RC73)
C
C                                       DELSIGMA, Delay uncertainty
C                                       (microseconds!)
       RC54 = 1.0 / (TWOPI * RC65 * BWRMS)
C                                       Modification from D. Lebach, add
C                                       time extrapolation component
       RC54 = SQRT( RC54**2 + (RC56*TSNTG)**2 )
C       print *,'RC54,MBERR(psec) ', RC54*1.E6, MBERR*1.E12
C
C      print *,'RC54/56/57/65/73,BWRMS,BWP,LIF,COHER ',
C    *         RC54,RC56,RC57,RC65,RC73,BWRMS,BWP,LIF,COHER
C
C
C      RC74 = SIGSBD (BWP, ANTSNR(IPOL,REF), ANTSNR(IPOL,REM))
       RC74 = 1.0E6*SQRT(12.0/LIF)/(RC65*TWOPI*BWP)
C                                       Modification from D. Lebach, add
C                                       time extrapolation component
       RC74 = SQRT( RC74**2 + (RC56*TSNTG)**2 )
C
               NREC = NREC + 1
               IF (MOD (NREC, 200) .EQ. 1) THEN
                  WRITE (MSGTXT, 1500) NREC
 1500             FORMAT ('Writing HF row ', I5)
                  CALL MSGWRT(1)
               ENDIF
C      WRITE(6,1234) IC2, CC26(1:2), CC27(1:2), CC25, RC58,
C    *       DC41*1.D6, DC42*1.D9, RC65
C1234  FORMAT(i4,1x,5(I2,1X),2x,A2,'-',A2,2x,A8,3x, F8.1, F18.2,
C    *        F15.2, F9.1)
C
               CALL OTABHF (HFTAB, 'WRIT', HFROW,
     *            CC1, IC2, IC3, IC4, IC5, IC6, IC7, IC8, IC9, IC10,
     *            IC11, IC12, IC13, IC14, IC15, IC16, IC17, IC18, IC19,
     *            IC20, IC21, IC22, IC23, IC24, CC25, CC26, CC27, CC28,
     *            CC29, CC30, CC31, CC32, CC33, CC34, CC35, CC36, CC37,
     *            CC38, DC39, DC40, DC41, DC42, DC43, DC44, DC45, DC46,
     *            DC47, DC48, DC49, DC50, RC51, RC52, RC53, RC54, RC55,
     *            RC56, RC57, RC58, RC59, RC60, RC61, RC62, RC63, RC64,
     *            RC65, RC66, RC67, RC68, RC69, RC70, RC71, RC72, RC73,
     *            RC74, RC75, RC76, RC77, IERR)
               IF (IERR.NE.0) GO TO 999
 490           CONTINUE
 500        CONTINUE
 510     CONTINUE
C
 999  RETURN
      END
       SUBROUTINE HFREF (REF, NTERMS, IPOL, ANTDLY, ANTCLK, ANTDCK,
     *   ANTATM, ANTDAM, TGEOC, THAYST )
C-------------------------------------------------------------------------------
C Calc offset only
C
C  Convert geocenter frame to reference station frame (Haystack frame).
C    Inputs:
C      REF     I
C      NTERMS  I    Number of terms in delay polynomial
C      IPOL    I    Polarization number (1 or 2)
C      ANTDLY  D(NTERMS,?) Antenna delay polynomials;
C                       If blanked then there is no data for this antenna.
C      ANTCLK  R(2,?) Antenna "clock" error 1/poln (sec)
C      ANTDCK  R(2,?) Antenna "clock" error rate 1/poln (sec/sec)
C      ANTATM  R(?)   Antenna "atmosphere" error. (sec)
C      ANTDAM  R(?)   Antenna "atmosphere" error rate. (sec/sec)
C      TGEOC   D    Geocenter time offset
C
C   Output:
C      THAYST  D    Reference station time offset corresponding to TGEOC.
C
C*****************************************************************************
C
      INTEGER   REF, ITER, NTERMS, IPOL
      DOUBLE PRECISION DPOLY, TGEOC, THAYST
      DOUBLE PRECISION ANTDLY(NTERMS,*)
      REAL      ANTCLK(2,*), ANTDCK(2,*), ANTATM(*), ANTDAM(*)
      DOUBLE PRECISION TT1, TT2, T2P, TTT, D21T2
C
       TT1 = -TGEOC
       TT2 = TT1
       D21T2 = 0.D0
      ITER = 0
C
 186   CONTINUE
       ITER = ITER + 1
       TTT = TT1 - D21T2
       D21T2 = - ( DPOLY (NTERMS,ANTDLY(1,REF),(TTT) )
     *      + ANTCLK(IPOL,REF) + ANTDCK(IPOL,REF)*TTT
     *      - ANTATM(REF) - ANTDAM(REF)*TTT )
       TTT = TT1 - D21T2
       T2P = TT1 - ( DPOLY (NTERMS,ANTDLY(1,REF),(TTT) )
     *      + ANTCLK(IPOL,REF) + ANTDCK(IPOL,REF)*TTT
     *      - ANTATM(REF) - ANTDAM(REF)*TTT )
C       PRINT *, 'ITER,D21T2,T2P', ITER,D21T2,T2P
       IF (DABS(TT2 - T2P) .LE. 1.0D-14) THEN
         TT2 = T2P
         THAYST = T2P
         GO TO 187
       ELSE
         TT2 = T2P
          IF (ITER .GT.10) THEN
            PRINT *, ' Iteration Failed !!!!!!!!'
            GO TO 999
          ENDIF
         GO TO 186
       ENDIF
C
 999   CONTINUE
C  write error message !!!!!
C
 187   CONTINUE
C
C       WRITE(6,'(" TGEOC,D21T2,THAYST: ",F15.10,E20.9,F15.10)')
C    *              TGEOC,D21T2,THAYST
C
C
       RETURN
       END
      SUBROUTINE RUNCOD (OBSDAT, TIME, CODE)
C-------------------------------------------------------------------------------
C
C   Convert reference date and start time to Haystack type "run code"
C   Inputs:
C      OBSDAT  C*8  Observing date as 'yyyymmdd' (15OCT98 AIPS)
C                    or 'dd/mm/yy' (earlier AIPS)
C      TIME    D    Time in days since OBSDAT
C   Output:
C      CODE    C*8  Run code as 'ddd-hhmm'
C-----------------------------------------------------------------------
      CHARACTER OBSDAT*8, CODE*8
      DOUBLE PRECISION TIME
      INTEGER   DAY, HOUR, MIN, DD, MM, YY, DOY
      DOUBLE PRECISION TEMP, JD, JD0
      CHARACTER JAN1*8
C-----------------------------------------------------------------------
      CODE = 'ERROR   '
C
C                                       Decode ref. date
      IF ( OBSDAT(3:3) .EQ. '/' ) THEN
        READ (OBSDAT, 1000, ERR=999) DD, MM, YY
      ELSE
        READ (OBSDAT, 1010, ERR=999) YY, MM, DD
C          IF ( YY .GE. 2000) YY = YY - 2000
C          IF ( YY .GT. 1900) YY = YY - 1900
      ENDIF
C
 1000 FORMAT (I2,'/', I2,'/', I2)
 1010 FORMAT (I4,I2,I2)
C
C                                       Convert TIME
      TEMP = TIME
      DAY = TEMP
      TEMP = 24.0D0 * (TEMP - DAY)
      HOUR = TEMP
      TEMP = 60.0D0 * (TEMP - HOUR)
      MIN = TEMP
C
C                                       Get DOY
      CALL JULDAY (OBSDAT, JD)
C
      IF ( OBSDAT(3:3) .EQ. '/' ) THEN
        WRITE (JAN1,1001,ERR=999) YY
 1001      FORMAT ('01/01/', I2)
      ELSE
        WRITE (JAN1,1011,ERR=999) YY
 1011      FORMAT (I4,'0101')
      ENDIF
      CALL JULDAY (JAN1, JD0)
C
      DOY = JD - JD0 + 1.5D0 + DAY
C     write(6,'("JAN1,JD,JD0,DOY ",A8,1x,2f10.1,i12)') JAN1,JD,JD0,DOY
C                                       Fill return string
      WRITE (CODE,1002,ERR=999) DOY, HOUR, MIN
C       print *,'DOY, HOUR, MIN, CODE ', DOY, HOUR, MIN, CODE
 999  RETURN
 1002 FORMAT (I3.3, '-', 2I2.2)
      END
      SUBROUTINE FRECOD (FREQ, CODE)
C-----------------------------------------------------------------------
C   Returns character band code for frequency
C   Inputs:
C      FREQ    D    Frequency in Hz.
C   Output:
C      CODE    C*2
C-----------------------------------------------------------------------
      CHARACTER CODE*2
      DOUBLE PRECISION FREQ
      INTEGER   I, NBAND
C                                       NBAND = number of defined bands
      PARAMETER (NBAND = 9)
      DOUBLE PRECISION FLOW(NBAND), FHI(NBAND)
      CHARACTER FCODE(NBAND)*2
      DATA FCODE /'4 ',   'P ',  'L ',  'S ',  'C ',  'X ',   'U ',
     *   'K ',      'Q '/
      DATA FLOW /0.0D0, 250.0D6, 1.0D9, 2.0D9, 4.0D9, 7.0D9, 13.0D9,
     *   18.0D9, 30.0D9/
      DATA FHI /2.5D8, 1000.0D6, 2.0D9, 4.0D9, 7.0D9, 13.0D9, 18.0D9,
     *   30.0D9, 50.0D9/
      CODE = '  '
      DO 100 I = 1,NBAND
         IF ((FREQ.GE.FLOW(I)) .AND. (FREQ.LE.FHI(I))) CODE = FCODE(I)
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE HAYDY2 (N, OBSDAT, TIME, SOURC, IQN, ARRAY, TTAG)
C-----------------------------------------------------------------------
C   Convert reference date and time to Haystack type time/date
C   Inputs:
C      N       I    N=1, Use schedule to compute time tags or use an
C                        input  file of time tags (user must supply).
C                        ARRAY will be Haystack definition of time tag
C                        or  the user supplied observation time tag.
C                        TSOFF =  (TIME) - (TIME TAG), in seconds.
C                   N=2, ARRAY will be TIME rounded to nearest integer
C                        second.
C
C      OBSDAT  C*8  Observing date as 'yyyymmdd' (15OCT98 AIPS)
C                      or 'dd/mm/yy' (earlier AIPS).
C                   !!! DOES NOT CHANGE AT 00:00 UTC !!!
C      TIME    D    Time in days since OBSDAT. TIME will normally be
C                   either the CL table time or the SN table time.
C                   For N=1, the nearest entry from the HTIME array will
C                   be selected as the time tag, TTAG. For N=2 (or any
C                   integer not equal to 1), TIME will be rounded up or
C                   down to the nearest integer second. After midnight
C                   UTC of the first day, TIME will be greater than 1.0!
C      IQN     I    Qauntization interval for time tags, seconds.
C
C   Output:
C      ARRAY   I(6) Year, month, day, h, m, s of HF TIME
C      TTAG    R*8  Time tag as fraction of a day since OBSDAT. Greater
C                   than 1 after midnight UTC!
C   Note: This subroutine no longer computes the offset from the CL or
C         the  SN table time. This is done in HFPUT for greater clarity
C         and versatility.
C  Mods:
C   2001.06.15 D. Gordon  Modified to round time tag to nearest IQN
C              seconds, instead of nearest second. IQN hard coded to
C              10 seconds for APARM(1) = 2, 3, or 4.
C-----------------------------------------------------------------------
C
      CHARACTER OBSDAT*8, SOURC*8
      DOUBLE PRECISION TIME, TTAG
      INTEGER ARRAY(6), N, NNN, IQN
      INTEGER DAY, HOUR, MIN, SEC, I, ISMIN, ITMP
      DOUBLE PRECISION TEMP, TSMIN, ZTIME, TMP1
      INTEGER IMNTHS(12)
      REAL      APARM(10), DPARM(10)
      COMMON /APRM/ APARM, DPARM
      INCLUDE 'TIMETAGS.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE IMNTHS
      DATA IMNTHS /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
C-----------------------------------------------------------------------
C
C                                       Check for leap year
      IF(MOD(ARRAY(1),4) .EQ. 0) IMNTHS(2) = 29
C
C                                      Decode OBSDAT as 'YYYYMMDD' or
C                                       'DD/MM/YY'
      IF ( OBSDAT(3:3) .EQ. '/' ) THEN
        READ (OBSDAT, 1000, ERR=999) ARRAY(3), ARRAY(2), ARRAY(1)
      ELSE
        READ (OBSDAT, 1010, ERR=999) ARRAY(1), ARRAY(2), ARRAY(3)
           IF (ARRAY(1) .GE. 2000) ARRAY(1) = ARRAY(1) - 2000
           IF (ARRAY(1) .GT. 1900) ARRAY(1) = ARRAY(1) - 1900
      ENDIF
C
 1000 FORMAT (I2,1X,I2,1X,I2)
 1010 FORMAT (I4,I2,I2)
C     print *, 'OBSDAT, Y/M/D ', OBSDAT, ARRAY(1), ARRAY(2), ARRAY(3)
C
       NNN = N
C
 117  CONTINUE
C
      IF ( NNN.EQ.1 ) THEN
C                                       Check for correct schedule
         IF (APARM(1) .NE. 4) THEN
            IF ( (ARRAY(1) .NE. KYEAR) .OR.
     *           (ARRAY(2) .NE. KMONTH) .OR.
     *           (ARRAY(3) .NE. KDAY) )  THEN
               PRINT *, ' Stopping: OBSDAT, KDAY, KMONTH, KYEAR ',
     *              OBSDAT, KDAY, KMONTH, KYEAR
                PRINT *,' Stopping: ARRAY ', ARRAY(3),ARRAY(2),ARRAY(1)
                STOP
               ENDIF
            ENDIF
C                           Match TIME with allowed time tag values
         TSMIN = 10000.0D0
         ISMIN = 0
C
         DO 100 I = 1, NHTIME
            IF (SRC(I) .EQ. '        ') GO TO 95
            IF (SOURC .NE. SRC(I)) GO TO 100
 95         CONTINUE
            ZTIME = DABS(HTIME(I)-TIME)
            IF (ZTIME .GE. TSMIN) GO TO 100
            ISMIN = I
            TSMIN = ZTIME
 100     CONTINUE
C
         IF (ISMIN .EQ. 0) THEN
            WRITE (MSGTXT, 1100) SOURC
            WRITE (6, 1100)      SOURC
 1100       FORMAT ('Problem finding time tag, SOURC = ', A8)
            CALL MSGWRT(4)
C    Couldn't match sources. Use input SN/CL time instead.
            NNN = 2
            GO TO 117
C           STOP
         ENDIF
C
C                                       Match found, set time tag
         TTAG = HTIME(ISMIN)
C                                       Seconds
         ARRAY(6) = ITAG(ISMIN,3)
C                                       Minutes
         ARRAY(5) = ITAG(ISMIN,2)
C                                       Hours
         ARRAY(4) = ITAG(ISMIN,1)
         TEMP = 0.
 37      CONTINUE
         IF(ARRAY(4) .GE. 24) THEN
            ARRAY(4) = ARRAY(4) - 24
            TEMP = TEMP + 1.
            GO TO 37
         ENDIF
C                                       Day
         ARRAY(3) = ARRAY(3) + TEMP
C                                       Month
C                                       ARRAY(2) = ARRAY(2)
C                                       Year
         IF (ARRAY(1) .LT. 90) THEN
            ARRAY(1) = ARRAY(1) + 2000
         ELSE
            ARRAY(1) = ARRAY(1) + 1900
         ENDIF
         IF (ARRAY(1) .LT. 1990) ARRAY(1) = ARRAY(1) + 100
C
C                                       Increment month/day if necessary
         IF (ARRAY(3) .GT. IMNTHS(ARRAY(2))) THEN
            ARRAY(3) = ARRAY(3) - IMNTHS(ARRAY(2))
            ARRAY(2) = ARRAY(2) + 1
         ENDIF
C                                       Increment year/month if necessary
         IF (ARRAY(2) .GT. 12) THEN
            ARRAY(2) = ARRAY(2) - 12
            ARRAY(1) = ARRAY(1) + 1
         ENDIF
C
      ELSE
C                                       Round input time to nearest
C                                       IQN seconds
         TEMP = TIME
         DAY = TEMP
         TEMP = 24.0D0 * (TEMP - DAY)
         HOUR = TEMP
         TEMP = 60.0D0 * (TEMP - HOUR)
         MIN = TEMP
         TEMP = 60.0D0 * (TEMP - MIN)
C        SEC = TEMP + 0.5
         TMP1 = (TEMP + IQN/2.D0)/IQN
         ITMP = TMP1
         SEC  = TMP1 * IQN
C
         TTAG = DAY + HOUR/24.0D0 + MIN/1440.0D0 + SEC/86400.0D0
C
         ARRAY(4) = HOUR
         ARRAY(5) = MIN
         ARRAY(6) = SEC
C                                       Seconds were rounded up
C                                       ==> might be 60 seconds.
         IF (ARRAY(6) .GE. 60) THEN
            ARRAY(6) = ARRAY(6) - 60
            ARRAY(5) = ARRAY(5) + 1
            IF (ARRAY(5) .EQ. 60) THEN
               ARRAY(5) = 0
               ARRAY(4) = ARRAY(4) + 1
            ENDIF
            IF (ARRAY(4) .EQ. 24) THEN
               ARRAY(4) = 0
               DAY = DAY + 1
            ENDIF
         ENDIF
C
         ARRAY(3) = ARRAY(3) + DAY
C                                       ARRAY(2) = ARRAY(2)
C                                       Increment month if necessary
         IF (ARRAY(3) .GT. IMNTHS(ARRAY(2))) THEN
            ARRAY(3) = ARRAY(3) - IMNTHS(ARRAY(2))
            ARRAY(2) = ARRAY(2) + 1
         ENDIF
C
         ARRAY(1) = ARRAY(1) + 1900
         IF (ARRAY(1) .LT. 1990) ARRAY(1) = ARRAY(1) + 100
C                                       Increment Year if necessary
         IF (ARRAY(2) .GT. 12) THEN
            ARRAY(2) = ARRAY(2) - 12
            ARRAY(1) = ARRAY(1) + 1
         ENDIF
C
      ENDIF
C
 999  RETURN
C
      END
      REAL FUNCTION TM2SEC (TIME)
C-----------------------------------------------------------------------
C   Converts time in days to seconds past the hour
C   Inputs:
C      TIME    D    Time in days
C   Output:
C      TM2SEC  R    Seconds past the hour
C-----------------------------------------------------------------------
      DOUBLE PRECISION TIME
      INTEGER   DAY, HOUR
      DOUBLE PRECISION TEMP
C-----------------------------------------------------------------------
C                                       Convert TIME
      TEMP = TIME
      DAY = TEMP
      TEMP = 24.0D0 * (TEMP - DAY)
      HOUR = TEMP
      TM2SEC = 3600.0D0 * (TEMP - HOUR)
C
 999  RETURN
      END
      DOUBLE PRECISION FUNCTION DPOLY (N, DCOEF, X)
C-----------------------------------------------------------------------
C   Evaluate a polynomial at X
C   Inputs:
C      N       I    The number of terms
C      DCOEF   D(*) Coefficient array
C      X       D    Value at which to evaluate.
C   Output:
C      DPOLY   D    Expanded polynomial
C-----------------------------------------------------------------------
      INTEGER   N
      DOUBLE PRECISION DCOEF(N), X
      INTEGER   LOOP
      DOUBLE PRECISION XPOWER
C-----------------------------------------------------------------------
      DPOLY = DCOEF(1)
      XPOWER = X
      DO 100 LOOP = 2,N
         DPOLY = DPOLY + XPOWER * DCOEF(LOOP)
         XPOWER = XPOWER * X
 100  CONTINUE
C
 999  RETURN
      END
C
      DOUBLE PRECISION FUNCTION DERPOL (N, DCOEF, X)
C-----------------------------------------------------------------------
C   Evaluate the first derivative of a polynomial at X
C   Inputs:
C      N       I    The number of terms
C      DCOEF   D(*) Coefficient array
C      X       D    Value at which to evaluate.
C   Output:
C      DERPOL   D    Derivative of polynomial evaluated at X
C-----------------------------------------------------------------------
      INTEGER   N
      DOUBLE PRECISION DCOEF(N), X
      INTEGER   LOOP
      DOUBLE PRECISION XPOWER
C-----------------------------------------------------------------------
      DERPOL = DCOEF(2)
      XPOWER = X
      DO 100 LOOP = 3,N
         DERPOL = DERPOL + (LOOP-1) * XPOWER * DCOEF(LOOP)
         XPOWER = XPOWER * X
 100     CONTINUE
C
 999  RETURN
      END
C
      DOUBLE PRECISION FUNCTION DR2POL (N, DCOEF, X)
C-----------------------------------------------------------------------
C   Evaluate the second derivative of a polynomial at X
C   Inputs:
C      N       I    The number of terms
C      DCOEF   D(*) Coefficient array
C      X       D    Value at which to evaluate.
C   Output:
C      DR2POL   D    Derivative of polynomial evaluated at X
C-----------------------------------------------------------------------
      INTEGER   N
      DOUBLE PRECISION DCOEF(N), X
      INTEGER   LOOP
      DOUBLE PRECISION XPOWER
C-----------------------------------------------------------------------
      DR2POL = 2.D0 * DCOEF(3)
      XPOWER = X
      DO 100 LOOP = 4,N
         DR2POL = DR2POL + (LOOP-2)*(LOOP-1) * XPOWER * DCOEF(LOOP)
         XPOWER = XPOWER * X
 100     CONTINUE
C
 999  RETURN
      END
C
C
      DOUBLE PRECISION FUNCTION DR3POL (N, DCOEF, X)
C-----------------------------------------------------------------------
C   Evaluate the third derivative of a polynomial at X
C   Inputs:
C      N       I    The number of terms
C      DCOEF   D(*) Coefficient array
C      X       D    Value at which to evaluate.
C   Output:
C      DR3POL   D    Derivative of polynomial evaluated at X
C-----------------------------------------------------------------------
      INTEGER   N
      DOUBLE PRECISION DCOEF(N), X
      INTEGER   LOOP
      DOUBLE PRECISION XPOWER
C-----------------------------------------------------------------------
      DR3POL = 6.D0 * DCOEF(4)
      XPOWER = X
      DO 100 LOOP = 5,N
         DR3POL = DR3POL +
     *           (LOOP-3)*(LOOP-2)*(LOOP-1) * XPOWER * DCOEF(LOOP)
         XPOWER = XPOWER * X
 100     CONTINUE
C
 999  RETURN
      END
C
      DOUBLE PRECISION FUNCTION MBDAMB (NUMIF, FOFF, SNRREF, SNRREM)
C-----------------------------------------------------------------------
C   Determine the multiband delay ambiguity for a set of frequencies.
C   Inputs:
C      NUMIF   I    Number of frequencies involved.
C      FOFF    D(*) Frequency table
C      SNRREF  R(*) If  > 0.0 then ref antenna has data.
C      SNRREM  R(*) If  > 0.0 then rem antenna has data.
C   Output:
C      MBDAMB   D    Multi band delay ambituity in sec.
C-----------------------------------------------------------------------
      INTEGER   NUMIF
      DOUBLE PRECISION FOFF(NUMIF)
      DOUBLE PRECISION MBDAM1
      DOUBLE PRECISION IFINC
      REAL      SNRREF(NUMIF), SNRREM(NUMIF)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LOOP, FLAG(MAXIF), NTYPE, IFS(8), IERR, I1
      DOUBLE PRECISION FMIN, FMAX, XFREQ(8)
C-----------------------------------------------------------------------
        I1 = 0
C                                       Set flags for which IFs are valid.
      DO 100 LOOP = 1,NUMIF
         IF ((SNRREF(LOOP).GT.1.0E-10) .AND. (SNRREM(LOOP).GT.1.0E-10))
     *      THEN
            FLAG(LOOP) = 1.0
C Test
C****************************************************
             I1 = I1 + 1
             XFREQ(I1) = FOFF(LOOP)
C****************************************************
         ELSE
            FLAG(LOOP) = 0.0
         END IF
 100  CONTINUE
      CALL FRAMB (NUMIF, FOFF, FLAG, NTYPE, IFS, FMIN, FMAX, IERR)
      IF (IERR.EQ.0) THEN
         MBDAMB = 1.0 / FMIN
      ELSE
         MBDAMB = 0.0D0
      END IF
C
C Test
C****************************************************
         MBDAM1 = MBDAMB
       FMIN = IFINC(XFREQ,I1)
         MBDAMB = 1.0 / FMIN
C         print *,' old/new MBDAMB: ', MBDAM1, MBDAMB
C****************************************************
C
 999  RETURN
      END
C
      SUBROUTINE FRAMB (NUMIF, FREQ, FLAG, NTYPE, IFS, FMIN, FMAX, IER)
C-----------------------------------------------------------------------
C   Determines the ambiguity spacing for fringe fitting the
C   multiband delay. Goes up to four pairs of frequencies
C   Lifted from Ed Fomalont.
C   Inputs:
C      NUMIF    I    Number of IFs.
C      FREQ     D(*) Frequencies (Hz) of the IF's
C      FLAG     I(*) Data for this IF? 1=yes, 0=no
C   Outputs:
C      NTYPE    I    Number of IF's:  Must be pairs =
C                    2,4,6,8
C      IFS      I(8) Up to eight frequencies; four pairs
C                    Sign with frequency
C      FMIN     D    The minimum frequency (up to 4 pairs)
C      FMAX     D    THE maximum frequency difference
C      IER      I    0-> Okay, 1-> Only one frequency
C-----------------------------------------------------------------------
      INTEGER   NUMIF, FLAG(*), NTYPE, IFS(*), IER
      DOUBLE PRECISION FREQ(*), FMIN, FMAX
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NGOOD, NCOMB, LODIF1, LODIF2, XIFS(8,1000), NPAIR, LC1,
     *   NC1, I, L1, L2
      DOUBLE PRECISION GFREQ(MAXIF)
      REAL      DIF(1000), EPS, TEST
C-----------------------------------------------------------------------
C                                       Store Good Frequencies
      IER = 0
      NGOOD = 0
      DO 10 I = 1,NUMIF
         IF (FLAG(I) .NE. 1) GO TO 10
         NGOOD = NGOOD + 1
         GFREQ(NGOOD) = FREQ(I)
 10      CONTINUE
C                                       Need at least 2 freq
      IF (NGOOD .LT. 2) GO TO 900
C                                       Store all FREQ pairs
      EPS = 1.0E10
      NCOMB = 0
      FMAX = 0.0
      DO 120 L2 = 1,NGOOD
         DO 110 L1 = 1,NGOOD
            TEST = ABS(GFREQ(L1) - GFREQ(L2))
            IF (TEST .EQ. 0.0) GO TO 110
            IF (TEST .LT. EPS) EPS = TEST
            IF (TEST .GT. FMAX) FMAX = TEST
            NCOMB = NCOMB + 1
            DIF (NCOMB) = TEST
            XIFS(1, NCOMB) = L1
            XIFS(2, NCOMB) = -L2
 110        CONTINUE
 120     CONTINUE
C                                       Roundoff tolerance
      EPS = EPS / 1000.0
      NPAIR = NCOMB
C                                       Store all PAIR pairs
      DO 220 L2 = 1,NPAIR
         DO 210 L1 = 1,NPAIR
            TEST = DIF(L1) - DIF(L2)
            IF (TEST .EQ. EPS) GO TO 210
            NCOMB = NCOMB + 1
            IF (NCOMB .GT. 500) GO TO 230
            DIF (NCOMB) = TEST
            XIFS(1,NCOMB) = XIFS(1,L1)
            XIFS(2,NCOMB) = XIFS(2,L1)
            XIFS(3,NCOMB) = -XIFS(1,L2)
            XIFS(4,NCOMB) = -XIFS(2,L2)
 210        CONTINUE
 220     CONTINUE
 230  CONTINUE
C                                       Find minimum difference
      FMIN = 1.0E14
C                                       One pair
      DO 310 L1 = 1, NPAIR
         TEST = DIF(L1)
         IF (TEST .LE. EPS) GO TO 310
         IF (TEST .LT. FMIN) THEN
            NTYPE = 2
            FMIN = TEST
            LODIF1 = L1
            END IF
 310     CONTINUE
      L2 = NPAIR + 1
C                                       Two pairs
      DO 320 L1 = L2, NCOMB
         TEST = DIF(L1)
         IF (TEST .LE. EPS) GO TO 320
         IF (TEST .LT. FMIN) THEN
            NTYPE = 4
            FMIN = TEST
            LODIF1 = L1
            END IF
 320     CONTINUE
C                                       Three or four pairs
      NC1 = NCOMB - 1
      DO 420 L2 = 1, NC1
         LC1 = L2 + 1
         DO 410 L1 = LC1, NCOMB
            TEST = DIF(L1) + DIF(L2)
            IF (TEST .LE. EPS) GO TO 410
            IF (TEST .LT. FMIN) THEN
               NTYPE = 6
               FMIN = TEST
               LODIF1 = L2
               LODIF2 = L1
               END IF
 410        CONTINUE
 420     CONTINUE
      IF (NTYPE .EQ. 6 .AND.
     *    LODIF1 .GT. NPAIR .AND. LODIF2 .GT. NPAIR) NTYPE = 8
C
C                                        Get antenna combos
      DO 500 I = 1,NTYPE
         IF (I .LE. 4) THEN
            IFS(I) = XIFS(I,LODIF1)
            ELSE
            IFS(I) = XIFS(I-4,LODIF2)
            END IF
 500     CONTINUE
      IF (NTYPE .EQ. 6) THEN
         IFS(3) = XIFS(3,LODIF2)
         IFS(4) = XIFS(4,LODIF2)
         END IF
      GO TO 999
C                                       Error Return
 900  IER = 1
C
 999  RETURN
      END
C
      REAL FUNCTION SIGMBD (NUMIF, FOFF, SNRREF, SNRREM)
C-----------------------------------------------------------------------
C   Estimate the multiband delay uncertainty for a set of frequencies.
C   Assumed that the SNR is 1/sigma of the phase.
C   Also assumes that the frequencies in FOFF are ordered.
C   Only the two most seperated frequencies with valid data are used in
C   this calculation.
C   Inputs:
C      NUMIF   I    Number of frequencies involved.
C      FOFF    D(*) Frequency table
C      SNRREF  R(*) SNR of the reference antenna (>10 => ref in fit)
C      SNRREM  R(*) SNR of the remote antenna (>10 => ref in fit)
C   Output:
C      SIGMBD  R    Multi band delay uncertainty in usec.
C-----------------------------------------------------------------------
      INTEGER   NUMIF
      DOUBLE PRECISION FOFF(NUMIF)
      REAL      SNRREF(NUMIF), SNRREM(NUMIF)
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LOOP, LO, HI, I
      REAL      SNRLO, SNRHI, SPDIF
      DOUBLE PRECISION DIF
C-----------------------------------------------------------------------
      SIGMBD = 0.0
C                                       Find first channel with valid data.
      LO = 0
      DO 100 LOOP = 1,NUMIF
         IF (((SNRREF(LOOP).GT.1.0E-10) .AND. (SNRREM(LOOP).GT.1.0E-10))
     *      .AND. (LO.LE.0)) LO = LOOP
 100   CONTINUE
C                                       Any valid data?
      IF (LO.LE.0) GO TO 999
C                                       Find highest channel with valid data.
      HI = 0
      DO 200 I = 1,NUMIF
         LOOP = NUMIF - I + 1
         IF (((SNRREF(LOOP).GT.1.0E-10) .AND. (SNRREM(LOOP).GT.1.0E-10))
     *      .AND. (HI.LE.0)) HI = LOOP
 200  CONTINUE
C                                       Multiple IFs?
      IF (LO.EQ.HI) GO TO 999
C                                       Estimate delay errors from phase
C                                       errors across the bandwidth.
      DIF = FOFF(HI) - FOFF(LO)
C                                       SNR assumed to be 1/sigma of
C                                       phase unless one  > 10.  (Both >
C                                       10 implies this is actual SNR)
      IF ((SNRREF(LO).GT.10.0) .AND. (SNRREM(LO).GT.10)) THEN
C                                       Both high
         SNRLO = 0.5 * (SNRREF(LO) + SNRREM(LO))
      ELSE IF  ((SNRREF(LO).LE.10.0) .AND. (SNRREM(LO).GT.10)) THEN
C                                       Ref low
         SNRLO = SNRREM(LO)
      ELSE IF  ((SNRREF(LO).GT.10.0) .AND. (SNRREM(LO).LE.10)) THEN
C                                       Rem. low
         SNRLO = SNRREF(LO)
      ELSE
C                                       Both low
         SNRLO = 0.5 * (SNRREF(LO) + SNRREM(LO))
         END IF
C                                       High channel SNR
      IF ((SNRREF(HI).GT.10.0) .AND. (SNRREM(HI).GT.10)) THEN
C                                       Both high
         SNRHI = 0.5 * (SNRREF(HI) + SNRREM(HI))
      ELSE IF  ((SNRREF(HI).LE.10.0) .AND. (SNRREM(HI).GT.10)) THEN
C                                       Ref low
         SNRHI = SNRREM(HI)
      ELSE IF  ((SNRREF(HI).GT.10.0) .AND. (SNRREM(HI).LE.10)) THEN
C                                       Rem. low
         SNRHI = SNRREF(HI)
      ELSE
C                                       Both low
         SNRHI = 0.5 * (SNRREF(HI) + SNRREM(HI))
         END IF
C                                       Sigma of phase difference
      SPDIF = SQRT (SNRLO*SNRLO + SNRHI*SNRHI)
      IF ((SPDIF.GT.1.0E-30) .AND. (DIF.GT.1.0D-20)) THEN
         SPDIF = 1.0 / SPDIF
         SIGMBD = (SPDIF / DIF) * 1.0E6
      ELSE
C                                        No valid value
         SIGMBD = 0.0
         END IF
C
 999  RETURN
      END
C
      REAL FUNCTION SIGRAT (TIMINT, SNRREF, SNRREM, FREQ)
C-----------------------------------------------------------------------
C   Estimate the delay rate uncertainty for a given obs. time.
C   Assumed that the SNR is 1/sigma of the phase.
C   Uses assumption that first and last times have phase measurments
C   with uncertainties given by the SNR values.
C   Inputs:
C      TIMINT  R    Observation time in sec
C      SNRREF  R    SNR of the reference antenna (>10 => ref in fit)
C      SNRREM  R    SNR of the remote antenna (>10 => ref in fit)
C      FREQ    D    Observing reference frequency (Hz)
C   Output:
C      SIGRAT  R    Delay rate uncertainty in usec/sec.
C-----------------------------------------------------------------------
      REAL      TIMINT, SNRREF, SNRREM
      DOUBLE PRECISION FREQ
      REAL      SNR, SPDIF
C-----------------------------------------------------------------------
C                                       SNR assumed to be 1/sigma of
C                                       phase unless one  > 10.  (Both >
C                                       10 implies this is actual SNR)
      IF ((SNRREF.GT.10.0) .AND. (SNRREM.GT.10)) THEN
C                                       Both high
         SNR = 0.5 * (SNRREF + SNRREM)
      ELSE IF  ((SNRREF.LE.10.0) .AND. (SNRREM.GT.10)) THEN
C                                       Ref low
         SNR = SNRREM
      ELSE IF  ((SNRREF.GT.10.0) .AND. (SNRREM.LE.10)) THEN
C                                       Rem. low
         SNR = SNRREF
      ELSE
C                                       Both low
         SNR = 0.5 * (SNRREF + SNRREM)
         END IF
C                                       Must have both
      IF ((SNRREF.LT.1.0E-10) .OR. (SNRREM.LT.1.0E-10)) SNR = 0.0
C                                       Sigma of phase difference
      SPDIF = SQRT (2.0) * SNR
      IF ((SPDIF.GT.1.0E-30) .AND. (TIMINT.GT.1.0D-20)) THEN
         SPDIF = 1.0 / SPDIF
         SIGRAT = (SPDIF / (TIMINT * FREQ)) * 1.0E6
      ELSE
C                                        No valid value
         SIGRAT = 0.0
         END IF
C
 999  RETURN
      END
C
      REAL FUNCTION SIGSBD (BW, SNRREF, SNRREM)
C-----------------------------------------------------------------------
C   Estimate the singleband delay uncertainty for a given bandwidth.
C   Assumed that the SNR is 1/sigma of the phase.
C   Uses assumption that ends of the bandpass have phase measurements
C   with uncertainties given by the SNR values.
C   Inputs:
C      BW      R    Bandwidth in Hz.
C      SNRREF  R    SNR of the reference antenna (>10 => ref in fit)
C      SNRREM  R    SNR of the remote antenna (>10 => ref in fit)
C   Output:
C      SIGSBD  R    Singleband delay uncertainty in usec.
C
      REAL BW, SNRREF, SNRREM, SNR, SPDIF
C-----------------------------------------------------------------------
C                                       SNR assumed to be 1/sigma of
C                                       phase unless one  > 10.  (Both >
C                                       10 implies this is actual SNR)
      IF ((SNRREF.GT.10.0) .AND. (SNRREM.GT.10)) THEN
C                                       Both high
         SNR = 0.5 * (SNRREF + SNRREM)
      ELSE IF  ((SNRREF.LE.10.0) .AND. (SNRREM.GT.10)) THEN
C                                       Ref low
         SNR = SNRREM
      ELSE IF  ((SNRREF.GT.10.0) .AND. (SNRREM.LE.10)) THEN
C                                       Rem. low
         SNR = SNRREF
      ELSE
C                                       Both low
         SNR = 0.5 * (SNRREF + SNRREM)
         END IF
C                                       Must have both
      IF ((SNRREF.LT.1.0E-10) .OR. (SNRREM.LT.1.0E-10)) SNR = 0.0
C                                       Sigma of phase difference
      SPDIF = SQRT (2.0) * SNR
      IF ((SPDIF.GT.1.0E-30) .AND. (BW.GT.1.0D-20)) THEN
         SPDIF = 1.0 / SPDIF
         SIGSBD = (SPDIF / BW) * 1.0E6
      ELSE
C                                        No valid value
         SIGSBD = 0.0
         END IF
 999  RETURN
      END
C
      REAL FUNCTION RESPHS (NUMIF, LIFNO, RC51, PHASE)
C----------------------------------------------------------------------
C   Average the Residual phases.
C   Inputs:
C      NUMIF  I     Number of IF's
C      LIFNO(*) I   The IF number of the valid channels
C      RC51   R(28) The amplitude, phase array for 14 IF's maximum
C      PHASE  R     Is the phase blanked?
C   Output:
C      RESPHS R     Average phase
C----------------------------------------------------------------------
      INTEGER  NUMIF
      REAL RC51(28), PHASE(NUMIF)
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   LOOP, COUNT, NIF, LIFNO(*), L
      REAL      SUMR, SUMI, DIF, RADDG
      DATA      RADDG / 0.017453293 /
C----------------------------------------------------------------------
      NIF = MIN (14, NUMIF)
C
      COUNT = 0
      SUMR = 0.0
      SUMI = 1.0E-20
      RESPHS = 0.0
      DO 100 L = 1,NIF
         LOOP = LIFNO(L)
         IF (PHASE(LOOP).NE.FBLANK) THEN
            COUNT = COUNT + 1
            DIF = RC51(2*L) * RADDG
            SUMR = SUMR + COS(DIF)
            SUMI = SUMI + SIN(DIF)
         ENDIF
 100     CONTINUE
      IF (COUNT.GT.0) RESPHS = ATAN2(SUMI/COUNT, SUMR/COUNT)
C
 999  RETURN
      END
C
      REAL FUNCTION AVGPHS (NUMIF, LIFNO, PHASE)
C-----------------------------------------------------------------------
C   Remove phase cals from phase residuals and average.
C    Modified by D. Lebach 970109, phase cals not removed.
C   Inputs:
C      NUMIF   I    Number of good IF's
C      LIFNO(*)I    The IF number for each good IF
C      PHASE   R    Phase residuals (magic value blanking allowed).
C!!!   PC      R    Phase cals corresponding to PHASE.
C   Output:
C      AVGPHS  R    Average corrected phase
C-----------------------------------------------------------------------
      INTEGER   NUMIF
C#    REAL      PHASE(NUMIF), PC(NUMIF)
      REAL      PHASE(*)
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   LOOP, COUNT, NIF, L, LIFNO(*)
      REAL      SUMR, SUMI
C     REAL      SUMR, SUMI, DIF
C-----------------------------------------------------------------------
      NIF = MIN (14, NUMIF)
C                                       Average non blanked corrected phase.
      COUNT = 0
      SUMR = 0.0
C#    SUMI = 1.0E-20
      SUMI = 0.0
      AVGPHS = 0.0
      DO 100 L = 1,NIF
        LOOP = LIFNO(L)
C#       IF ((PHASE(LOOP).NE.FBLANK) .AND. (PC(LOOP).NE.FBLANK)) THEN
        IF ( PHASE(LOOP).NE.FBLANK ) THEN
            COUNT = COUNT + 1
C#          DIF = (PHASE(LOOP) - PC(LOOP))
C#          SUMR = SUMR + COS (DIF)
C#          SUMI = SUMI + SIN (DIF)
            SUMR = SUMR + COS (PHASE(LOOP))
            SUMI = SUMI + SIN (PHASE(LOOP))
        END IF
 100  CONTINUE
C#    IF (COUNT.GT.0) AVGPHS = ATAN2 (SUMI/COUNT, SUMR/COUNT)
C   A MODIFICATION of 970109 (DEL) starts below (division by COUNT
C    not necessary):
      IF (COUNT.GT.0)  THEN
         IF (ABS(SUMR) .LT. 1.0E-20)  SUMR = SIGN(1.0E-20,SUMR)
         AVGPHS = ATAN2 (SUMI, SUMR)
      ENDIF
 999  RETURN
      END
C
      SUBROUTINE SNPHS (NUMIF, LIFNO, ANTPHS, FOFF, IPOL, REF, REM,
     *                 MAXIF, RC51, MBERR)
C-----------------------------------------------------------------------
C   Puts SN table phases into RC51 and computes multiband delay
C    slope error.
C   Inputs:
C      NUMIF     I  Number of good IF's
C      LIFNO(*)  I  The IF number for each good IF
C      MAXIF     I
C      ANTPHS(*) R  Phases from the SN table after removing multiband
C                    slope.
C      FOFF(*)   R  Frequencies of good channels.
C   Output:
C      RC51(2,*) R  Baseline channel phases.
C      MBERR     R  Multiband delay slope error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   NUMIF, MAXIF, REF, REM,IPOL
      REAL      ANTPHS(MAXIF,2,*), RC51(2,14), FMAX, MBERR
      DOUBLE PRECISION FOFF(*), TWOPI, XDBL
      INTEGER   LOOP, L, LIFNO(*)
      REAL       XPH(14), PH1, PHSUM, PHAVE, SQSUM, RMS
C
      DATA TWOPI /6.2831853071795865D0/
C
C-----------------------------------------------------------------------
C
       FMAX = FOFF(LIFNO(NUMIF)) - FOFF(LIFNO(1))
       DO 20 L = 1,14
        RC51(2,L) = 0.0
  20   CONTINUE
        PHSUM = 0.0
        SQSUM = 0.0
C
      DO 100 L = 1,NUMIF
        LOOP = LIFNO(L)
        IF (ANTPHS(LOOP,IPOL,REF) .NE. FBLANK .AND.
     *      ANTPHS(LOOP,IPOL,REM) .NE. FBLANK) THEN
         XPH(L) = ANTPHS(LOOP,IPOL,REF) - ANTPHS(LOOP,IPOL,REM)
         XDBL = XPH(L)
         XPH(L) = DMOD (XDBL, TWOPI) * 57.29577951
          IF (XPH(L) .GT.  180.) XPH(L) = XPH(L) - 360.
          IF (XPH(L) .LT. -180.) XPH(L) = XPH(L) + 360.
           IF(L.EQ.1) THEN
            PH1 = XPH(1)
           ELSE
            IF ((XPH(L)-PH1) .GT.  180.) XPH(L) = XPH(L) - 360.
            IF ((XPH(L)-PH1) .LT. -180.) XPH(L) = XPH(L) + 360.
           ENDIF
          RC51(2,L) = XPH(L)
          PHSUM = PHSUM + XPH(L)
        ENDIF
 100  CONTINUE
C
      PHAVE = PHSUM/NUMIF
C
      DO 200 L = 1,NUMIF
       SQSUM = SQSUM + (XPH(L)-PHAVE)**2
 200  CONTINUE
C
       RMS = SQRT(SQSUM/NUMIF)
       MBERR = RMS/(360.*FMAX)
       RC51(2,14) = MBERR*1.E12
C
C#    PRINT *,'SNPHS:',XPH(1),XPH(2),XPH(3),XPH(4),FMAX,PHAVE,RMS,
C#   *       MBERR*1.E12
C
 999  RETURN
      END
C
      SUBROUTINE FILLPC (NUMIF, LIFNO, PCREF, PCREM, PCREFA, PCREMA,
     *                   IC19)
C-----------------------------------------------------------------------
C   Fill phase cal array for HF table entry.
C   Inputs:
C      NUMIF   I    Number of frequencies involved.
C      LIFNO(*) I   The IF numbers which are good
C      PDREF   R(*) Phase cal (rad) for reference antenna / IF
C      PDREM   R(*) Phase cal (rad) for remote antenna / IF
C   Output:
C      IC19    I(3,2,*)  IC19(i,j,k)
C                        i = amp, phase*100 (-18000, 18000), freq(kHz)
C                        j = ref, rem antenna; k = IF number
C-----------------------------------------------------------------------
      INTEGER  NUMIF, LIFNO(*)
      INTEGER  IC19(3,2,14)
      REAL     PCREF(NUMIF), PCREM(NUMIF), PCREFA(NUMIF), PCREMA(NUMIF)
      INCLUDE 'INCS:DDCH.INC'
      INTEGER  K, PHASE, IROUND, L
C-----------------------------------------------------------------------
C                                       Loop over IF
      DO 100 L = 1,NUMIF
         K = LIFNO(L)
C                                       Reference antenna
         IF (PCREF(K).NE.FBLANK) THEN
C                                       Amplitudes = dummy
C           IC19(1,1,L) = 1
C                                       Real PCal amplitudes (*1000)
            IC19(1,1,L) = PCREFA(K) * 1000.0
C                                       Phase
            PHASE = IROUND (PCREF(K) * 5729.577951)
            PHASE = MOD (PHASE, 36000)
            IF (PHASE.GT.18000) PHASE = PHASE - 360000
            IC19(2,1,L) = PHASE
C                                       Rate = 10 kHz
            IC19(3,1,L) = 10
         ELSE
            IC19(1,1,L) = 0
            IC19(2,1,L) = 0
            IC19(3,1,L) = 10
            END IF
C                                       Remote antenna
         IF (PCREM(K).NE.FBLANK) THEN
C                                       Amplitudes = dummy
C           IC19(1,2,L) = 1
C                                       Real PCal amplitudes (*1000)
            IC19(1,2,L) = PCREMA(K) * 1000.0
C                                       Phase
            PHASE = IROUND (PCREM(K) * 5729.577951)
            PHASE = MOD (PHASE, 36000)
            IF (PHASE.GT.18000) PHASE = PHASE - 360000
            IC19(2,2,L) = PHASE
C                                       Rate = 10 kHz
            IC19(3,2,L) = 10
         ELSE
            IC19(1,2,L) = 0
            IC19(2,2,L) = 0
            IC19(3,2,L) = 10
            END IF
 100     CONTINUE
C
 999  RETURN
      END
C
      SUBROUTINE APHIF (NUMIF, LIFNO, RE, IM, ANTGAI, IPOL, REF, REM,
     *   RC51, SCLAMP, COHER)
C-----------------------------------------------------------------------
C   Fill visibility array for HF table entry.
C   Inputs:
C      NUMIF   I    Number of frequencies involved.
C      LIFNO(*)I    The IF values of the valid frquencies
C      RE      R(2,*) (1,if) = real part of correlation
C      IM      R(2,*) (1,if) = imaginary part of correlation
C      ANTGAI  R(MAXIF, 2, MXGANT) Antenna gains of reference antenna
C      IPOL    I    Pol number
C      REF     I    Reference antenna
C      REM     I    Remote antenna
C   Output:
C      RC51    R(2,14) Amplitude (correlation), phase (-180, 180 deg) per If.
C      SCLAMP  R       Scalar IF averaged amplitude.
C      COHER   R       Coherence level
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMIF, IPOL, REF, REM
      REAL      RE(2,NUMIF), IM(2,NUMIF), RC51(2,14), SCLAMP, COHER
      REAL      ANTGAI(MAXIF, 2, *)
      INTEGER   NIF, LOOP, SCLCNT, L, LIFNO(*)
      REAL      AMP, PH, CAMP
C-----------------------------------------------------------------------
      NIF = MIN (14, NUMIF)
      SCLAMP = 0.0
      SCLCNT = 0
      COHER = 0.0
C                                       Loop over IF
      DO 100 L = 1,NIF
         LOOP = LIFNO(L)
C                                       Convert to amp, phase
         AMP = SQRT (RE(1,LOOP)*RE(1,LOOP) + IM(1,LOOP)*IM(1,LOOP))
         CAMP = AMP / ANTGAI(LOOP, IPOL, REF)
     *      / ANTGAI(LOOP, IPOL, REM)
         PH = ATAN2 (IM(1,LOOP), RE(1,LOOP)+1.0E-20) * 57.29577951
         IF (PH.GT.180.0) PH = PH - 360.0
         IF (PH.LT.-180.0) PH = PH + 360.0
         RC51(1,L) = AMP
         RC51(2,L) = PH
         IF (AMP.GT.1.0E-20) THEN
            SCLAMP = SCLAMP + AMP
            COHER = COHER + CAMP
            SCLCNT = SCLCNT + 1
            END IF
 100     CONTINUE
      IF (SCLCNT.GT.1) SCLAMP = SCLAMP / SCLCNT
      IF (SCLCNT.GT.1) COHER = COHER / SCLCNT
C
 999  RETURN
      END
C
      SUBROUTINE QCODE (NUMIF, LIFNO, VCOUNT, MAXSMP, MINSMP, FRACT,
     *                  CC33, QB)
C-----------------------------------------------------------------------
C   Fill quality code for HF table entry.
C   -Modified by D. Lebach 961231. Haystack QB factor added.
C
C   If some IFs have insufficient data then code='A'
C   else code = 10*(avg cnt/MAXSMP)
C   Inputs:
C      NUMIF   I    Number of frequencies involved.
C      LINFO   I(*) IF value of each valid frequency
C      VCOUNT  R(2,if) (2,if) = IF time sample count
C      MAXSMP  I    Maximum possible number of time samples for this scan
C      MINSMP  I    Minimum acceptable time samples.
C   Output:
C      FRACT   R    Fraction of good time samples.
C      CC33    C*1  Quality code.
C      QB      R    Haystack "QB" factor (ratio of min. to max. data
C                   accepted over the IFs)
C-----------------------------------------------------------------------
      INTEGER   NUMIF, MAXSMP, MINSMP, LIFNO(*)
      REAL      VCOUNT(2,NUMIF), QB
      CHARACTER CC33*1
      INTEGER   LOOP, INDEX, COUNT, L
      REAL      SUM, FRACT
      REAL      IFMNSP,IFMXSP
      CHARACTER CODES*10
      DATA CODES /'0123456789'/
C-----------------------------------------------------------------------
C     CC33 = 'A'
      CC33 = ' '
C                                       Loop over IF, make sure IFs OK
      SUM = 0.0
      COUNT = 0
      IFMNSP = MAXSMP
      IFMXSP = 0.0
      DO 100 L = 1,NUMIF
         LOOP = LIFNO(L)
         IF (VCOUNT(2,LOOP).LT.MINSMP)  CC33 = 'A'
         IF (VCOUNT(2,LOOP).LT.IFMNSP)  IFMNSP = VCOUNT(2,LOOP)
         IF (VCOUNT(2,LOOP).GT.IFMXSP)  IFMXSP = VCOUNT(2,LOOP)
C        print *, 'IF #, Counts ', LOOP, VCOUNT(2,LOOP)
C                                       Accumulate counts
         COUNT = COUNT + 1
         SUM = SUM + VCOUNT(2,LOOP)
 100     CONTINUE
C                                       Get average fraction
      FRACT = SUM / (COUNT * MAXSMP)
      INDEX = 10.0 * FRACT + 1.01
      INDEX = MAX (INDEX, 1)
      INDEX = MIN (INDEX, 10)
C                                       Set quality code
      IF (CC33 .EQ. ' ')  CC33 = CODES(INDEX:INDEX)
C
C                                       Compute QB factor.
      IF (IFMXSP .GT. 0.0)  THEN
         QB = 100.0*IFMNSP/IFMXSP
      ELSE
         QB = 0.0
      END IF
C
 999  RETURN
      END
C
      SUBROUTINE XANCOD(A1, A2, C1, C37, C38)
C-----------------------------------------------------------------------
C    This subroutine determines the Haystack baseline code C1 from
C     the names of the telescopes A1, A2.
C
C   INPUTS:
C         A1     C*8     The reference antenna name
C         A2     C*8     The remote antenna name
C         C1     C*2     The baseline code
C         C37    C*8     The reference antenna occupation code
C         C38    C*8     The remote antenna occupation code
C
      CHARACTER  A1*8, A2*8, C1*2, C37*8, C38*8
      INTEGER  NANT, I
      CHARACTER*8 ANAME(3, 10), BNAME(3,10), CNAME(3,10), FCODE(3,30),
     *            OCODE(30)
      CHARACTER*1 TNAME(30), T1, T2
C
      EQUIVALENCE (ANAME(1,1), FCODE(1,1)),
     *           (BNAME(1,1), FCODE(1,11)),
     *           (CNAME(1,1), FCODE(1,21))
C
      DATA  ANAME /'BR      ','VLBA_BR ','VLBA-BR ',                        ! 1
     *             'FD      ','VLBA_FD ','VLBA-FD ',                        ! 2
     *             'HN      ','VLBA_HN ','VLBA-HN ',                        ! 3
     *             'KP      ','VLBA_KP ','VLBA-KP ',                        ! 4
     *             'LA      ','VLBA_LA ','VLBA-LA ',                        ! 5
     *             'MK      ','VLBA_MK ','VLBA-MK ',                        ! 6
     *             'NL      ','VLBA_NL ','VLBA-NL ',                        ! 7
     *             'OV      ','VLBA_OV ','VLBA-OV ',                        ! 8
     *             'PT      ','VLBA_PT ','PIETOWN ',                        ! 9
     *             'SC      ','VLBA_SC ','VLBA-SC ' /                       !10
      DATA  BNAME /'Y       ','VLA     ','VL      ',                        !11
     *             'GB      ','NRAO 140','NRG     ',                        !12
     *             'WF      ','WESTFORD','        ',                        !13
     *             'ON      ','ONSALA60','        ',                        !14
     *             'D5      ','DUMMY5  ','        ',                        !15
     *             'JB      ','JODDRELL','        ',                        !16
     *             'EB      ','EFLSBERG','        ',                        !17
     *             'HAK     ','HAYSTACK','        ',                        !18
     *             'TC      ','TIGOCONC','        ',                        !19
     *             'NT      ','NOTO    ','        ' /                       !20
      DATA  CNAME /'GC      ','GILCREEK','        ',                        !21
     *             'KK      ','KOKEE   ','        ',                        !22
     *             'GN      ','NRAO20  ','        ',                        !23
     *             'GG      ','GGAO7108','        ',                        !24
     *             'NY      ','NYALES20','        ',                        !25
     *             'MC      ','MEDICINA','        ',                        !26
     *             'HH      ','HARTRAO ','        ',                        !27
     *             'WZ      ','WETTZELL','        ',                        !28
     *             'TS      ','TSUKUB32','        ',                        !29
     *             'MA      ','MATERA  ','        ' /                       !30
      DATA  TNAME /'A','B','C','D','E','F','G','H','I','J',
     *             'K','L','M','N','O','P','Q','R','S','T',
     *             'U','V','W','X','Y','Z','1','2','3','4' /
      DATA  OCODE /'76149901','76139801','76185001','76109401',             ! 4
     *             '76119601','76175501','76129701','76165401',             ! 8
     *             '72348601','76159001','00000000','72048001',             !12
     *             '72097301','72137701','00000000','00000000',             !16
     *             '72037901','72057401','00000000','75478901',             !20
     *             '40476601','72983001','72484701','71085301',             !24
     *             '73313301','72308801','72326201','72247801',             !28
     *             '73452301','00000000'/
C
      NANT = 30
      DO 100 I = 1, NANT
         IF ((A1 .EQ. FCODE(1,I)) .OR.
     *       (A1 .EQ. FCODE(2,I)) .OR.
     *       (A1 .EQ. FCODE(3,I))) GO TO 110
 100     CONTINUE
      T1 = '?'
      C37 = '00000000'
      GO TO 120
 110  T1 = TNAME(I)
      C37 = OCODE(I)
 120  DO 200 I = 1, NANT
         IF ((A2 .EQ. FCODE(1,I)) .OR.
     *       (A2 .EQ. FCODE(2,I)) .OR.
     *       (A2 .EQ. FCODE(3,I))) GO TO 210
 200     CONTINUE
      T2 = '?'
      C38 = '00000000'
      GO TO 220
 210  T2 = TNAME(I)
      C38 = OCODE(I)
 220  CONTINUE
C
      C1 = T1 // T2
      RETURN
      END
C     SUBROUTINE UVFRQQ (UVDATA, PIXREF, PIXNUM, UVFREQ, IERR)
C-----------------------------------------------------------------------
C   UVdata class utility routine
C   Returns reference (u,v,w) frequency and a pixel number information.
C   Inputs:
C      UDATA   C*?  Name of uvdata.
C   Output:
C      UVFREQ  D    u,v,w reference frequency
C      PIXNUM  R    Number of pixels (spectral channels) in each IF
C      PIXREF  R    The reference pixel number location set in the UV header
C                   (where the data was fringed)
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
C     CHARACTER UVDATA*(*)
C     DOUBLE PRECISION UVFREQ
C     INTEGER TYPE, DIM(7), NAXIS(7), IERR
C     REAL PIXREF, PIXNUM
C     CHARACTER CDUMMY*1
C     INCLUDE 'INCS:PUVD.INC'
C     INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C     IERR = 0
C
C       CALL OUVOPN(UVDATA,'READ',IERR)
C       IF (IERR.NE.0) GO TO 990
C
C     DIM(1) = 16
C     DIM(2) =  1
C     DIM(3) =  0
C
C  NAXIS(3) = Number of pixels per IF
C     CALL UVDGET (UVDATA, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
C     IF (IERR.NE.0) GO TO 990
C      PIXNUM = NAXIS(3)
C                                       Reference frequency (at lower
C                                       edge of IF #1)
C     CALL UVDGET (UVDATA, 'REFFREQ', TYPE, DIM, UVFREQ, CDUMMY, IERR)
C     IF (IERR.NE.0) GO TO 990
C                                       Reference pixel number (for
C                                       phase referencing)
C     CALL UVDGET (UVDATA, 'REFFPIX', TYPE, DIM, PIXREF, CDUMMY, IERR)
C     IF (IERR.NE.0) GO TO 990
C
C       CALL OCLOSE (UVDATA,IERR)
C       IF (IERR.NE.0) GO TO 990
C
C     GO TO 999
C                                       Error
C990  CONTINUE
C     MSGTXT = 'UVFRQQ: ERROR FINDING FREQUENCIES FOR ' // UVDATA
C     CALL MSGWRT (8)
C
C999  RETURN
C     END
C*****************************************************************
C
      DOUBLE PRECISION FUNCTION IFINC (IFFREQ, NIF)
C-----------------------------------------------------------------------
C   The grid spacing for IF frequencies.
C
C   [Borrowed from MBDLY]
C
C   Inputs:
C      IFFREQ   D(*)     IF reference frequencies
C      NIF      I        Number of IFs
C-----------------------------------------------------------------------
C     DOUBLE PRECISION IFFREQ(*)
      DOUBLE PRECISION IFFREQ(8)
      INTEGER   NIF
C
C   TOL      D        Tolerance used in testing for termination (Hz)
C
      DOUBLE PRECISION TOL
      PARAMETER (TOL = 10.0)
      INTEGER   IMAX, I
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION D(MAXIF), DIFF, MAX, MIN
C
      INTEGER   IDAMAX
      DOUBLE PRECISION DMIN
C-----------------------------------------------------------------------
C                                       Compute the frequency
C                                       differences and use a
C                                       generalized form of Euclid's
C                                       algorithm for finding their
C                                       gcd.
      DO 10 I = 1, NIF - 1
         D(I) = DABS(IFFREQ(I+1) - IFFREQ(I))
   10    CONTINUE
C
   20 CONTINUE
C                                        Find maximum and minimum
C                                        differences
         IMAX = IDAMAX (D, NIF - 1)
         MAX = D(IMAX)
         MIN = DMIN (D, NIF - 1)
C                                        Find range of differences:
         DIFF = MAX - MIN
C                                        Test for termination:
         IF (DIFF.LE.TOL) GO TO 40
C                                        Reduce maximum by minimum
         D(IMAX) = DIFF
         GO TO 20
   40 CONTINUE
C                                        The GCD is what we want:
      IFINC = D(1)
      END
      INTEGER FUNCTION IDAMAX (S, N)
C-----------------------------------------------------------------------
C The index of the first minimum in a single-precision vector
C S of length N.  Compatible with the BLAS1 routine.
C
C Inputs:
C    S     D(*)       The vector
C    N     I          The length of the vector
C-----------------------------------------------------------------------
      DOUBLE PRECISION S(*)
      INTEGER   N
C
      INTEGER   I
      DOUBLE PRECISION MAX
C-----------------------------------------------------------------------
      IDAMAX = 1
      MAX = S(1)
      DO 10 I = 2, N
         IF (S(I).GT.MAX) THEN
            MAX = S(I)
            IDAMAX = I
            END IF
   10    CONTINUE
C
      END
      DOUBLE PRECISION FUNCTION DMIN (D, N)
C-----------------------------------------------------------------------
C Return the minimum value in a double precision vector of length N
C
C Inputs:
C    D       D(*)       The vector
C    N       I          The number of elements in the vector
C-----------------------------------------------------------------------
      DOUBLE PRECISION D(*)
      INTEGER   N
C
      INTEGER   I
C-----------------------------------------------------------------------
      DMIN = D(1)
      DO 10 I = 2, N
         IF (D(I).LT.DMIN) DMIN = D(I)
   10    CONTINUE
      END

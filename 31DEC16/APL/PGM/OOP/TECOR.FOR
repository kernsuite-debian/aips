      PROGRAM TECOR
C-----------------------------------------------------------------------
C! Use TEC maps to calibrate ionospheric effects
C# Task Calibration VLA VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 1998-2000, 2001, 2003, 2005, 2010-2011, 2013, 2015
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
C   TECOR uses maps of ionospheric total electron content (TEC) to
C   determine corrections for ionospheric Faraday rotation and, at the
C   user's option, dispersive delay.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
C
C   Local Variables
C
C      IRET    Subroutine return status
C      UVFILE  Name of UVDATA object used to access AIPS data file
C      ICLTAB  Name of TABLE object used to access input CL table
C      OCLTAB  Name of TABLE object used to access output CL table
C      INPUTS  Name of INPUTS object used to access input adverbs
C      SUBARR  Selected subarray number or zero
C      NANTS   Number of antenna selections
C      ANTENS  List of selected antennas
C      ANTREJ  Should antenna in ANTENS be rejected?
C      DODDEL  Should dispersive delay corrections be calculated?
C      LFACT   Follow rotation of ionosphere (1) to not at all (0)
C      OUTVER  Version number of output CL table
C      NUMIF   Number of IFs in CL tables
C      NUMPOL  Number of polarizations in CL tables
C      NUMMOD  Number of CL rows that were mdofied
C      WORK    Workspace for DIE
C
      INTEGER   IRET
C
      CHARACTER UVFILE*7, ICLTAB*14, OCLTAB*15, INPUTS*13
      PARAMETER (UVFILE = 'UV file')
      PARAMETER (ICLTAB = 'Input CL table')
      PARAMETER (OCLTAB = 'Output CL table')
      PARAMETER (INPUTS = 'Input adverbs')
C
      INTEGER   SUBARR, NANTS
      INTEGER   ANTENS(MAXANT)
      LOGICAL   ANTREJ
C
      LOGICAL   DODDEL
      REAL      LFACT
C
      INTEGER   OUTVER, NUMIF, NUMPOL
C
      INTEGER   NUMMOD
      INTEGER   WORK(256)
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      CALL TECINI (ICLTAB, OCLTAB, UVFILE, INPUTS, SUBARR, NANTS,
     *   ANTENS, ANTREJ, DODDEL, LFACT, OUTVER, NUMIF, NUMPOL, IRET)
C                                       IRET = 0 implies SUBARR >= 0
C                                       IRET = 0 implies
C                                                0 <= NANTS <= MAXANT
C                                       IRET = 0 implies
C                                                ANTENS(1:NANTS) > 0
C                                       IRET = 0 implies OUTVER >= 2
C                                       IRET = 0 implies
C                                                1 <= NUMIF <= MAXIF
C                                       IRET = 0 implies
C                                                1 <= NUMPOL <= 2
      IF (IRET .EQ. 0) THEN
         CALL TECPRO (ICLTAB, OCLTAB, UVFILE, SUBARR, NANTS, ANTENS,
     *      ANTREJ, DODDEL, LFACT, NUMIF, NUMPOL, NUMMOD, IRET)
C                                       IRET = 0 implies NUMMOD >= 0
         IF (IRET .EQ. 0) THEN
            CALL TECHIS (OCLTAB, INPUTS, DODDEL, LFACT, OUTVER, NUMMOD,
     *         IRET)
            IF (IRET .NE. 0) THEN
               MSGTXT = 'TECOR: FAILED TO UPDATE HISTORY FILE'
               CALL MSGWRT (9)
            END IF
         ELSE
            MSGTXT = 'TECOR: FAILED TO GENERATE CORRECTIONS'
            CALL MSGWRT (9)
         END IF
      ELSE
         MSGTXT = 'TECOR: FAILED TO INITIALIZE PROGRAM'
         CALL MSGWRT (9)
      END IF
C
      CALL DIE (IRET, WORK)
      END
      SUBROUTINE TECINI (ICLTAB, OCLTAB, UVFILE, INPUTS, SUBARR, NANTS,
     *   ANTENS, ANTREJ, DODDEL, LFACT, OUTVER, NUMIF, NUMPOL, IRET)
C----------------------------------------------------------------------
C   Read the input adverbs and restart AIPS; open the input and output
C   calibration tables and initialize the module that interprets IONEX
C   files.
C
C   Inputs:
C      ICLTAB   C*(*)       The name of the TABLE object used to access
C                            the input CL table
C      OCLTAB   C*(*)       The name of the TABLE object used to access
C                            the output CL table
C      UVFILE   C*(*)       The name of the UVDATA object used to
C                            access the AIPS data file
C      INPUTS   C*(*)       The name of the INPUTS data object used to
C                            access the input adverbs
C
C   Outputs:
C      SUBARR   I           The number of the subarray to correct or
C                            zero if all subarrays are to be corrected
C      NANTS    I           The number of antennas in the selection
C                            list
C      ANTENS   I(*)        The antenna selection list
C      ANTREJ   L           Are antennas in the selection list to be
C                            rejected?
C      DODDEL   L           Are dispersive delay corrections to be
C                            calculated?
C      LFACT    R           Follow rotation of ionsphere factor
C      OUTVER   I           Version number of the output CL table
C      NUMIF    I           Number of IFs in the CL tables
C      NUMPOL   I           Number of polarizations in the CL tables
C      IRET     I           Return status
C                            0 - task initialized
C                            1 - failed to initialize output table
C                            2 - failed to initialize IONEX module
C                            3 - failed to initialize dispersive
C                                 delay flag
C                            4 - failed to initialize antenna list
C                            5 - failed to initialize input table
C                            6 - failed to read adverbs
C
C   Preconditions:
C      ICLTAB is not blank and is unique
C      OCLTAB is not blank and is unique
C      UVFILE is not blank and is unique
C      INPUTS is not blank and is unique
C      ANTENS has at least MAXANT entries
C
C   Postconditions
C      IRET = 0 implies ICLTAB is open read-only
C      IRET = 0 implies OCLTAB is open for writing and is empty
C      IRET = 0 implies UVFILE has been initialized
C      IRET = 0 implies INPUTS has been initialized
C      IRET = 0 implies SUBARR >= 0
C      IRET = 0 implies 0 <= NANTS <= MAXANT
C      IRET = 0 implies ANTENS(1:NANTS) > 0
C      IRET = 0 implies OUTVER is the version number of OCLTAB
C      IRET = 0 implies OUTVER > 1
C      IRET = 0 implies NUMIF is the number of IFs in ICLTAB and OCLTAB
C      IRET = 0 implies NUMPOL is the number of polarizations in ICLTAB
C                       and OCLTAB
C-----------------------------------------------------------------------
      CHARACTER ICLTAB*(*), OCLTAB*(*), UVFILE*(*), INPUTS*(*)
      INTEGER   SUBARR, NANTS, ANTENS(*)
      LOGICAL   ANTREJ, DODDEL
      REAL      LFACT
      INTEGER   OUTVER, NUMIF, NUMPOL, IRET
C
      INCLUDE 'INCS:PAOOF.INC'
C
C     Local variables
C
C     TASKNM    task name
C     NPARMS    number of adverbs
C     AVNAME    list of adverb names
C     AVTYPE    list of adverb type codes
C     AVDIM     list of adverb dimensions
C     FIRSCL    time of earliest record in ICLTAB
C     LASTCL    time of latest record in ICLTAB
C     NUMANT    number of antennas listed in ICLTAB
C     NTERM     number of polynomial delay terms in ICLTAB
C     GMMOD     mean gain modulus of corrections in ICLTAB
C     IRET2     temporary status value
C
      CHARACTER TASKNM*6
      PARAMETER (TASKNM = 'TECOR ')
      INTEGER   NPARMS
      PARAMETER (NPARMS = 11)
      CHARACTER AVNAME(NPARMS)*8
      INTEGER   AVTYPE(NPARMS), AVDIM(2, NPARMS)
C
      DOUBLE PRECISION FIRSCL, LASTCL
      INTEGER   NUMANT, NTERM
      REAL      GMMOD
      INTEGER   IRET2
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA AVNAME /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *             'INFILE  ', 'NFILES  ', 'SUBARR  ', 'ANTENNAS',
     *             'GAINVER ', 'GAINUSE ', 'APARM   '/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT,
     *             OOACAR, OOAINT, OOAINT, OOAINT,
     *             OOAINT, OOAINT, OOARE /
      DATA AVDIM / 12, 1,    6, 1,    1, 1,   1, 1,
     *             48, 1,    1, 1,    1, 1,   50, 1,
     *              1, 1,    1, 1,   10, 1 /
C-----------------------------------------------------------------------
      CALL AV2INP (TASKNM, NPARMS, AVNAME, AVTYPE, AVDIM, INPUTS, IRET)
C                                       IRET = 0 implies
C                                                INPUTS initialized
      IF (IRET .EQ. 0) THEN
         CALL TECITB (INPUTS, UVFILE, ICLTAB, NUMANT, NUMIF, NUMPOL,
     *                NTERM, GMMOD, FIRSCL, LASTCL, IRET)
C                                       IRET = 0 implies
C                                                UVFILE initialized
C                                       IRET = 0 implies
C                                                ICLTAB open read-only
C                                       IRET = 0 implies NUMANT is
C                                                number of antennas in
C                                                ICLTAB
C                                       IRET = 0 implies NUMIF is
C                                                number of IFs in
C                                                ICLTAB
C                                       IRET = 0 implies
C                                                1 <= NUMIF <= MAXIF
C                                       IRET = 0 implies NUMPOL is
C                                                number of polarizations
C                                                in ICLTAB
C                                       IRET = 0 implies
C                                                1 <= NUMPOL <= 2
C                                       IRET = 0 implies GMMOD is mean
C                                                gain modulus for
C                                                ICLTAB
C                                       IRET = 0 implies FIRSCL is
C                                                earliest time in ICLTAB
C                                       IRET = 0 implies LASTCL is
C                                                latest time in ICLTAB
         IF (IRET .EQ. 0) THEN
            CALL TECANT (INPUTS, SUBARR, NANTS, ANTENS, ANTREJ, IRET)
C                                       IRET = 0 implies SUBARR >= 0
C                                       IRET = 0 implies
C                                                0 <= NANTS <= MAXANT
C                                       IRET = 0 implies
C                                                ANTENS(1:NANTS) > 0
            IF (IRET .EQ. 0) THEN
               CALL TECDEL (INPUTS, DODDEL, LFACT, IRET)
               IF (IRET .EQ. 0) THEN
                  CALL TECION (INPUTS, FIRSCL, LASTCL, IRET)
C                                       IRET = 0 implies IONEX module
C                                                initialized
C                                       IRET = 0 implies earliest TEC
C                                                map precedes FIRSCL
C                                       IRET = 0 implies latest TEC
C                                                map follows LASTCL
                  IF (IRET .EQ. 0) THEN
                     CALL TECOTB (INPUTS, UVFILE, OCLTAB, NUMANT, NUMIF,
     *                            NUMPOL, NTERM, GMMOD, OUTVER, IRET)
C                                       IRET = 0 implies OCLTAB is open
C                                                for writing
C                                       IRET = 0 implies OCLTAB is
C                                                empty
C                                       IRET = 0 implies OUTVER >= 2
C                                       IRET = 0 implies OCLTAB has
C                                                NUMANT antennas
C                                       IRET = 0 implies OCLTAB has
C                                                NUMIF IFs
C                                       IRET = 0 implies OCLTAB has
C                                                NUMPOL polarizations
C                                       IRET = 0 implies OCLTAB has
C                                                NTERM polynomial terms
C                                       IRET = 0 implies OCLTAB has a
C                                                mean gain modulus of
C                                                GMMOD
                     IF (IRET .NE. 0) THEN
                        WRITE (MSGTXT, 9001) IRET
                        CALL MSGWRT (9)
                        IRET = 1
                     END IF
                  ELSE
                     WRITE (MSGTXT, 9002) IRET
                     CALL MSGWRT (9)
                     IRET = 2
                  END IF
               ELSE
                  WRITE (MSGTXT, 9003) IRET
                  CALL MSGWRT (9)
                  IRET = 3
               END IF
            ELSE
               WRITE (MSGTXT, 9004) IRET
               CALL MSGWRT (9)
               IRET = 4
            END IF
C
C           Force closure of input table if anything is wrong:
C
            IF (IRET .NE. 0) THEN
               CALL TABCLO (ICLTAB, IRET2)
            END IF
         ELSE
            WRITE (MSGTXT, 9005) IRET
            CALL MSGWRT (9)
            IRET = 5
         END IF
      ELSE
         WRITE (MSGTXT, 9006) IRET
         CALL MSGWRT (9)
         IRET = 6
      END IF
C-----------------------------------------------------------------------
 9001 FORMAT ('TECINI: ERROR ', I4, ' INITIALIZING OUTPUT CL TABLE')
 9002 FORMAT ('TECINI: ERROR ', I4, ' INITIALIZING IONEX MODULE')
 9003 FORMAT ('TECINI: ERROR ', I4, ' INITIALIZING DELAY FLAG')
 9004 FORMAT ('TECINI: ERROR ', I4, ' INITIALIZING ANTENNA LIST')
 9005 FORMAT ('TECINI: ERROR ', I4, ' INITIALIZING INPUT CL TABLE')
 9006 FORMAT ('TECINI: ERROR ', I4, ' INITIALIZING TASK')
      END
      SUBROUTINE TECITB (INPUTS, UVFILE, ICLTAB, NUMANT, NUMIF, NUMPOL,
     *                   NTERM, GMMOD, FIRSCL, LASTCL, IRET)
C-----------------------------------------------------------------------
C   Initialize the input CL table.
C
C   Inputs:
C      INPUTS   C*(*)       Name of INPUTS object used to access adverbs
C      UVFILE   C*(*)       Name of UVDATA object used to access AIPS
C                            data file
C      ICLTAB   C*(*)       Name of TABLE object used to access input CL
C                            table
C
C   Outputs:
C      NUMANT   I           Number of antennas in CL table
C      NUMIF    I           Number of IFs in CL table
C      NUMPOL   I           Number of polarizations in CL table
C      NTERM    I           Number of polynomial terms in CL table
C      GMMOD    R           Mean gain modulus for CL table
C      FIRSCL   D           Earliest time in CL table (Julian date)
C      LASTCL   D           Latest time in CL table (Julian date)
C      IRET     I           Return status
C                              0 - table initialized
C                              1 - error reading table
C                              2 - error opening table
C                              3 - error obtaining reference date
C                            999 - logic flaw
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVFILE*(*), ICLTAB*(*)
      INTEGER   NUMANT, NUMIF, NUMPOL, NTERM
      REAL      GMMOD
      DOUBLE PRECISION FIRSCL, LASTCL
      INTEGER   IRET
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables
C
C     NUMKEY      Number of UVDATA keywords to initialize from adverbs
C     INKEY       List of adverbs used to initialize UVDATA object
C     OUTKEY      List of UVDATA keywords to receive adverb values
C     INVER       Input table version number
C     NUMROW      Number of rows in CL table
C     CLROW       Current row in CL table
C     TIME        CL row time
C     TIMEI       CL row time interval
C     SOURID      CL row source ID
C     ANTNO       CL row antenna number
C     SUBA        CL row subarray number
C     FREQID      CL row frequency ID
C     IFR         CL row ionospheric Faraday rotation
C     GEODLY      CL row geometric delay polynomial
C     DOPOFF      CL row Doppler offsets
C     ATMOS       CL row atmospheric group delays
C     DATMOS      CL row atmospheric group delay derivatives
C     MBDLY       CL row multiband delays
C     CLOCK       CL row clock offsets
C     DCLOCK      CL row clock drift rates
C     DISP        CL row dispersive delays
C     DDISP       CL row dispersive delay rates
C     CREAL       CL row real gains
C     CIMAG       CL row imaginary gains
C     DELAY       CL row delays
C     RATE        CL row rates
C     WEIGHT      CL row weights
C     REFA        CL row reference antennas
C     DATOBS      Observing date
C     JD0         Julian date at midnight on observing date
C     TYPE        Attribute type code
C     DIM         Attribute dimensions
C     IDUMMY      Dummy numeric argument
C     CDUMMY      Dummy character argument
C     IRET2       Temporary subroutine exit status
C
      INTEGER   NUMKEY
      PARAMETER (NUMKEY = 4)
      CHARACTER INKEY(NUMKEY)*8, OUTKEY(NUMKEY)*16
      INTEGER   INVER, NUMROW, CLROW
      DOUBLE PRECISION TIME
      REAL      TIMEI
      INTEGER   SOURID, ANTNO, SUBA, FREQID
      REAL      IFR
      DOUBLE PRECISION GEODLY(12)
      REAL      DOPOFF(MAXIF), ATMOS(2), DATMOS(2), MBDLY(2)
      REAL      CLOCK(2), DCLOCK(2), DISP(2), DDISP(2)
      REAL      CREAL(2, MAXIF), CIMAG(2, MAXIF)
      REAL      DELAY(2, MAXIF), RATE(2, MAXIF), WEIGHT(2, MAXIF)
      INTEGER   REFA(2, MAXIF)
C
      CHARACTER DATOBS*8
      DOUBLE PRECISION JD0
C
      INTEGER   IDUMMY, TYPE, DIM(3), IRET2
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA INKEY /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  '/
      DATA OUTKEY /'FILE_NAME.NAME  ', 'FILE_NAME.CLASS ',
     *             'FILE_NAME.IMSEQ ', 'FILE_NAME.DISK'/
C-----------------------------------------------------------------------
      CALL OUVCRE (UVFILE, IRET)
      CALL CHECK ('TECITB', 1, IRET .EQ. 0, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL IN2OBJ (INPUTS, NUMKEY, INKEY, OUTKEY, UVFILE, IRET)
      CALL CHECK ('TECITB', 2, IRET .EQ. 0, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL INGET (INPUTS, 'GAINVER', TYPE, DIM, INVER, CDUMMY, IRET)
      CALL CHECK ('TECITB', 3, (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                         .AND. (DIM(1) .EQ. 1)
     *                         .AND. (DIM(2) .EQ. 1), IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Override negative values of INVER:
C
      IF (INVER .LT. 0) THEN
         INVER = 0
      END IF
C                                       INVER >= 0
      CALL UV2TAB (UVFILE, ICLTAB, 'CL', INVER, IRET)
      CALL CHECK ('TECITB', 4, IRET .EQ. 0, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL OCLINI (ICLTAB, 'READ', CLROW, NUMANT, NUMPOL, NUMIF, NTERM,
     *             GMMOD, IRET)
C                                       IRET = 0 implies CLROW = 1
C                                       IRET = 0 implies NUMANT >= 0
C                                       IRET = 0 implies
C                                                1 <= NUMPOL <= 2
C                                       IRET = 0 implies
C                                                1 <= NUMIF <= MAXIF
C                                       IRET = 0 implies NTERM >= 0
      IF (IRET .EQ. 0) THEN
         CALL TABGET (ICLTAB, 'VER', TYPE, DIM, INVER, CDUMMY, IRET)
         CALL CHECK ('TECITB', 5, (IRET .EQ. 0)
     *                            .AND. (TYPE .EQ. OOAINT)
     *                            .AND. (DIM(1) .EQ. 1)
     *                            .AND. (DIM(2) .EQ. 1), IRET)
         CALL INPUTT (INPUTS, 'GAINVER', TYPE, DIM, INVER, CDUMMY, IRET)
         WRITE (MSGTXT, 1000) INVER
         CALL MSGWRT (5)
         WRITE (MSGTXT, 1001)
         CALL MSGWRT (5)
         CALL TABGET (ICLTAB, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
         CALL CHECK ('TECITB', 6, (IRET .EQ. 0)
     *                            .AND. (TYPE .EQ. OOAINT)
     *                            .AND. (DIM(1) .EQ. 1)
     *                            .AND. (DIM(2) .EQ. 1), IRET)
         FIRSCL = +9999.0D0
         LASTCL = -9999.0D0
C
C        Invariant: IRET = 0 implies FIRSCL is the earliest unflagged
C                            time in the first CLROW - 1 rows of ICLTAB
C                            and LASTCL is the lastest unflagged time
C                            in the first CLROW -1 rows of ICLTAB
C        Bound: NUMROW - CLROW + 1
C
   10    IF ((IRET .EQ. 0) .AND. (CLROW .NE. (NUMROW + 1))) THEN
            CALL OTABCL (ICLTAB, 'READ', CLROW, NUMPOL, NUMIF, TIME,
     *                   TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *                   GEODLY, DOPOFF, ATMOS, DATMOS, MBDLY, CLOCK,
     *                   DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *                   WEIGHT, REFA, IRET)
            IF (IRET .EQ. 0) THEN
               IF (TIME .LT. FIRSCL) THEN
                  FIRSCL = TIME
               END IF
               IF (TIME .GT. LASTCL) THEN
                  LASTCL = TIME
               END IF
            ELSE IF (IRET .EQ. -1) THEN
C
C              Ignore flagged records:
C
               IRET = 0
            ELSE
               WRITE (MSGTXT, 9010) IRET
               CALL MSGWRT (9)
               IRET = 1
               CALL OTABCL (ICLTAB, 'CLOS', CLROW, NUMPOL, NUMIF, TIME,
     *                      TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *                      GEODLY, DOPOFF, ATMOS, DATMOS, MBDLY, CLOCK,
     *                      DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY,
     *                      RATE, WEIGHT, REFA, IRET2)
            END IF
            GO TO 10
         END IF
         WRITE (MSGTXT, 1010)
         CALL MSGWRT (5)
      ELSE
         WRITE (MSGTXT, 9011) IRET
         CALL MSGWRT (9)
         IRET = 2
      END IF
C
C     Add Julian day offset to times
C
      CALL OUVATT (UVFILE, .FALSE., IRET)
      IF (IRET. EQ. 0) THEN
         CALL UVDGET (UVFILE, 'DATE-OBS', TYPE, DIM, IDUMMY, DATOBS,
     *                IRET)
         CALL CHECK ('TECITB', 7, (IRET .EQ. 0) .AND. (TYPE .EQ. OOACAR)
     *                            .AND. (DIM(1) .EQ. 8)
     *                            .AND. (DIM(2) .EQ. 1), IRET)
         IF (IRET .NE. 0) GO TO 999
         CALL JULDAY (DATOBS, JD0)
         FIRSCL = FIRSCL + JD0
         LASTCL = LASTCL + JD0
      ELSE
         WRITE (MSGTXT, 9012) IRET
         CALL MSGWRT (9)
         IRET = 3
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Reading from CL table version number ', I4)
 1001 FORMAT ('Searching for earliest and latest times in CL table')
 1010 FORMAT ('Done')
 9010 FORMAT ('TECITB: ERROR ', I4, ' READING CL TABLE')
 9011 FORMAT ('TECITB: ERROR ', I4, ' OPENING CL TABLE')
 9012 FORMAT ('TECITB: ERROR ', I4, ' ATTACHING UV FILE')
      END
      SUBROUTINE TECANT (INPUTS, SUBARR, NANTS, ANTENS, ANTREJ, IRET)
C-----------------------------------------------------------------------
C   Initialize antenna selection.
C
C   Inputs:
C      INPUTS  C*(*)      Name of INPUTS object used to access adverbs
C
C   Outputs:
C      NANTS   I          Number of items in antenna selection list
C      ANTENS  I(*)       Antenna selection list
C      ANTREJ  L          Should antennas in selection list be rejected
C      IRET    I          Return status
C                            0 - selection initalized
C                          999 - logic flaw
C
C   Preconditions:
C      INPUTS is initialized
C      ANTENS has at least MAXANT elements
C
C   Postconditions:
C      IRET = 0 implies SUBARR >= 0
C      IRET = 0 implies 0 <= NANTS <= MAXANT
C      IRET = 0 implies ANTENS(1:NANTS) > 0
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*)
      INTEGER   SUBARR, NANTS, ANTENS(*)
      LOGICAL   ANTREJ
      INTEGER   IRET
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
C
C     Local Variables:
C
C     ANTLIS    Values of ANTENNAS adverb
C     TYPE      Attribute type
C     DIM       Attribute dimensions
C     I         Array index
C     CDUMMY    Dummy character argument
C
      INTEGER   ANTLIS(50), TYPE, DIM(3), I
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL INGET (INPUTS, 'SUBARR', TYPE, DIM, SUBARR, CDUMMY, IRET)
      CALL CHECK ('TECANT', 1, (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                         .AND. (DIM(1) .EQ. 1)
     *                         .AND. (DIM(2) .EQ. 1), IRET)
      IF (IRET .NE. 0) GO TO 999
      IF (SUBARR .LT. 0) THEN
         WRITE (MSGTXT, 1000)
         CALL MSGWRT (5)
         SUBARR = 1
      END IF
      CALL INGET (INPUTS, 'ANTENNAS', TYPE, DIM, ANTLIS, CDUMMY, IRET)
      CALL CHECK ('TECANT', 2, (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                         .AND. (DIM(1) .LE. 50)
     *                         .AND. (DIM(2) .EQ. 1), IRET)
      IF (IRET .NE. 0) GO TO 999
      NANTS = 0
      ANTREJ = .FALSE.
C
C     Invariant: NANTS is the minimum of MAXANT and the number of
C                non-zero entries in ANTLIS(1:I-1) and ANTENS(1:NANTS)
C                constains the absolute values of the first NANTS
C                non-zero entries in ANTLIS and ANTREJ is true if and
C                only if there is a negative element in ANTLIS(1:I-1).
C
      DO 10 I = 1, DIM(1)
         IF (ANTLIS(I) .NE. 0) THEN
            IF (ANTLIS(I) .LT. 0) THEN
               ANTREJ = .TRUE.
            END IF
            IF (NANTS .LT. MAXANT) THEN
               NANTS = NANTS + 1
               ANTENS(NANTS) = ABS(ANTLIS(I))
            ELSE
               WRITE (MSGTXT, 1001) ABS(ANTLIS(I))
               CALL MSGWRT (5)
            END IF
         END IF
   10 CONTINUE
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Defaulting to subarray 1')
 1001 FORMAT ('Ignoring selection of antenna ', I4, ' (no more room)')
      END
      SUBROUTINE TECDEL (INPUTS, DODDEL, LFACT, IRET)
C-----------------------------------------------------------------------
C   Set the dispersive delay corrections flag.
C
C   Inputs:
C      INPUTS   C*(*)    Name of INPUTS object used to access adverbs
C
C   Outputs:
C      DODDEL   L        Calculate dispersive delay corrections?
C      LFACT    R        Follow ionosphere factor
C      IRET     I        Return status
C                           0 - flag set
C                         999 - logic flaw
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*)
      LOGICAL   DODDEL
      REAL      LFACT
      INTEGER   IRET
C
      INCLUDE 'INCS:PAOOF.INC'
C
C     Local Variables
C
C     APARM    Value of APARM adverb
C     TYPE     Attribute type
C     DIM      Attribute dimensions
C     CDUMMY   Dummy character argument
C
      REAL      APARM(10)
      INTEGER   TYPE, DIM(3)
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL INGET (INPUTS, 'APARM', TYPE, DIM, APARM, CDUMMY, IRET)
      CALL CHECK ('TECDEL', 1, (IRET .EQ. 0) .AND. (TYPE .EQ. OOARE)
     *                         .AND. (DIM(1) .LE. 10)
     *                         .AND. (DIM(2) .EQ. 1), IRET)
      IF (IRET .NE. 0) GO TO 999
      DODDEL = APARM(1).GT.0.0
      LFACT = APARM(2)
      IF (APARM(2).EQ.0.0) LFACT = 1.0
      LFACT = MAX (0.0, MIN (1.0, LFACT))
C
      IF (DODDEL) THEN
         MSGTXT = 'Dispersive delay corrections will be calculated'
      ELSE
         MSGTXT = 'Dispersive delay corrections will not be calculated'
      END IF
      CALL MSGWRT (5)
C
  999 RETURN
      END
      SUBROUTINE TECION (INPUTS, FIRSCL, LASTCL, IRET)
C-----------------------------------------------------------------------
C   Initialize the IONEX module and check that the TEC data spans the
C   times covered by the CL table.
C
C   Inputs:
C      INPUTS  C*(*)     Name of the INPUTS object used to access the
C                         adverb values
C      FIRSCL  D         Earliest time in the CL table
C      LASTCL  D         Latest time in the CL table
C
C   Outputs:
C      IRET    I         Return status
C                           0 - IONEX module initialized
C                           1 - TEC data does not cover time range
C                           2 - failed to initialize IONEX
C                           3-  IONEX filename incorrect format (if NFILES
C                               > 1)
C                         999 - logic flaw
C
C   Preconditions:
C      INPUTS is initialized
C
C   Postconditions:
C      IRET = 0 imples IONEX module is initialized
C      IRET = 0 implies IONFIR() <= FIRSCL
C      IRET = 0 implies IONLAS() >= LASTCL
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*)
      DOUBLE PRECISION FIRSCL, LASTCL
      INTEGER   IRET
C
      INCLUDE 'INCS:PAOOF.INC'
C
C     Local variables
C
C     INFILE     Inputs file name
C     TYPE       Attribute type code
C     DIM        Attribute dimensions
C     IDUMMY     Dummy numeric argument
C     NFILES     Number if IONEX files
C     DAYNUM     Day number to be used in the names of IONEX files
C     YEAR       Year to be used in the names of IONEX files
C     CHDAYN     Character day number
C     CHYEAR     Character 2 digit year
C
      CHARACTER INFILE*48, FILENA*48, CHYEAR*2, CHDAYN*3,
     *CDUMMY
      INTEGER   TYPE, DIM(3), IDUMMY, DAYNUM, YEAR, NFILES, I, J, K,
     *LEAP
C
      DOUBLE PRECISION IONFIR, IONLAS
      EXTERNAL  IONFIR, IONLAS
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL INGET (INPUTS, 'INFILE', TYPE, DIM, IDUMMY, INFILE, IRET)
      CALL CHECK ('TECION', 1, (IRET .EQ. 0) .AND. (TYPE .EQ. OOACAR)
     *                         .AND. (DIM(1) .LE. 48)
     *                         .AND. (DIM(2) .EQ. 1), IRET)
      CALL INGET (INPUTS, 'NFILES', TYPE, DIM, NFILES, CDUMMY, IRET)
      CALL CHECK ('TECION', 2, (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                         .AND. (DIM(1) .LE. 1)
     *                         .AND. (DIM(2) .EQ. 1), IRET)
      LEAP = 0
      IF (IRET .NE. 0) GO TO 999
      J = INDEX(INFILE, ':')
      IF (J .EQ. 0) THEN
         DO 5 K=48, 1, -1
            IF (INFILE(K:K) .EQ. '/') THEN
               J = K
               GOTO 7
               ENDIF
 5          CONTINUE
         ENDIF
 7    IF (NFILES .GT. 1) THEN
C                                             Find daynum and year
         READ(INFILE(J+5:J+7),'(I3)',ERR=20) DAYNUM
         READ(INFILE(J+10:J+11),'(I2)',ERR=20) YEAR
C                                             Check for leap year, next divisiable
C                                             by 100 non-leap-year is 2100
C                                             last one was 1900, neither a worry
         IF (MOD(YEAR,4).EQ.0) LEAP=1
C                                             Check valid daynum and year
         IF (DAYNUM .GT. 365+LEAP .OR. DAYNUM .LT. 0 .OR.
     *         YEAR .LT. 0) THEN
            WRITE (MSGTXT, 9002)
            CALL MSGWRT (9)
            IRET = 3
            GO TO 999
         END IF
C                                             Loop through files
         DO 10 I=1, NFILES
C                                             Increment year if DAYNUM > 365
           IF (DAYNUM .GT. 365+LEAP) THEN
              DAYNUM = DAYNUM-365-LEAP
              YEAR = YEAR+1
           ENDIF
C                                             Convert daynum and year to CHAR
           WRITE(CHDAYN,'(I3)') DAYNUM
           WRITE(CHYEAR,'(I2)') YEAR
           IF (YEAR .LT. 10) CHYEAR(1:1) = '0'
           IF (DAYNUM .LT. 100) CHDAYN(1:1) = '0'
           IF (DAYNUM .LT. 10)  CHDAYN(2:2) = '0'
C                                             Create filename
           FILENA = INFILE(1:J+4)//CHDAYN//INFILE(J+8:J+9)
     *//CHYEAR//INFILE(J+12:J+12)
           CALL IONINI (FILENA, I, IRET)
           DAYNUM = DAYNUM+1
   10    CONTINUE
      ELSE
         CALL IONINI (INFILE, 1, IRET)
      END IF
      IF (IRET .EQ. 0) THEN
         IF ((IONFIR() .GT. FIRSCL) .OR. (IONLAS() .LE. LASTCL)) THEN
            WRITE (MSGTXT, 9000)
            CALL MSGWRT (9)
            IRET = 1
         END IF
      ELSE
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
         IRET = 2
      END IF
      GO TO 999
C                               File format incorrect
   20 WRITE (MSGTXT, 9002)
      CALL MSGWRT (9)
      IRET=3
C
  999 RETURN
C-----------------------------------------------------------------------
 9000 FORMAT ('TECION: TEC DATA DOES NOT COVER FULL RANGE OF CL TABLE')
 9001 FORMAT ('TECION: ERROR ', I4, ' INITIALIZING IONEX MODULE')
 9002 FORMAT ('TECION: INVALID DAY NUMBER AND/OR YEAR')
      END
      SUBROUTINE TECOTB (INPUTS, UVFILE, OCLTAB, NUMANT, NUMIF, NUMPOL,
     *                   NTERM, GMMOD, OUTVER, IRET)
C-----------------------------------------------------------------------
C   Open the output CL table.
C
C   Inputs:
C      INPUTS   C*(*)   Name of the INPUTS object used to access adverbs
C      UVFILE   C*(*)   Name of the UVDATA object used to access the
C                        AIPS data file
C      OCLTAB   C*(*)   Name of the TABLE object used to access the
C                        output CL table
C      NUMANT   I       Number of antennas for CL table
C      NUMIF    I       Number of IFs for CL table
C      NUMPOL   I       Number of polarizations for CL table
C      NTERM    I       Number of polynomial terms for CL table
C      GMMOD    R       Mean gain modulus for CL table
C
C   Ouputs:
C      OUTVER   I       Version number of output CL table
C      IRET     I       Return status
C                          0 - table opened
C                          1 - output table already exists
C                          2 - can not open output table
C                        999 - logic flaw
C
C   Preconditions:
C      INPUTS is initialized
C      UVFILE is initialized
C      NUMANT >= 0
C      1 <= NUMIF <= MAXIF
C      1 <= NUMPOL <= 2
C      NTERM >= 0
C
C   Postconditions:
C     IRET = 0 implies OCLTAB is open for writing and is empty
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVFILE*(*), OCLTAB*(*)
      INTEGER   NUMANT, NUMIF, NUMPOL, NTERM
      REAL      GMMOD
      INTEGER   OUTVER, IRET
C
      INCLUDE 'INCS:PAOOF.INC'
C
C     Local variables
C
C     CLROW     CL table row number
C     TYPE      Attribute type code
C     DIM       Attribute dimensions
C     CDUMMY    Dummy character argument
C
      INTEGER   CLROW, TYPE, DIM(3)
      CHARACTER CDUMMY
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL INGET (INPUTS, 'GAINUSE', TYPE, DIM, OUTVER, CDUMMY, IRET)
      CALL CHECK ('TECOTB', 1, (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                         .AND. (DIM(1) .EQ. 1)
     *                         .AND. (DIM(2) .EQ. 1), IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Fill in default for negative table version numbers:
C
      IF (OUTVER.LT.0) OUTVER = 0
C
      CALL UV2TAB (UVFILE, OCLTAB, 'CL', OUTVER, IRET)
      CALL CHECK ('TECOTB', 2, IRET .EQ. 0, IRET)
      IF (IRET .NE. 0) GO TO 999
C
      CALL OCLINI (OCLTAB, 'WRIT', CLROW, NUMANT, NUMPOL, NUMIF, NTERM,
     *             GMMOD, IRET)
      IF (IRET .EQ. 0) THEN
         IF (CLROW .GT. 1) THEN
C                                       OUTVER > 0 since a new table
C                                       will always be created otherwise
            WRITE (MSGTXT, 9000) OUTVER
            CALL MSGWRT (9)
            IRET = 1
         ELSE
C
C           Find the actual version number:
C
            CALL TABGET (OCLTAB, 'VER', TYPE, DIM, OUTVER, CDUMMY, IRET)
            CALL CHECK ('TECOTB', 3, (IRET .EQ. 0)
     *                               .AND. (TYPE .EQ. OOAINT)
     *                               .AND. (DIM(1) .EQ. 1)
     *                               .AND. (DIM(2) .EQ. 1), IRET)
            IF (IRET .NE. 0) GO TO 999
C
            WRITE (MSGTXT, 1000) OUTVER
            CALL MSGWRT (5)
            CALL INPUTT (INPUTS, 'GAINUSE', TYPE, DIM, OUTVER, CDUMMY,
     *         IRET)
         END IF
      ELSE
         WRITE (MSGTXT, 9001) IRET
         CALL MSGWRT (9)
         IRET = 2
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Creating CL table version ', I4)
 9000 FORMAT ('TECOTB: WILL NOT OVERWRITE EXISTING CL TABLE ', I4)
 9001 FORMAT ('TECOTB: ERROR ', I4,
     *        ' OPENING OUTPUT CL TABLE FOR WRITING')
      END
      SUBROUTINE TECPRO (ICLTAB, OCLTAB, UVFILE, SUBARR, NANTS, ANTENS,
     *   ANTREJ, DODDEL, LFACT, NUMIF, NUMPOL, NUMMOD, IRET)
C-----------------------------------------------------------------------
C   Copy records from ICLTAB to OCLTAB, updating ionospheric Faraday
C   rotation and dispersive delay for those records that meet the
C   subarray and antenna selection criteria.
C
C   Inputs:
C      ICLTAB  C*(*)  Name of the TABLE object used to access the
C                      input CL table
C      OCLTAB  C*(*)  Name of the TABLE object used to access the
C                      output CL table
C      UVFILE  C*(*)  Name of the UVDATA object used to access the AIPS
C                      data file
C      SUBARR  I      Selected subarray or 0 if all subarrays are
C                      selected
C      NANTS   I      Number of antennas in selection list
C      ANTENS  I(*)   Antenna selection list
C      ANTREJ  L      Are antennas in selection list to be rejected?
C      DODDEL  L      Should dispersive delays be calculated?
C      LFACT   R      Ionosphere following factor
C      NUMIF   I      Number of IFs in CL tables
C      NUMPOL  I      Number of polarizations in CL table
C
C   Outputs:
C      NUMMOD  I      Number of rows modified
C      IRET    I      Return status
C                        0 - table copied
C                        1 - I/O error
C
C   Preconditions:
C      ICLTAB is open read-only
C      OCLTAB is open for writing and is empty
C      UVFILE is initialized
C      SUBARR >= 0
C      0 <= NANTS <= length(ANTENS)
C      0 <= NUMIF <= MAXIF
C      1 <= NUMPOL <= 2
C
C   Postconditions
C      IRET = 0 implies ICLTAB is closed
C      IRET = 0 implies OCLTAB is closed
C      IRET = 0 implies NUMOD >= 0
C-----------------------------------------------------------------------
      CHARACTER ICLTAB*(*), OCLTAB*(*), UVFILE*(*)
      INTEGER   SUBARR, NANTS, ANTENS(*)
      LOGICAL   ANTREJ, DODDEL
      REAL      LFACT
      INTEGER   NUMIF, NUMPOL, NUMMOD, IRET
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables
C
C     NUMROW      Number of rows in input table
C     IROW        Input row number
C     OROW        Output row number
C     TIME        CL row time
C     TIMEI       CL row time interval
C     SOURID      CL row source ID
C     ANTNO       CL row antenna number
C     SUBA        CL row subarray number
C     FREQID      CL row frequency ID
C     IFR         CL row ionospheric Faraday rotation
C     GEODLY      CL row geometric delay polynomial
C     DOPOFF      CL row Doppler offsets
C     ATMOS       CL row atmospheric group delays
C     DATMOS      CL row atmospheric group delay derivatives
C     MBDLY       CL row multiband delays
C     CLOCK       CL row clock offsets
C     DCLOCK      CL row clock drift rates
C     DISP        CL row dispersive delays
C     DDISP       CL row dispersive delay rates
C     CREAL       CL row real gains
C     CIMAG       CL row imaginary gains
C     DELAY       CL row delays
C     RATE        CL row rates
C     WEIGHT      CL row weights
C     REFA        CL row reference antennas
C     ANT         Antenna index in ANTENS
C     WANTEN      Is antenna wanted?
C     TYPE        Attribute type code
C     DIM         Attribute dimensions
C     CDUMMY      Dummy character argument
C     IRET2       Return status for table closes
C
      INTEGER   NUMROW, IROW, OROW
C
      DOUBLE PRECISION TIME
      REAL      TIMEI
      INTEGER   SOURID, ANTNO, SUBA, FREQID
      REAL      IFR
      DOUBLE PRECISION GEODLY(12)
      REAL      DOPOFF(MAXIF), ATMOS(2), DATMOS(2), MBDELY(2)
      REAL      CLOCK(2), DCLOCK(2), DISP(2), DDISP(2)
      REAL      CREAL(2, MAXIF), CIMAG(2, MAXIF)
      REAL      DELAY(2, MAXIF), RATE(2, MAXIF), WEIGHT(2, MAXIF)
      INTEGER   REFA(2, MAXIF)
C
      INTEGER   ANT
      LOGICAL   WANTEN
      INTEGER   TYPE, DIM(3)
      CHARACTER CDUMMY
      INTEGER   IRET2
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NUMMOD = 0
      CALL TABGET (ICLTAB, 'NROW', TYPE, DIM, NUMROW, CDUMMY, IRET)
      CALL CHECK ('TECPRO', 1, (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *            .AND. (DIM(1) .EQ. 1) .AND. (DIM(2) .EQ. 1), IRET)
      IF (IRET .NE. 0) GO TO 999
C
      IROW = 1
      OROW = 1
C
C     Invariant: IRET = 0 implies IROW - 1 rows have been read from
C                                 ICLTAB
C     Bound: NUMROW + 1 - IROW
C
   10 IF ((IRET .EQ. 0) .AND. (IROW .NE. NUMROW + 1)) THEN
         CALL OTABCL (ICLTAB, 'READ', IROW, NUMPOL, NUMIF, TIME, TIMEI,
     *                SOURID, ANTNO, SUBA, FREQID, IFR, GEODLY, DOPOFF,
     *                ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *                DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA,
     *                IRET)
         IF (IRET .EQ. 0) THEN
            IF ((SUBARR .EQ. 0) .OR. (SUBA .EQ. SUBARR)) THEN
               IF (NANTS .EQ. 0) THEN
                  WANTEN = .TRUE.
               ELSE
                  WANTEN = .FALSE.
                  DO 20 ANT = 1, NANTS
                     IF (ANTENS(ANT) .EQ. ANTNO) THEN
                        WANTEN = .TRUE.
                     END IF
   20             CONTINUE
                  IF (ANTREJ) THEN
                     WANTEN = .NOT. WANTEN
                  END IF
               END IF
               IF (WANTEN) THEN
                  CALL TECCOR (UVFILE, DODDEL, LFACT, TIME, SUBA, ANTNO,
     *               SOURID, NUMPOL, IFR, DISP, DDISP, IRET)
C
C                 Ignore failed interpolation:
C
                  IF (IRET .EQ. 1) THEN
                     IRET = 0
                  END IF
                  IF (IRET .EQ. 0) THEN
                     NUMMOD = NUMMOD + 1
                  ELSE
                     WRITE (MSGTXT, 9020) IRET
                     CALL MSGWRT (9)
                     IRET = 1
                  END IF
               END IF
            END IF
            IF (IRET .EQ. 0) THEN
               CALL OTABCL (OCLTAB, 'WRIT', OROW, NUMPOL, NUMIF, TIME,
     *                      TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *                      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *                      CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG,
     *                      DELAY, RATE, WEIGHT, REFA, IRET)
               IF (IRET .NE. 0) THEN
                  WRITE (MSGTXT, 9021) IRET
                  CALL MSGWRT (9)
                  IRET = 1
               END IF
            END IF
         ELSE IF (IRET .EQ. -1) THEN
C
C           Skip flagged records:
C
            IRET = 0
         ELSE
            WRITE (MSGTXT, 9022) IRET
            CALL MSGWRT (9)
            IRET = 1
         END IF
         GO TO 10
      END IF
C
C     Close tables:
C
      CALL OTABCL (OCLTAB, 'CLOS', OROW, NUMPOL, NUMIF, TIME, TIMEI,
     *             SOURID, ANTNO, SUBA, FREQID, IFR, GEODLY, DOPOFF,
     *             ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP, DDISP,
     *             CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET2)
      IF (IRET2 .EQ. 0) THEN
         CALL OTABCL (ICLTAB, 'CLOS', IROW, NUMPOL, NUMIF, TIME,
     *                TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *                GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK,
     *                DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *                WEIGHT, REFA, IRET2)
         IF (IRET2 .NE. 0) THEN
            WRITE (MSGTXT, 9023) IRET2
            CALL MSGWRT (9)
            IRET = 1
         END IF
      ELSE
         WRITE (MSGTXT, 9024) IRET2
         CALL MSGWRT (9)
         IRET = 1
      END IF
C
      IF (IRET .EQ. 0) THEN
         WRITE (MSGTXT, 1020) OROW - 1
         CALL MSGWRT (5)
         WRITE (MSGTXT, 1021) NUMMOD
         CALL MSGWRT (5)
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Copied ', I6, ' records to the output table')
 1021 FORMAT (I6, ' were modified')
 9020 FORMAT ('TECPRO: ERROR ', I4, ' OBTAINING NEW CALIBRATION')
 9021 FORMAT ('TECPRO: ERROR ', I4, ' WRITING TO OUTPUT TABLE')
 9022 FORMAT ('TECPRO: ERROR ', I4, ' READING FROM INPUT TABLE')
 9023 FORMAT ('TECPRO: ERROR ', I4, ' CLOSING OUTPUT TABLE')
 9024 FORMAT ('TECPRO: ERROR ', I4, ' CLOSING INPUT TABLE')
      END
      SUBROUTINE TECCOR (UVFILE, DODDEL, LFACT, TIME, SUBA, ANTNO,
     *   SOURID, NUMPOL, IFR, DISP, DDISP, IRET)
C-----------------------------------------------------------------------
C   Calculate ionospheric Faraday rotation and dispersive delay
C   corrections.
C
C   Inputs:
C      UVFILE  C*(*)     Name of UVDATA object used to access data file
C      DODDEL  L         Calculate dispersive delays?
C      LFACT   R         Follow ionospher factor
C      TIME    D         Time at which to calculate correction
C      SUBA    I         Subarray number
C      ANTNO   I         Antenna number
C      SOURID  I         Source ID number
C      NUMPOL  I         Number of polarizations
C
C   Outputs:
C      IFR     R         Ionospheric Faraday rotation
C      DISP    R(*)      Dispersive delay in seconds at 1m wavelength
C      DDISP   R(*)      Derivative of dispersive delay wrt time in
C                         seconds at 1m wavelength
C      IRET    I         Return status
C                           0 - corrections calculated
C                           1 - TEC data not available
C                           2 - I/O error
C                         999 - logic flaw
C
C   Preconditions:
C      UVFILE is initialized
C      SUBA >= 1
C      1 <= ANTNO <= MAXANT
C      SOURID >= 1
C      1 <= NUMPOL <= 2
C
C   Postconditions:
C      IRET = 1 implies IFR = FBLANK
C      IRET = 1 and DODDEL implies DISP(1:NUMPOL) = FBLANK
C      IRET = 1 and DODDEL implies DDISP(1:NUMPOL) = FBLANK
C-----------------------------------------------------------------------
      CHARACTER UVFILE*(*)
      LOGICAL   DODDEL
      DOUBLE PRECISION TIME
      INTEGER   SUBA, ANTNO, SOURID, NUMPOL
      REAL      LFACT, IFR, DISP(*), DDISP(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
C
C     Local variables:
C
C     LASTSA    Subarray number on last call
C     LASTSU    Source number on last call
C     DISK      Disk number for UV file
C     CNO       Catalogue number for UV file
C     CATBLK    Header block for UV file
C     SULUN     AIPS LUN for SU table
C     ANBUFF    Buffer for reading AN table
C     ANT       Antenna number
C     ANTIDX    Antenna index
C     HA        Hour angle (radians)
C     EL        Elevation (radians)
C     ZA        Zenith angle (radians)
C     COSAZ     Cosine of azimuth
C     AZ        Azimuth (radians)
C     DLAT      Latitude offset of puncture point (radians)
C     DLON      Longitude offset of puncture point (radians)
C     AZION     Azimuth at puncture point (radians)
C     ZAION     Zenith angle at puncture point (radians)
C     TEC       Zenith TEC at puncture point (electrons per square
C                meter)
C     TEPATH    TEC along line of sight
C     B         Magnetic field vector at puncture point
C     MAG       Magnetic field along line of sight
C     RADIUS    Radius of the Earth (meters)
C     POL       Polarization number
C     DATOBS    Reference date for observations
C     JD0       Julian day number on reference date
C     JD0UTC    Julian date corrected to UTC
C     TYPE      Attribute type
C     DIM       Attribute dimensions
C     CDUMMY    Dummy character argument
C     IDUMMY    Dummy numerical argument
C
      INTEGER   LASTSA, LASTSU, DISK, CNO, CATBLK(256), SULUN, LASTID
      SAVE      LASTSA, LASTSU, DISK, CNO, CATBLK, SULUN, LASTID
      INTEGER   ANBUFF(512), ANT, ANTIDX
      REAL      HA, EL, ZA
      REAL      AZ, DLAT, DLON, AZION, ZAION, TEC, TEPATH, B(3),
     *          MAG, RADIUS
      INTEGER   POL
      CHARACTER DATOBS*8
      DOUBLE PRECISION JD0, JD0UTC
      SAVE      JD0, JD0UTC
      INTEGER   IDUMMY, TYPE, DIM(3)
      CHARACTER CDUMMY
C
      PARAMETER (RADIUS = 6378000.0)
C
      REAL      IONALT
      EXTERNAL  IONALT
C
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
C
      DATA LASTSA /0/
      DATA LASTSU, LASTID /2*0/
      DATA DISK   /0/
C-----------------------------------------------------------------------
      IRET = 0
C
C     Read file information and allocate LUN for source table if this is
C     the first call (DISK = 0):
C
      IF (DISK .EQ. 0) THEN
         CALL OUVATT (UVFILE, .FALSE., IRET)
         CALL CHECK ('TECCOR', 1, IRET .EQ. 0, IRET)
         CALL OUVCGT (UVFILE, CATBLK, IRET)
         CALL CHECK ('TECCOR', 2, IRET .EQ. 0, IRET)
         IF (IRET .NE. 0) GO TO 999
         CALL FNAGET (UVFILE, 'DISK', TYPE, DIM, DISK, CDUMMY, IRET)
         CALL CHECK ('TECCOR', 3, (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                            .AND. (DIM(1) .EQ. 1)
     *                            .AND. (DIM(2) .EQ. 1), IRET)
         IF (IRET .NE. 0) GO TO 999
         CALL FNAGET (UVFILE, 'CNO', TYPE, DIM, CNO, CDUMMY, IRET)
         CALL CHECK ('TECCOR', 4, (IRET .EQ. 0) .AND. (TYPE .EQ. OOAINT)
     *                            .AND. (DIM(1) .EQ. 1)
     *                            .AND. (DIM(2) .EQ. 1), IRET)
         IF (IRET .NE. 0) GO TO 999
         CALL OBLUN (SULUN, IRET)
         CALL CHECK ('TECCOR', 5, IRET .EQ. 0, IRET)
         IF (IRET .NE. 0) GO TO 999
         CALL UVDGET (UVFILE, 'DATE-OBS', TYPE, DIM, IDUMMY, DATOBS,
     *                IRET)
         CALL CHECK ('TECCOR', 6, (IRET .EQ. 0) .AND. (TYPE .EQ. OOACAR)
     *                            .AND. (DIM(1) .EQ. 8)
     *                            .AND. (DIM(2) .EQ. 1), IRET)
         IF (IRET .NE. 0) GO TO 999
         CALL JULDAY (DATOBS, JD0)
      END IF
C
C     Make sure array information is up to date:
C
      IF ((IRET .EQ. 0) .AND. (SUBA .NE. LASTSA)) THEN
         CALL GETANT (DISK, CNO, SUBA, CATBLK, ANBUFF, IRET)
         IF (IRET .EQ. 0) THEN
            LASTSA = SUBA
            JD0UTC = JD0 - ANTUTC / (24.0D0 * 3600.0D0)
         ELSE
            WRITE (MSGTXT, 9000) IRET
            CALL MSGWRT (9)
            IRET = 2
            END IF
         END IF
C
C     Make sure source information is up to date:
C     change number if required by missing source number in CL
C
      IF ((IRET.EQ.0) .AND. (SOURID.NE.LASTSU)) THEN
         CALL GETSOU (SOURID, DISK, CNO, CATBLK, SULUN, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 9001) IRET, SOURID
            CALL MSGWRT (7)
            LASTID = -SOURID
            IF (IRET.EQ.11) IRET = 0
         ELSE
            LASTID = SOURID
            END IF
         LASTSU = SOURID
         END IF
      IF (IRET.EQ.0) THEN
C
C     Find the antenna in the DANS.INC common blocks
C
         ANTIDX = 0
         DO 10 ANT = 1, NSTNS
            IF (TELNO(ANT).EQ.ANTNO) ANTIDX = ANT
 10         CONTINUE
         IF ((ANTIDX.NE.0) .AND. (LASTID.GT.0)) THEN
            CALL SOUELV (ANTIDX, TIME, HA, EL, AZ)
            ZA = (PI / 2.0) - EL
            CALL GETSIP (REAL (STNLAT(ANTIDX)), AZ, ZA, IONALT(),
     *         DLAT, DLON, AZION, ZAION)
            CALL IONTEC (LFACT, REAL (STNLAT(ANTIDX) + DLAT),
     *         REAL (STNLON(ANTIDX) + DLON), JD0UTC + TIME, TEC, IRET)
            IF (IRET.EQ.0) THEN
               TEPATH = TEC / COS (ZAION)
               CALL MAGDIP (REAL (STNLAT(ANTIDX) + DLAT),
     *                      REAL (STNLON(ANTIDX) + DLON),
     *                      RADIUS + IONALT(), B)
               MAG = -1.0 * (B(1) * COS (ZAION)
     *                       + B(2) * SIN (ZAION) * SIN (AZION)
     *                       + B(3) * SIN (ZAION) * COS (AZION))
               IFR = 2.6E-17 * MAG * TEPATH
               IF (DODDEL) THEN
                  DO 20 POL = 1, NUMPOL
                     DISP(POL) = 40.28 * TEPATH / (VELITE ** 3)
                     DDISP(POL) = 0.0
   20                CONTINUE
                  END IF
            ELSE
               IFR = FBLANK
               IF (DODDEL) THEN
                  DO 30 POL = 1, NUMPOL
                     DISP(POL) = FBLANK
                     DDISP(POL) = FBLANK
   30                CONTINUE
                  END IF
               IRET = 1
               END IF
C                                       No antenna/source data.
         ELSE
            IFR = FBLANK
            IF (DODDEL) THEN
               DO 40 POL = 1, NUMPOL
                  DISP(POL) = FBLANK
                  DDISP(POL) = FBLANK
   40             CONTINUE
               END IF
            IRET = 1
            END IF
         END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 9000 FORMAT ('TECCOR: ERROR',I4,' READING ARRAY INFORMATION')
 9001 FORMAT ('TECCOR: ERROR',I4,' READING SOURCE',I4,' INFORMATION')
      END
      SUBROUTINE TECHIS (OCLTAB, INPUTS, DODDEL, LFACT, OUTVER, NUMMOD,
     *   IRET)
C-----------------------------------------------------------------------
C   Update history file.
C
C   Inputs:
C      OCLTAB  C*(*)   Name of TABLE object used to access output CL
C                       table
C      INPUTS  C*(*)   Name of inputs object used to access adverbs
C      DODDEL  L       Were dispersive delays calculated?
C      OUTVER  I       Version number of output CL table
C      NUMMOD  I       Number of modified CL records
C
C   Outputs:
C      IRET    I       Return status:
C                       0 - history updated
C                       1 - I/O error
C
C   Preconditions:
C      OCLTAB is initialized
C      INPUTS is initialized
C      OUTVER > 0
C      NUMMOD >= 0
C-----------------------------------------------------------------------
      CHARACTER OCLTAB*(*), INPUTS*(*)
      LOGICAL   DODDEL
      REAL      LFACT
      INTEGER   OUTVER, NUMMOD, IRET
C
C     Local variables
C
C     NLIST   Number of adverbs to list
C     LIST    Adverbs to list
C     ENTRY   History file entry
C
      INTEGER   NLIST
      PARAMETER (NLIST = 11)
      CHARACTER LIST(NLIST)*8, ENTRY*72
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA LIST /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *           'INFILE  ', 'NFILES  ', 'SUBARR  ', 'ANTENNAS',
     *           'GAINVER ', 'GAINUSE ', 'APARM   ' /
C-----------------------------------------------------------------------
      CALL OHTIME (OCLTAB, IRET)
      IF (IRET .EQ. 0) THEN
         CALL OHLIST (INPUTS, LIST, NLIST, OCLTAB, IRET)
         IF (IRET .EQ. 0) THEN
            WRITE (ENTRY, 1000) OUTVER
            CALL OHWRIT (ENTRY, OCLTAB, IRET)
            IF (IRET .EQ. 0) THEN
               WRITE (ENTRY, 1001) NUMMOD
               CALL OHWRIT (ENTRY, OCLTAB, IRET)
               IF ((IRET .EQ. 0) .AND. DODDEL) THEN
                  ENTRY = 'Calculated dispersive delay'
                  CALL OHWRIT (ENTRY, OCLTAB, IRET)
                  WRITE (ENTRY,4000) LFACT
                  CALL OHWRIT (ENTRY, OCLTAB, IRET)
               END IF
            END IF
         END IF
      END IF
C
      IF (IRET .NE. 0) THEN
         WRITE (MSGTXT, 9000) IRET
         CALL MSGWRT (9)
         IRET = 1
      END IF
C-----------------------------------------------------------------------
 1000 FORMAT ('Created CL table version ', I4)
 1001 FORMAT ('Updated ', I6, ' CL table rows')
 4000 FORMAT ('LFACT    =',F8.5,5X,'/ Follow ionosphere fraction')
 9000 FORMAT ('TECHIS: ERROR ', I4, ' UPDATING HISTORY FILE')
      END
      SUBROUTINE TECVAL (MAP, LONDIM, NLONG, NLAT, MINLON, DLONG,
     *                   MINLAT, DLAT, LONG, LAT, TEC, IRET)
C-----------------------------------------------------------------------
C   Find the zenith TEC at LAT and LONG.
C
C   Inputs:
C      MAP        R(LONDIM, *)     Map of zenith TEC; actual map values
C                                    are stored in the subrange
C                                    MAP(1:NLONG, 1:NLAT) and cells for
C                                    which the zenith TEC is not
C                                    available are set to FBLANK.
C      LONDIM     I                Leading dimension (longitude) of MAP
C                                    array
C      NLONG      I                Number of longitude cells in MAP
C      NLAT       I                Number of longitude cells in MAP
C      MINLON     R                East longitude at MAP(1, :) in
C                                    degrees
C      DLONG      R                Spacing in longitude
C      MINLAT     R                Latitude at MAP(:, 1) in degrees
C      DLAT       R                Spacing in latitude
C      LAT        R                Latitude at which zenith TEC is
C                                    desired in degrees
C      LONG       R                East longitude at which zenith TEC
C                                    is desired in degrees
C
C   Outputs:
C      TEC        R                Zenith TEC in electrons per square
C                                    metre
C      IRET       I                Return status:
C                                    0 - TEC available
C                                    1 - TEC data missing
C
C   Preconditions:
C      LONDIM >= NLONG >= 2
C      NLAT  >= 2
C      DLONG > 0.0
C      DLAT > 0.0
C      -180.0 <= LONG <= +180.0
C      -90.0 <= LAT <= +90.0
C
C   Postconditions:
C      IRET = 0 implies TEC /= FBLANK and TEC > 0.0
C      IRET = 1 implies TEC = FBLANK
C-----------------------------------------------------------------------
      INTEGER   LONDIM
      REAL      MAP(LONDIM, *)
      INTEGER   NLONG, NLAT
      REAL      MINLON, DLONG, MINLAT, DLAT, LAT, LONG, TEC
      INTEGER   IRET
C
C     Local variables
C
C     I     Coordinate of cell with next lower longitude to LONG in MAP
C     J     Coordinate of cell with next lower latitude to LAT in MAP
C     WX    Interpolation weight for x-axis (longitude)
C     WY    Interpolation weight for y-axis (latitude)
C
      INTEGER   I, J
      REAL      WX, WY
C
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      I = INT ((LONG - MINLON) / DLONG) + 1
      J = INT ((LAT - MINLAT) / DLAT) + 1
      IF ((1 .LE. I) .AND. (I .LT. NLONG) .AND. (1 .LE. J)
     *     .AND. (J .LT. NLAT)) THEN
         WX = (LONG - MINLON - (I - 1) * DLONG) / DLONG
         WY = (LAT - MINLAT - (J - 1) * DLAT) / DLAT
         IF ((MAP(I, J) .NE. FBLANK) .AND. (MAP(I, J + 1) .NE. FBLANK)
     *       .AND. (MAP(I + 1, J) .NE. FBLANK)
     *       .AND. (MAP(I + 1, J + 1) .NE. FBLANK)) THEN
            TEC = (1.0 - WX) * (1.0 - WY) * MAP(I, J)
     *            + (1.0 - WX) * WY * MAP(I, J + 1)
     *            + WX * (1.0 - WY) * MAP(I + 1, J)
     *            + WX * WY * MAP(I + 1, J + 1)
            IRET = 0
         ELSE
            TEC = FBLANK
            IRET = 1
         END IF
      ELSE
         TEC = FBLANK
         IRET = 1
      END IF
      END
LOCAL INCLUDE 'IONEX.INC'
C
C   Private structures maintaining information from IONEX maps
C
C   MAXMAP   Maximum number of TEC maps
C   MAXLON   Maximum number of longitude cells
C   MAXLAT   Maximum number of latitude cells
C   NMAPS    Number of TEC maps expected in one file
C   NMAPRD   Number of TEC maps read in one file
C   NLONG    Number of longitude cells in maps
C   NLAT     Number of latitude cells in maps
C   MINLON   Smallest East longitude in maps in degrees
C   MINLAT   Smallest latitude in degrees
C   DLONG    Longitude spacing in degrees
C   DLAT     Latitude spacing in degrees
C   MAPTIM   Julian day number for each map
C   MAPS     TEC maps
C   ALT      Height of ionosphere
C   EXPONT   Exponent for IONEX file
C   TOTMAP   Total number of maps read in all files
C
      INTEGER   MAXMAP, MAXLON, MAXLAT
      PARAMETER (MAXMAP = 300)
      PARAMETER (MAXLON = 100)
      PARAMETER (MAXLAT = 80)
      INTEGER   NMAPS, NMAPRD, NLONG, NLAT, TOTMAP
      REAL      MINLON, MINLAT, DLONG, DLAT
      DOUBLE PRECISION MAPTIM(MAXMAP)
      REAL      MAPS(MAXLON, MAXLAT, MAXMAP), ALT
      INTEGER   EXPONT
C
      COMMON /IONEX/ MAPTIM, MAPS, NMAPS, NMAPRD, NLONG, NLAT, MINLON,
     *               MINLAT, DLONG, DLAT, ALT, EXPONT, TOTMAP
      SAVE /IONEX/
C
LOCAL END
      SUBROUTINE IONINI (FNAME, NFILE, IRET)
C-----------------------------------------------------------------------
C   Initialize IONEX module and parse input file.
C
C   Inputs:
C      FNAME    C*(*)   Input file name
C      NFILE    I       File number, if multiple IONEX files requested
C
C   Outputs:
C      IRET     I       Return status
C                          0 - module initialized
C                          1 - failed to read maps
C                          2 - failed to read header
C                          3 - can not open file
C-----------------------------------------------------------------------
      CHARACTER FNAME*(*)
      INTEGER   IRET, NFILE
C
C     Local variables
C
C     TXTLUN   AIPS LUN for input file
C     FIND     FTAB index for input file
C     IRET2    Disposable return status
C
      INTEGER   TXTLUN
      PARAMETER (TXTLUN = 10)
      INTEGER   FIND, IRET2
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
      CALL ZTXOPN ('READ', TXTLUN, FIND, FNAME, .FALSE., IRET)
      IF (IRET .EQ. 0) THEN
         CALL IONHDR (TXTLUN, FIND, IRET)
         IF (IRET .EQ. 0) THEN
            IF (NFILE .EQ. 1) NMAPRD = 0
C                                       NMAPS is the number of TEC
C                                       maps in the IONEX file
C
C           Invariant: IRET = 0 implies that NMAPRD TEC maps have been
C                               read from the input file
C           Bound: NMAPS+TOTMAP - NMAPRD
C
   10       IF ((IRET .EQ. 0) .AND. (NMAPRD .NE. NMAPS+TOTMAP)) THEN
               CALL IONMAP (TXTLUN, FIND, IRET)
               IF (IRET .NE. 0) THEN
                  WRITE (MSGTXT, 9010) IRET
                  CALL MSGWRT (9)
                  IRET = 1
               END IF
               GO TO 10
            END IF
         ELSE
            WRITE (MSGTXT, 9011) IRET
            CALL MSGWRT (9)
            IRET = 2
         END IF
C
C           Close input file; ignore errors at this stage
C
         CALL ZTXCLS (TXTLUN, FIND, IRET2)
         TOTMAP=NMAPRD
      ELSE
         WRITE (MSGTXT, 9012) IRET
         CALL MSGWRT (9)
         IRET = 3
      END IF
C-----------------------------------------------------------------------
 9010 FORMAT ('IONINI: ERROR ', I4, ' READING IONEX DATA')
 9011 FORMAT ('IONINI: ERROR ', I4, ' READING IONEX HEADER')
 9012 FORMAT ('IONINI: ERROR ', I4, ' OPENING INPUT TEXT FILE')
      END
      SUBROUTINE IONHDR (LUN, FIND, IRET)
C-----------------------------------------------------------------------
C   Parse the header of an IONEX file that is open for reading on LUN
C   with FTAB index FIND.
C
C   Inputs:
C      LUN      AIPS LUN for input file
C      FIND     FTAB index of input file
C
C   Output:
C      IRET     Return status
C                  0 - header parsed and file is usable
C                  1 - file unusable
C                  2 - wrong IONEX version or data type
C                  3 - input file is not an IONEX file
C                  4 - header data missing
C                 10 - I/O error detected
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IRET
C
C     Local variables
C
C     LINE    Line buffer
C     VERS    IONEX revision
C     TYPE    Data type code
C     SYSTEM  Observing system
C
      CHARACTER LINE*80
      REAL      VERS
      CHARACTER TYPE*1, SYSTEM*3
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
C
C     Set default exponent
C
      EXPONT = -1
C
      NMAPS = 0
      NLONG = 0
      NLAT = 0
      ALT = 0.0
      CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
      IF (IRET .EQ. 0) THEN
         IF (LINE(61:80) .EQ. 'IONEX VERSION / TYPE') THEN
            READ (LINE, FMT = '(F8.1, 12X, A1, 19X, A3, 37X)',
     *            IOSTAT = IRET) VERS, TYPE, SYSTEM
            IF (IRET .EQ. 0) THEN
               IF ((VERS .EQ. 1.0) .AND. (TYPE .EQ. 'I')) THEN
C
C                 Read each header record
C
   10             IF ((IRET .EQ. 0)
     *                .AND. (LINE(61:80) .NE. 'END OF HEADER       '))
     *               THEN
                     CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
                     IF (IRET .EQ. 0) THEN
                        IF (LINE(61:80)
     *                      .EQ. '# OF MAPS IN FILE   ') THEN
                           CALL IONNMP (LINE, IRET)
                           IF (IRET .NE. 0) THEN
                              WRITE (MSGTXT, 9010) IRET
                              CALL MSGWRT (9)
                              IRET = 1
                           END IF
                        ELSE IF (LINE(61:80)
     *                           .EQ. 'MAP DIMENSION       ') THEN
                           CALL IONDIM (LINE, IRET)
                           IF (IRET .NE. 0) THEN
                              WRITE (MSGTXT, 9011) IRET
                              CALL MSGWRT (9)
                              IRET = 1
                           END IF
                        ELSE IF (LINE(61:80)
     *                           .EQ. 'HGT1 / HGT2 / DHGT ') THEN
                           CALL IONHGT (LINE, IRET)
                           IF (IRET .NE. 0) THEN
                              WRITE (MSGTXT, 9012) IRET
                              CALL MSGWRT (9)
                              IRET = 1
                           END IF
                        ELSE IF (LINE(61:80)
     *                           .EQ. 'LAT1 / LAT2 / DLAT ') THEN
                           CALL IONLAT (LINE, IRET)
                           IF (IRET .NE. 0) THEN
                              WRITE (MSGTXT, 9013) IRET
                              CALL MSGWRT (9)
                              IRET = 1
                           END IF
                        ELSE IF (LINE(61:80)
     *                           .EQ. 'LON1 / LON2 / DLON ') THEN
                           CALL IONLON (LINE, IRET)
                           IF (IRET .NE. 0) THEN
                              WRITE (MSGTXT, 9014) IRET
                              CALL MSGWRT (9)
                              IRET = 1
                           END IF
                        ELSE IF (LINE(61:80)
     *                           .EQ. 'EXPONENT           ') THEN
                           CALL IONEXP (LINE, IRET)
                           IF (IRET .NE. 0) THEN
                              WRITE (MSGTXT, 9015) IRET
                              CALL MSGWRT (9)
                              IRET = 1
                           END IF
                        END IF
                     ELSE
                        WRITE (MSGTXT, 9016) IRET
                        CALL MSGWRT (9)
                        IRET = 10
                     END IF
                     GO TO 10
                  END IF
               ELSE
                  WRITE (MSGTXT, 9017) VERS, TYPE
                  CALL MSGWRT (9)
                  IRET = 2
               END IF
            ELSE
               WRITE (MSGTXT, 9018)
               CALL MSGWRT (9)
               IRET = 10
            END IF
         ELSE
            WRITE (MSGTXT, 9019)
            CALL MSGWRT (9)
            IRET = 3
         END IF
      ELSE
         WRITE (MSGTXT, 9016) IRET
         CALL MSGWRT (9)
         IRET = 10
      END IF
C
C     Check that nothing important was missing:
C
      IF (IRET .EQ. 0) THEN
         IF (NMAPS .EQ. 0) THEN
            WRITE (MSGTXT, 9020)
            CALL MSGWRT (9)
            IRET = 4
         END IF
         IF (NLAT .EQ. 0) THEN
            WRITE (MSGTXT, 9021)
            CALL MSGWRT (9)
            IRET = 4
         END IF
         IF (NLONG .EQ. 0) THEN
            WRITE (MSGTXT, 9022)
            CALL MSGWRT (9)
            IRET = 4
         END IF
         IF (ALT .EQ. 0.0) THEN
            WRITE (MSGTXT, 9023)
            CALL MSGWRT (9)
            IRET = 4
         END IF
      END IF
C-----------------------------------------------------------------------
 9010 FORMAT ('IONHDR: ERROR ', I4, ' READING NUMBER OF MAPS')
 9011 FORMAT ('IONHDR: ERROR ', I4, ' READING MAP DIMENSIONS')
 9012 FORMAT ('IONHDR: ERROR ', I4, ' READING HEIGHT CARD')
 9013 FORMAT ('IONHDR: ERROR ', I4, ' READING LATITUDE CARD')
 9014 FORMAT ('IONHDR: ERROR ', I4, ' READING LONGITUDE CARD')
 9015 FORMAT ('IONHDR: ERROR ', I4, ' READING EXPONENT CARD')
 9016 FORMAT ('IONHDR: ERROR ', I4, ' READING TEXT FILE')
 9017 FORMAT ('IONHDR: UNKNOWN VERSION (', F6.1, ') OR TYPE (''', A1,
     *        ''')')
 9018 FORMAT ('IONHDR: ERROR PARSING FIRST RECORD')
 9019 FORMAT ('IONHDR: INPUT FILE IS NOT IN IONEX FORMAT')
 9020 FORMAT ('IONHDR: NUMBER OF MAPS IS MISSING')
 9021 FORMAT ('IONHDR: LATITUDE SPECIFICATION IS MISSING')
 9022 FORMAT ('IONHDR: LONGITUDE SPECIFICATION IS MISSING')
 9023 FORMAT ('IONHDR: HEIGHT SPECIFICATION IS MISSING')
      END
      SUBROUTINE IONNMP (LINE, IRET)
C-----------------------------------------------------------------------
C   Parse a number of maps record in LINE.
C
C   Input:
C      LINE     C*80     Number of maps record
C
C   Output:
C      IRET     I        Return status:
C                          0 - valid number of maps
C                          1 - invalid number of maps
C                          2 - parse error
C-----------------------------------------------------------------------
      CHARACTER LINE*80
      INTEGER   IRET
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
      READ (LINE, FMT = '(I6, 74X)', IOSTAT = IRET) NMAPS
      IF (IRET .EQ. 0) THEN
         IF ((1 .LE. NMAPS) .AND. (NMAPS .LE. MAXMAP)) THEN
            IRET = 0
         ELSE IF (NMAPS .LT. 1) THEN
            WRITE (MSGTXT, 9000) NMAPS
            CALL MSGWRT (9)
            IRET = 1
         ELSE IF (NMAPS .GT. MAXMAP) THEN
            WRITE (MSGTXT, 9001) NMAPS, MAXMAP
            CALL MSGWRT (9)
            IRET = 1
         END IF
      ELSE
         WRITE (MSGTXT, 9002)
         CALL MSGWRT (9)
         IRET = 2
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('IONNMP: INVALID NUMBER OF MAPS ', I6)
 9001 FORMAT ('IONNMP: TOO MANY MAPS - ', I6 , ' EXCEEDS MAX ', I6)
 9002 FORMAT ('IONNMP: FAILED TO PARSE NUMBER OF MAPS')
      END
      SUBROUTINE IONDIM (LINE, IRET)
C-----------------------------------------------------------------------
C   Parse dimensions record
C
C   Input
C      LINE    C*80    Dimensions record
C
C   Output
C      IRET    I       Return status
C                        0 - dimensions valid
C                        1 - dimensions invalid
C                        2 - parse error
C-----------------------------------------------------------------------
      CHARACTER LINE*80
      INTEGER   IRET
C
C     Local variables
C
C     DIM      Dimensionality of maps
C
      INTEGER   DIM
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
      READ (LINE, FMT = '(I6, 74X)', IOSTAT = IRET) DIM
      IF (IRET .EQ. 0) THEN
         IF (DIM .EQ. 2) THEN
            IRET = 0
         ELSE
            WRITE (MSGTXT, 9000) DIM
            CALL MSGWRT (9)
            IRET = 1
         END IF
      ELSE
         WRITE (MSGTXT, 9001)
         CALL MSGWRT (9)
         IRET = 2
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('IONDIM: CAN NOT USE ', I6, ' DIMENSIONAL MAPS')
 9001 FORMAT ('IONDIM: ERROR PARSING NUMBER OF DIMENSIONS')
      END
      SUBROUTINE IONHGT (LINE, IRET)
C-----------------------------------------------------------------------
C   Parse height record in LINE
C
C   Input:
C      LINE   C*80    Height record
C
C   Output:
C      IRET   I       Return status
C                       0 - record parsed
C                       1 - not constant height data
C                       2 - parse error
C-----------------------------------------------------------------------
      CHARACTER LINE*80
      INTEGER   IRET
C
C     Local variables
C
C     HGT1    Minimum height
C     HGT2    Maximum height
C     DHGT    Height grid spacing
C
      REAL      HGT1, HGT2, DHGT
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
      READ (LINE, FMT = '(2X, 3F6.1, 60X)', IOSTAT = IRET) HGT1, HGT2,
     *                                                     DHGT
      IF (IRET .EQ. 0) THEN
         IF (HGT1 .EQ. HGT2) THEN
            ALT = 1000.0 * HGT1
            IRET = 0
         ELSE
            WRITE (MSGTXT, 9000)
            CALL MSGWRT (9)
            IRET = 1
         END IF
      ELSE
         WRITE (MSGTXT, 9001)
         CALL MSGWRT (9)
         IRET = 2
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('IONHGT: CAN ONLY HANDLE CONSTANT HEIGHT DATA')
 9001 FORMAT ('IONHGT: ERROR PARSING HEIGHT RECORD')
      END
      SUBROUTINE IONLAT (LINE, IRET)
C-----------------------------------------------------------------------
C   Parse latitude record in LINE
C
C   Input:
C      LINE   C*80    Latitide record
C
C   Output:
C      IRET   I       Return status
C                       0 - record parsed
C                       1 - invalid number of latitudes
C                       2 - parse error
C-----------------------------------------------------------------------
      CHARACTER LINE*80
      INTEGER   IRET
C
C     Local variables
C
C     LAT1    Minimum latitude
C     LAT2    Maximum latitude
C
      REAL      LAT1, LAT2
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
      READ (LINE, FMT = '(2X, 3F6.1, 60X)', IOSTAT = IRET) LAT1, LAT2,
     *                                                     DLAT
      IF (IRET .EQ. 0) THEN
         NLAT = INT (ABS(LAT2 - LAT1) / ABS (DLAT)) + 1
         IF ((1 .LE. NLAT) .AND. (NLAT .LE. MAXLAT)) THEN
            DLAT = ABS (DLAT)
            MINLAT = MIN (LAT1, LAT2)
         ELSE IF (NLAT .LT. 1) THEN
            WRITE (MSGTXT, 9000) NLAT
            CALL MSGWRT (9)
            IRET = 1
         ELSE IF (NLAT .GT. MAXLAT) THEN
            WRITE (MSGTXT, 9001) NLAT, MAXLAT
            CALL MSGWRT (9)
            IRET = 1
         END IF
      ELSE
         WRITE (MSGTXT, 9002)
         CALL MSGWRT (9)
         IRET = 2
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('IONLAT: INVALID NUMBER OF LATITUDES ', I6)
 9001 FORMAT ('IONLAT: TOO MANY LATITUDES - ', I6, ' EXCEEDS ', I6)
 9002 FORMAT ('IONLAT: ERROR PARSING LATITUDE RECORD')
      END
      SUBROUTINE IONLON (LINE, IRET)
C-----------------------------------------------------------------------
C   Parse longitude record in LINE
C
C   Input:
C      LINE   C*80    Longitude record
C
C   Output:
C      IRET   I       Return status
C                       0 - record parsed
C                       1 - invalid number of longitudes
C                       2 - parse error
C-----------------------------------------------------------------------
      CHARACTER LINE*80
      INTEGER   IRET
C
C     Local variables
C
C     LON1    Minimum longitude
C     LON2    Maximum longitude
C
      REAL      LON1, LON2
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
      READ (LINE, FMT = '(2X, 3F6.1, 60X)', IOSTAT = IRET) LON1, LON2,
     *                                                     DLONG
      IF (IRET .EQ. 0) THEN
         NLONG = INT (ABS(LON2 - LON1) / ABS (DLONG)) + 1
         IF ((1 .LE. NLONG) .AND. (NLONG .LE. MAXLON)) THEN
            DLONG = ABS (DLONG)
            MINLON = MIN (LON1, LON2)
         ELSE IF (NLONG .LT. 1) THEN
            WRITE (MSGTXT, 9000) NLONG
            CALL MSGWRT (9)
            IRET = 1
         ELSE IF (NLONG .GT. MAXLON) THEN
            WRITE (MSGTXT, 9001) NLONG, MAXLON
            CALL MSGWRT (9)
            IRET = 1
         END IF
      ELSE
         WRITE (MSGTXT, 9002)
         CALL MSGWRT (9)
         IRET = 2
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('IONLON: INVALID NUMBER OF LONGITUDES ', I6)
 9001 FORMAT ('IONLON: TOO MANY LONGITUDES - ', I6, ' EXCEEDS ', I6)
 9002 FORMAT ('IONLON: ERROR PARSING LONGITUDE RECORD')
      END
      SUBROUTINE IONEXP (LINE, IRET)
C-----------------------------------------------------------------------
C   Parse an exponent record in LINE.
C
C   Inputs:
C      LINE    C*80     Exponent record
C
C   Output:
C      IRET    I        Return status
C                         0 - parsed exponent
C                         1 - parse error
C-----------------------------------------------------------------------
      CHARACTER LINE*80
      INTEGER IRET
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
      READ (LINE, FMT = '(I6, 74X)', IOSTAT = IRET) EXPONT
      IF (IRET .NE. 0) THEN
         WRITE (MSGTXT, 9000)
         CALL MSGWRT (9)
         IRET = 1
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('IONEXP: ERROR PARSING EXPONENT RECORD')
      END
      SUBROUTINE IONMAP (LUN, FIND, IRET)
C-----------------------------------------------------------------------
C   Parse the next TEC map in an IONEX file open for reading on LUN.
C   This should be called after IONHDR.
C
C   Inputs:
C      LUN     I       LUN on which file is open
C      FIND    I       FTAB index of file
C
C   Output:
C      IRET    I       Return status
C                        0 - map read
C                        1 - bad epoch
C                        2 - bad data block
C                        3 - epoch out of order
C                        4 - bad exponent
C                       10 - I/O error
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IRET, JRET
C
C     Local variables
C
C     LINE      Line buffer
C
      CHARACTER LINE*80
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
      CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
      JRET = -1
C
C     Skip to start of TEC map, updating the current exponent if any
C     exponent records are found:
C
   10 IF ((IRET .EQ. 0)
     *    .AND. (LINE(61:80) .NE. 'START OF TEC MAP    ')) THEN
         IF (LINE(61:80) .EQ. 'EXPONENT            ') THEN
            CALL IONEXP (LINE, IRET)
            IF (IRET .EQ. 0) THEN
               CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
            ELSE
               WRITE (MSGTXT, 9010) IRET
               CALL MSGWRT (9)
               IRET = 4
            END IF
         ELSE
            CALL ZTXIO  ('READ', LUN, FIND, LINE, IRET)
         END IF
         GO TO 10
      END IF
      IF (IRET .EQ. 0) THEN
C
C        Must be the start of a TEC map.
C
         NMAPRD = NMAPRD + 1
         MAPTIM(NMAPRD) = 0.0D0
         CALL RFILL (MAXLON * MAXLAT, FBLANK, MAPS(1, 1, NMAPRD))
C
C        Parse map records:
C
   20    IF ((IRET .EQ. 0)
     *      .AND. (LINE(61:80) .NE. 'END OF TEC MAP    ')) THEN
C
C           Skip to next important record:
C
   30       IF ((IRET .EQ. 0)
     *          .AND. (LINE(61:80) .NE. 'EXPONENT            ')
     *          .AND. (LINE(61:80) .NE. 'EPOCH OF CURRENT MAP')
     *          .AND. (LINE(61:80) .NE. 'LAT/LON1/LON2/DLON/H')
     *          .AND. (LINE(61:80) .NE. 'END OF TEC MAP      ')) THEN
               CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
               GO TO 30
            END IF
            IF (IRET .EQ. 0) THEN
C
C              Parse the record:
C
               IF (LINE(61:80) .EQ. 'EXPONENT            ') THEN
                  CALL IONEXP (LINE, IRET)
                  IF (IRET .NE. 0) THEN
                     WRITE (MSGTXT, 9010) IRET
                     CALL MSGWRT (9)
                     IRET = 4
                  END IF
               ELSE IF (LINE(61:80) .EQ. 'EPOCH OF CURRENT MAP') THEN
                  CALL IONEPO (LINE, IRET)
                  IF (IRET .NE. 0) THEN
                     WRITE (MSGTXT, 9030) IRET
                     CALL MSGWRT (9)
                     IRET = 1
                  END IF
                  IF (MAPTIM(NMAPRD) .EQ. MAPTIM(NMAPRD - 1) .AND.
     *               (MAPTIM(NMAPRD)-AINT(MAPTIM(NMAPRD)).EQ.0.5)) THEN
C
C              The same time as the last record of the last file.
C              Over-write the old one, this is more likely correct.
C
                     NMAPRD = NMAPRD - 1
                     NMAPS = NMAPS - 1
                     JRET = 1
C                    GO TO 40
                  END IF
               ELSE IF (LINE(61:80) .EQ. 'LAT/LON1/LON2/DLON/H') THEN
                  CALL IONBLK (LINE, LUN, FIND, IRET)
                  IF (IRET .NE. 0) THEN
                     WRITE (MSGTXT, 9031) IRET
                     CALL MSGWRT (9)
                     IRET = 2
                  END IF
               END IF
            ELSE
               WRITE (MSGTXT, 9032) IRET
               CALL MSGWRT (9)
               IRET = 10
            END IF
C
C           Advance over parsed record
C
            CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
            IF (IRET .NE. 0) THEN
               WRITE (MSGTXT, 9032) IRET
               CALL MSGWRT (9)
               IRET = 10
            END IF
            GO TO 20
         END IF
C
C        Check for times out of order:
C
         IF ((IRET .EQ. 0) .AND. (NMAPRD .GT. 1)) THEN
            IF (MAPTIM(NMAPRD) .LE. MAPTIM(NMAPRD - 1)) THEN
               WRITE (MSGTXT, 9033)
               CALL MSGWRT (9)
               IRET = 3
            ENDIF
         END IF
      ELSE
         WRITE (MSGTXT, 9032) IRET
         CALL MSGWRT (9)
      END IF
40    IF (JRET .GT. 0) THEN
         WRITE(MSGTXT, 9034)
         CALL MSGWRT (6)
      END IF
C-----------------------------------------------------------------------
 9010 FORMAT ('IONMAP: ERROR ', I4, ' READING EXPONENT')
 9030 FORMAT ('IONMAP: ERROR ', I4, ' READING EPOCH')
 9031 FORMAT ('IONMAP: ERROR ', I4, ' READING DATA BLOCK')
 9032 FORMAT ('IONMAP: ERROR ', I4, ' READING INPUT FILE')
 9033 FORMAT ('IONMAP: FAULTY INPUT FILE - TIMES OUT OF ORDER')
 9034 FORMAT ('IONMAP: last TEC map of previous file over-written')
      END
      SUBROUTINE IONEPO (LINE, IRET)
C-----------------------------------------------------------------------
C   Parse an IONEX map epoch record in LINE.
C
C   Input:
C      LINE   C*80      Map epoch record
C
C   Output:
C      IRET   I         Return status
C                         0 - epoch read
C                         1 - parse error
C-----------------------------------------------------------------------
      CHARACTER LINE*80
      INTEGER   IRET
C
C     Local variables:
C
C     IT     Broken-down date and time
C
      INTEGER   IT(6)
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
      READ (LINE, FMT = '(6I6, 44X)', IOSTAT = IRET) IT
      IF (IRET .EQ. 0) THEN
         CALL DAT2JD (IT, MAPTIM(NMAPRD))
      ELSE
         WRITE (MSGTXT, 9000)
         CALL MSGWRT (9)
         IRET = 1
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('IONEPO: ERROR PARSING MAP EPOCH RECORD')
      END
      SUBROUTINE IONBLK (LINE, LUN, FIND, IRET)
C-----------------------------------------------------------------------
C   Read a data block from an IONEX file open on LUN and positioned
C   at the line following the 'LAT/LON1/LON2/DLON' record which should
C   be stored in LINE.
C
C   Inputs:
C      LINE    C*80     Header for the data block
C      LUN     I        Logical unit number for input file
C      FIND    I        FTAB index of input file
C
C   Output:
C      IRET    I        Return status
C                         0 - data read
C                         1 - error reading file
C                         2 - parse error
C-----------------------------------------------------------------------
      CHARACTER LINE*80
      INTEGER   LUN, FIND, IRET
C
C     Local variables
C
C     LAT      Latitude of data block (degrees)
C     LATIDX   Index of latitude in map
C     LON1     First longitude in data block (degrees)
C     LON2     Last longitude in data block (degrees)
C     DL       Longitude spacing in degrees
C     LON      Longitude of current data point
C     LONIDX   Index of longitude in map
C     H        Height of map
C     COUNT    Number of values left to read
C     NREAD    Number of values left on current line
C     OFF      Character offset in current line
C     VAL      Current value
C
      REAL     LAT, LON1, LON2, DL, LON, H
      INTEGER  LATIDX, LONIDX, COUNT, NREAD, OFF, VAL
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
      READ (LINE, FMT = '(2X, 5F6.1, 48X)', IOSTAT = IRET) LAT, LON1,
     *                                                     LON2, DL, H
      IF (IRET .EQ. 0) THEN
         LATIDX = NINT ((LAT - MINLAT) / DLAT) + 1
         LON = LON1
         COUNT = NINT ((LON2 - LON1) / DL) + 1
   10    IF ((IRET .EQ. 0) .AND. (COUNT .NE. 0)) THEN
            CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
            IF (IRET .EQ. 0) THEN
               NREAD = MIN (16, COUNT)
               OFF = 1
   20          IF ((IRET .EQ. 0) .AND. (NREAD .NE. 0)) THEN
                  READ (LINE(OFF:OFF+4), FMT = '(I5)', IOSTAT = IRET)
     *               VAL
                  IF (IRET .EQ. 0) THEN
                     LONIDX = NINT ((LON - MINLON) / DLONG) + 1
                     IF (VAL .EQ. 9999) THEN
                        MAPS(LONIDX, LATIDX, NMAPRD) = FBLANK
                     ELSE
                        MAPS(LONIDX, LATIDX, NMAPRD) = VAL
     *                     * 10.0 ** (16 + EXPONT)
                     END IF
                  ELSE
                     WRITE (MSGTXT, 9020)
                     CALL MSGWRT (9)
                     IRET = 2
                  END IF
                  OFF = OFF + 5
                  LON = LON + DL
                  NREAD = NREAD - 1
                  COUNT = COUNT - 1
                  GO TO 20
               END IF
            ELSE
               WRITE (MSGTXT, 9021) IRET
               CALL MSGWRT (9)
               IRET =1
            END IF
            GO TO 10
         END IF
      ELSE
         WRITE (MSGTXT, 9022)
         CALL MSGWRT (9)
         IRET = 2
      END IF
C-----------------------------------------------------------------------
 9020 FORMAT ('IONBLK: ERROR PARSING DATA VALUE')
 9021 FORMAT ('IONBLK: ERROR ', I4, ' READING INPUT FILE')
 9022 FORMAT ('IONBLK: ERROR PARSING DATA BLOCK HEADER')
      END
      DOUBLE PRECISION FUNCTION IONFIR()
C-----------------------------------------------------------------------
C   Earliest time covered by IONEX data.
C-----------------------------------------------------------------------
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
      IONFIR = MAPTIM(1)
      END
      DOUBLE PRECISION FUNCTION IONLAS()
C-----------------------------------------------------------------------
C   Latest time covered by IONEX data.
C-----------------------------------------------------------------------
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
      IONLAS = MAPTIM(TOTMAP)
      END
      REAL FUNCTION IONALT()
C-----------------------------------------------------------------------
C   Altitude of the ionosphere in meters.
C-----------------------------------------------------------------------
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
      IONALT = ALT
      END
      SUBROUTINE IONTEC (LFACT, LAT, LONG, TIME, TEC, IRET)
C-----------------------------------------------------------------------
C   Find the zenith TEC for LAT, LONG at TIME.
C
C   Inputs:
C      LAT     R      Latitude (radians)
C      LONG    R      Longitude (radians)
C      TIME    D      Time (Julian date)
C
C   Outputs:
C      TEC     R      Zenith TEC (electrons per square meter)
C      IRET    I      Return status:
C                      0 - TEC valid
C                      1 - data not available
C-----------------------------------------------------------------------
      REAL      LFACT, LAT, LONG
      DOUBLE PRECISION TIME
      REAL      TEC
      INTEGER   IRET
C
      INCLUDE 'INCS:PSTD.INC'
C
C     Local variables
C
C     TIDX    Index of map at latest epoch less than or equal to TIME
C     DT1     Weight for linear interpolation
C     DT2     Weight for linear interpolation
C     TEC1    Zenith TEC from map before TIME
C     TEC2    Zenith TEC from map after TIME
C     TLONG   Time-shifted longitude (degrees)
C
      INTEGER   TIDX
      DOUBLE PRECISION DT1, DT2
      REAL      TEC1, TEC2, TLONG
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'IONEX.INC'
C-----------------------------------------------------------------------
C
C     Use linear interpolation between maps bracketing the time at which
C     the zenith TEC is required. This requires maps on either side of
C     the current time.
C
      IF ((MAPTIM(1) .LE. TIME) .AND. (TIME .LT. MAPTIM(TOTMAP))) THEN
         TIDX = 0
C
C        Invariant: MAPTIM(1:TIDX) .LE. TIME
C        Bound:     TOTMAP - TIDX - 1
C
   10    IF (MAPTIM(TIDX + 1) .LE. TIME) THEN
            TIDX = TIDX + 1
            GO TO 10
         END IF
C
C        TIDX < TOTMAP and MAPTIM(TIDX) <= TIME < MAPTIM(TIDX + 1)
C
         DT1 = TIME - MAPTIM(TIDX)
         DT2 = MAPTIM(TIDX+1) - TIME
C
C        The ionosphere rotates with the sun so interpolation should
C        give better results is we adjust the longitude to account for
C        this.
C
C        At the time of the earlier map at TIDX, the ionosphere
C        currently at LONG was DT1 turns to the East (more positive).
C
         TLONG = RAD2DG * LONG + 360.0 * DT1 * LFACT
         IF (TLONG .LT. -180.0) THEN
            TLONG = TLONG + 360.0
         ELSE IF (TLONG .GT. 180.0) THEN
            TLONG = TLONG - 360.0
         END IF
         CALL TECVAL (MAPS(1, 1, TIDX), MAXLON, NLONG, NLAT, MINLON,
     *                DLONG, MINLAT, DLAT, TLONG, REAL(RAD2DG * LAT),
     *                TEC1, IRET)
         IF (IRET .EQ. 0) THEN
C
C           At the the time of the later map at TIDX + 1, the
C           ionosphere at LONG will be DT2 turns to the West (more
C           negative).
C
            TLONG = RAD2DG * LONG - 360.0 * DT2 * LFACT
            IF (TLONG .LT. -180.0) THEN
               TLONG = TLONG + 360.0
            ELSE IF (TLONG .GT. 180.0) THEN
               TLONG = TLONG - 360.0
            END IF
            CALL TECVAL (MAPS(1, 1, TIDX + 1), MAXLON, NLONG, NLAT,
     *                   MINLON, DLONG, MINLAT, DLAT, TLONG,
     *                   REAL(RAD2DG * LAT), TEC2, IRET)
            IF (IRET .EQ. 0) THEN
               TEC = (DT2 * TEC1 + DT1 * TEC2) / (DT1 + DT2)
               IRET = 0
            ELSE
               TEC = FBLANK
               IRET = 1
            END IF
         ELSE
            TEC = FBLANK
            IRET = 1
         END IF
      ELSE
         TEC = FBLANK
         IRET = 1
      END IF
      END

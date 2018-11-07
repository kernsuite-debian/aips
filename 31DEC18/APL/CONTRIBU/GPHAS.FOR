LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
      PARAMETER (NPARMS=43)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'VNPUTDATA.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       DATA statments defining input
C                                       parameters.
C                                       Uses PAOOF.INC
      DATA AVNAME /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
C                                       2
     *             'IN2NAME ', 'IN2CLASS', 'IN2SEQ  ', 'IN2DISK ',
C                                       6
     *             'CMODEL  ', 'CMETHOD ', 'SMODEL  ', 'INVERS  ',
C                                       10
     *             'BCOMP   ', 'NCOMP   ', 'FLUX    ', 'NMAPS   ',
C                                       14
     *             'OUTNAME ', 'OUTCLASS', 'OUTSEQ  ', 'OUTDISK ',
C                                       18
     *             'CALSOUR ', 'QUAL    ', 'CALCODE ',
C                                       22
     *                         'STOKES  ', 'BCHAN   ', 'ECHAN   ',
C                                       24
     *             'BIF     ', 'EIF     ', 'DOCALIB ',
C                                       28
     *                         'FREQID  ', 'SELBAND ', 'SELFREQ ',
C                                       30
     *             'DOBAND  ',             'SMOOTH  ',
C                                       33
     *             'ANTENNAS', 'SUBARRAY', 'FLAGVER ', 'GAINUSE ',
C                                       36
     *                         'BPVER   ',
C                                       39
     *             'REFANT  ', 'SOLINT  ', 'APARM   ', 'BADDISK '/
C                                       41
      DATA AVTYPE / OOACAR   ,  OOACAR   ,  OOAINT   ,  OOAINT   ,
C                                       2
     *              OOACAR   ,  OOACAR   ,  OOAINT   ,  OOAINT   ,
C                                       6
     *              OOACAR   ,  OOACAR   ,  OOARE    ,  OOAINT   ,
C                                       10
     *              OOAINT   ,  OOAINT   ,  OOARE,      OOAINT,
C                                       14
     *              OOACAR   ,  OOACAR   ,  OOAINT   ,  OOAINT   ,
C                                       18
     *              OOACAR   ,  OOAINT   ,  OOACAR   ,
C                                       22
     *                          OOACAR   ,  OOAINT   ,  OOAINT   ,
C                                       24
     *              OOAINT   ,  OOAINT   ,  OOARE   ,
C                                       28
     *                          OOAINT   ,  OOARE    ,  OOARE    ,
C                                       30
     *              OOAINT   ,              OOARE    ,
C                                       33
     *              OOAINT   ,  OOAINT   ,  OOAINT   ,  OOAINT   ,
C                                       36
     *                          OOAINT   ,
C                                       39
     *              OOAINT   ,  OOARE    ,  OOARE    ,  OOAINT   /
C                                       41
      DATA AVDIM  / 12,1     ,  6,1      ,  1,1      ,  1,1      ,
C                                       2
     *              12,1     ,  6,1      ,  1,1      ,  1,1      ,
C                                       6
     *              4,1      ,  4,1      ,  7,1      ,  1,1      ,
C                                       10
     *         MAXAFL,1,   MAXAFL,1     ,  1,1,        1,1      ,
C                                       14
     *              12,1     ,  6,1      ,  1,1      ,  1,1      ,
C                                       18
     *              16,30    ,  1,1      ,  4,1      ,
C                                       22
     *                          4,1      ,  1,1      ,  1,1      ,
C                                       24
     *              1,1      ,  1,1      ,  1,1      ,
C                                       28
     *                          1,1      ,  1,1      ,  1,1      ,
C                                       30
     *              1,1      ,              3,1      ,
C                                       33
     *              50,1     ,  1,1      ,  1,1      ,  1,1      ,
C                                       36
     *                          1,1      ,
C                                       39
     *              1,1      ,  1,1      ,  10,1     ,  10,1     /
C                                       41
LOCAL END
      PROGRAM GPHAS
C-----------------------------------------------------------------------
C! Averages several baselines into one single effective baseline
C# UV OOP VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2000, 2002-2003
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
C   All baselines from a list of selected antennas to a selected
C   'reference' antenna are averaged together and in time over
C   intervals given by SOLINT, after calibration is applied
C   and a model is divided out.  The single effective resulting
C   baseline is written to an output file.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PRGM*6, POPS*36, UVIN*36, IMIN(MAXFLD)*36, UVOUT*36
      CHARACTER UVSCR*36, PIMIN*36
      INTEGER  IRET
C
      PARAMETER (PRGM = 'GPHAS ')
      PARAMETER (POPS = 'Input ADVob')
      PARAMETER (UVIN = 'Input UVob')
      PARAMETER (PIMIN = 'Pseudo Input IMob')
      PARAMETER (UVOUT = 'Output UVob')
      PARAMETER (UVSCR = 'Scratch UVob')
C
      INTEGER   TYPE, DIM(7), MFIELD, BUFF(256)
      CHARACTER CDUM*1
      LOGICAL   DIVMOD, DOPTMD
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Get AIPS Adverb values
      CALL GETADV (PRGM, POPS, MFIELD, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Prep UVIN using POPS
      CALL GUVIN (POPS, UVIN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Prep UVOUT using UVIN
      CALL GUVOUT (UVIN, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Calibrate/Select UVIN into UVSCR
      CALL GUVSCR (UVIN, UVSCR, MFIELD, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Divide data by model?
      CALL OGET (UVSCR, 'DIVMOD', TYPE, DIM, DIVMOD, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (DIVMOD) THEN
C                                       Prep IMIN using POPS
         CALL OGET (UVSCR, 'MODDOPT', TYPE, DIM, DOPTMD, CDUM, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       In-place divide UVSCR by pt
C                                       source model
         IF (DOPTMD) THEN
            CALL GUVDIV (UVSCR, 1, PIMIN, IRET)
            IF (IRET.NE.0) GO TO 990
C                                       In-place divide IMIN into UVSCR
         ELSE
            CALL GIMIN (POPS, MFIELD, IMIN, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL GUVDIV (UVSCR, MFIELD, IMIN, IRET)
            IF (IRET.NE.0) GO TO 990
            END IF
         END IF
C                                       Average UVSCR into UVOUT
      CALL GPHASE (UVSCR, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy tables from UVIN to UVOUT
      CALL GCPTAB (UVIN, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Write History from UVIN to UVOUT
      CALL GWRTHI (POPS, UVIN, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Done
      GO TO 995
C                                       Error handling stuff
 990  WRITE (MSGTXT,1990) IRET
      CALL MSGWRT(6)
C
 995  CALL DIE (IRET, BUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1990 FORMAT ('GPHAS: error ',I5)
      END
      SUBROUTINE GETADV (PRGM, POPS, MFIELD, IRET)
C-----------------------------------------------------------------------
C   GETADV gets adverb values from AIPS and fills them into POPSob
C   Inputs:
C      PRGM    C*?  program name
C      POPS    C*?  memory object describing AIPS adverb values
C   Output:
C      MFIELD  I    Number of fields
C      IRET    I    error code: 0 => ok
C-----------------------------------------------------------------------
C                                       I/O parameters
      INTEGER   MFIELD, IRET
      CHARACTER PRGM*(*), POPS*(*)
C                                       Std. OOP declarations
      INTEGER   DIM(7), TYPE, IDUM
      CHARACTER CDUM*1
      REAL      RDUM
      LOGICAL   LDUM
C                                       Enumerate number of adverbs
      INCLUDE 'INPUT.INC'
C                                       Enumerate available data types
      INCLUDE 'INCS:PAOOF.INC'
C                                       AIPS errorlog facilities
      INCLUDE 'INCS:DMSG.INC'
C                                       AIPS baddisk mechanism
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       List adverb names
      INCLUDE 'VNPUTDATA.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Create/Fill adverbs into POPSob
      CALL AV2INP (PRGM, NPARMS, AVNAME, AVTYPE, AVDIM, POPS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Allow choice of BADDISKs
      CALL OGET (POPS, 'BADDISK', TYPE, DIM, IBAD, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Allow choice of BADDISKs
      CALL OGET (POPS, 'NMAPS', TYPE, DIM, MFIELD, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
      MFIELD = MAX (1, MIN (MAXFLD, MFIELD))
C                                       Save Integer keyword type
      TYPE = OOAINT
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (POPS, 'INTEGER', TYPE, DIM, IDUM, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Save Logical keyword type
      TYPE = OOALOG
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (POPS, 'LOGICAL', TYPE, DIM, LDUM, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Save Real keyword type
      TYPE = OOARE
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (POPS, 'REAL', TYPE, DIM, RDUM, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Done
      GO TO 999
C                                       Error handling stuff
 990  WRITE (MSGTXT,1990) IRET
      CALL MSGWRT(6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('GETADV: error ',I5)
      END
      SUBROUTINE GUVIN (POPS, UVIN, IRET)
C-----------------------------------------------------------------------
C   Create UVINob and fill using POPSob, check underlying data-set
C   properties
C   Inputs:
C      POPS    C*?  memory object describing AIPS adverbs
C   Output:
C      UVIN    C*?  memory object describing input UV data set
C      IRET    I    Error code: 0 => ok
C-----------------------------------------------------------------------
C                                       I/O parameters
      INTEGER   IRET
      CHARACTER POPS*(*), UVIN*(*)
C                                       OOP keyword transfer variables
      INTEGER   NKEY0
      PARAMETER (NKEY0=3)
      CHARACTER IKEY0(NKEY0)*8, OKEY0(NKEY0)*16
C
      INTEGER   NKEY1
      PARAMETER (NKEY1=4)
      CHARACTER IKEY1(NKEY1)*8, OKEY1(NKEY1)*16
C
      INTEGER   NKEY2
      PARAMETER (NKEY2=18)
      CHARACTER IKEY2(NKEY2)*8, OKEY2(NKEY2)*16
C
      INTEGER   NKEY3
      PARAMETER (NKEY3=6)
      CHARACTER IKEY3(NKEY3)*8, OKEY3(NKEY3)*16
C
      INTEGER   NKEY4
      PARAMETER (NKEY4=7)
      CHARACTER IKEY4(NKEY4)*8, OKEY4(NKEY4)*16
C                                       Std OOP declarations
      INTEGER   TYPE, DIM(7), IDUM
      CHARACTER CDUM*1
C                                       Internal variables
      INTEGER SPCANT, IANTEN(50), I, J, SUBARR, NEWANT,
     *   NANT, CLUSE, NROW, REFCOL, METHOD
      REAL      APARM(10), XDOCAL
      LOGICAL   T, F, HAVREF, USEALL, DIVMOD, LDUM, ISCOMP
      INTEGER   ABSEL, BSELET(50), ASELET(50)
      LOGICAL   L1VIS, LTEMP
      CHARACTER SORD*2, ANTAB*36, CLTAB*36
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C                                       Xfer keyword types
      DATA IKEY0 /'INTEGER ', 'LOGICAL ', 'REAL'/
      DATA OKEY0 /'INTEGER ', 'LOGICAL ', 'REAL'/
C                                       File specification info
      DATA IKEY1 /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  '/
      DATA OKEY1 /'FILE_NAME.NAME  ', 'FILE_NAME.CLASS ',
     *            'FILE_NAME.IMSEQ ', 'FILE_NAME.DISK  '/
C                                       Cal/Selection info
      DATA IKEY2 /'CALSOUR ', 'QUAL    ', 'CALCODE ',
     *                        'STOKES  ', 'BCHAN   ', 'ECHAN   ',
     *            'BIF     ', 'EIF     ',
     *                        'FREQID  ', 'SELBAND ', 'SELFREQ ',
     *            'DOBAND  ',             'SMOOTH  ',
     *            'ANTENNAS', 'SUBARRAY', 'FLAGVER ', 'GAINUSE ',
     *                        'BPVER   '/
      DATA OKEY2 /'CALEDIT.SOURCS  ', 'CALEDIT.SELQUA  ',
     *            'CALEDIT.SELCOD  ',
     *                                'CALEDIT.STOKES  ',
     *            'CALEDIT.BCHAN   ', 'CALEDIT.ECHAN   ',
     *            'CALEDIT.BIF     ', 'CALEDIT.EIF     ',
     *                                'CALEDIT.FRQSEL  ',
     *            'CALEDIT.SELBAN  ', 'CALEDIT.SELFRQ  ',
     *            'CALEDIT.DOBAND  ',
     *                                'CALEDIT.SMOOTH  ',
     *            'CALEDIT.ANTENNS ', 'CALEDIT.SUBARR  ',
     *            'CALEDIT.FGVER   ', 'CALEDIT.CLUSE   ',
     *            'CALEDIT.BPVER   '/
C                                       Output file specification info
      DATA IKEY3 /'OUTNAME ', 'OUTCLASS', 'OUTSEQ  ', 'OUTDISK ',
     *            'REFANT  ', 'SOLINT  '/
      DATA OKEY3 /'OUTNAME ', 'OUTCLASS', 'OUTSEQ  ', 'OUTDISK ',
     *            'REFANT  ', 'SOLINT  '/
C                                       Image division info
      DATA IKEY4 /'CMODEL  ', 'CMETHOD ', 'SMODEL  ', 'INVERS  ',
     *            'BCOMP   ', 'NCOMP   ', 'FLUX'/
      DATA OKEY4 /'CMODEL  ', 'CMETHOD ', 'SMODEL  ', 'INVERS  ',
     *            'BCOMP   ', 'NCOMP   ', 'FLUX'/

      DATA T, F /.TRUE., .FALSE./
      DATA ANTAB /'Scratch Antenna Table'/
      DATA CLTAB /'Requested Calibration Table'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Get APARM list from POPSob
      CALL OGET (POPS, 'APARM', TYPE, DIM, APARM, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get REFANT from POPSob
      CALL OGET (POPS, 'REFANT  ', TYPE, DIM, NEWANT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get ANTENNA list from POPSob
      CALL OGET (POPS, 'ANTENNAS', TYPE, DIM, IANTEN, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get special antenna
      SPCANT = INT(APARM(1))
C                                       set default behaviour
      IF (NEWANT.EQ.0) APARM(2)=2
C                                       get method for determining REFANT
      METHOD = INT(APARM(2))
C
      L1VIS = APARM(3).GT.0
C                                       remove all zeroes
      USEALL = T
      I = 1
      DO 10, J=1,50
         IF (IANTEN(J).NE.0) THEN
            HAVREF = HAVREF.OR.(IANTEN(J).EQ.SPCANT)
            IANTEN(I) = IANTEN(J)
            IF (J.GT.I) IANTEN(J) = 0
            I = I+1
            USEALL = F
            END IF
 10      CONTINUE
C                                       Check if special antenna is present
C                                       Add special antenna if not present
      IF (.NOT.(HAVREF.OR.USEALL)) THEN
         IF(I.LE.50) THEN
            IANTEN(I) = SPCANT
         ELSE
            MSGTXT = 'GDUVIN: replacing last antenna with REFANT'
            IANTEN(50) = SPCANT
            END IF
         END IF
C                                       Order ants. by increasing #
      DO 20, I = 1,49
         DO 25, J = I+1,50
            IF ((ABS(IANTEN(I)).GT.ABS(IANTEN(J)))
     *         .AND.(IANTEN(J).NE.0)) THEN
               IANTEN(I) = IANTEN(I) - IANTEN(J)
               IANTEN(J) = IANTEN(J) + IANTEN(I)
               IANTEN(I) = IANTEN(J) - IANTEN(I)
               END IF
 25         CONTINUE
 20      CONTINUE
C                                       Put new list back in POPSob
      CALL OPUT (POPS, 'ANTENNAS', TYPE, DIM, IANTEN, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Check AN table number
      CALL OGET (POPS, 'SUBARRAY', TYPE, DIM, SUBARR, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (SUBARR.EQ.0) THEN
         MSGTXT = 'GUVIN: Selecting Subarray 1'
         CALL MSGWRT(6)
         SUBARR = 1
C                                       Save Subarray #
         CALL OPUT (POPS, 'SUBARRAY', TYPE, DIM, SUBARR, CDUM, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       Create UVINob
      CALL CREATE (UVIN, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 990
      CALL IN2OBJ (POPS, NKEY0, IKEY0, OKEY0, UVIN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Specify UVINob
      CALL IN2OBJ (POPS, NKEY1, IKEY1, OKEY1, UVIN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Select/Calibrate UVINob
      CALL IN2OBJ (POPS, NKEY2, IKEY2, OKEY2, UVIN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Prep for UVOUTob
      CALL IN2OBJ (POPS, NKEY3, IKEY3, OKEY3, UVIN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Prep for UVOUTob
      CALL IN2OBJ (POPS, NKEY4, IKEY4, OKEY4, UVIN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       DOCALIB
      CALL OGET (POPS, 'DOCALIB', TYPE, DIM, XDOCAL, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
      LTEMP = XDOCAL.GT.0.0
      CALL OPUT (UVIN, 'CALEDIT.DOCAL', OOALOG, DIM, LTEMP, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
      LTEMP = XDOCAL.GE.1.5
      CALL OPUT (UVIN, 'CALEDIT.DOWTCL', OOALOG, DIM, LTEMP, CDUM,
     *   IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get state
      CALL OUVATT (UVIN, F, IRET)
C                                       get compression state
      CALL OGET (UVIN, 'UV_DESC.ISCOMP', TYPE, DIM, ISCOMP, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       save compression state
      CALL OPUT (UVIN, 'DOUVCOMP', TYPE, DIM, ISCOMP, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Open UVINob
      CALL OOPEN (UVIN, 'READ', IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Check Sort Order, must be 'TB'
      CALL OGET (UVIN, 'UV_DESC.SORTORD', TYPE, DIM, IDUM, SORD, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (SORD.NE.'TB') THEN
         MSGTXT = 'GUVIN: Sorry, data not in TB order, must run UVSRT'
         CALL MSGWRT(6)
         IRET = 1
         GO TO 999
         END IF
C                                       SET MODEL DIVISION FLAG
C
C                                       Check Source Axis, must not exist
      CALL UVDFND (UVIN, 1, 'SOURCE', I, IRET)
      IF (IRET.EQ.0) THEN
C                                       IRET = 0 means source axis
C                                       still exists
         MSGTXT = 'GUVIN: Sorry, you selected more than one source,'
         CALL MSGWRT (6)
         MSGTXT = '       AND, Im quitting - RIGHT NOW!'
         CALL MSGWRT (6)
         IRET = 1
         GO TO 999
      ELSE
C                                       IRET = -1 means source axis
C                                       removed
C                                       IRET = 1 means source axis does
C                                              not exist
         DIVMOD = .TRUE.
         END IF
      CALL OGET (UVIN, 'LOGICAL', TYPE, DIM, LDUM, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL OPUT (UVIN, 'DIVMOD ', TYPE, DIM, DIVMOD, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C
C                                       SET ANTENNA NUMBER FOR NEW BASELINE
C
C                                       Save AN table #
      CALL OGET (UVIN, 'INTEGER', TYPE, DIM, IDUM, CDUM, IRET)
      CALL OPUT (UVIN, 'ANVER', TYPE, DIM, SUBARR, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Put chosen METHOD in POPSob
      CALL OGET (UVIN, 'INTEGER ', TYPE, DIM, IDUM, CDUM, IRET)
      CALL OPUT (UVIN, 'METHOD  ', TYPE, DIM, METHOD, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Establish antenna number to use
C                                       for effective baseline
      IF ((METHOD.EQ.2).OR.(METHOD.EQ.1)) THEN
C                                       Determine the number of antennas
C                                       in the AN table
C                                       Open AN table
         CALL UV2TAB (UVIN, ANTAB, 'AN', SUBARR, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL TABOPN (ANTAB, 'READ', IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Get # of antennas in AN table
         CALL ANTNO (ANTAB, SUBARR, NANT, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Close AN table
         CALL TABCLO (ANTAB,IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      IF (METHOD.EQ.2) THEN
C                                       Use most commonly occuring
C                                       REFANT in chosen CL table
C                                       as NEWANT
C
C                                       Get CL table number
         CALL OGET (UVIN, 'CALEDIT.CLUSE', TYPE, DIM, CLUSE, CDUM, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Open CL table
         CALL UV2TAB (UVIN, CLTAB, 'CL', CLUSE, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL TABOPN (CLTAB, 'READ', IRET)
         IF (IRET.NE.0) GO TO 990
         CALL TABCOL (CLTAB, 1, 'REFA', REFCOL, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Get # of rows in CL table
         CALL TABGET (CLTAB, 'NROW', TYPE, DIM, NROW, CDUM, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Initialize ref.ant. counter
         DO 100 I=1,NANT
            IANTEN(I)= 0
 100        CONTINUE
C                                       count #appearances of each
C                                       ant as refant
         DO 110 I = 1,NROW
            CALL TABDGT (CLTAB, I, REFCOL, TYPE, DIM, NEWANT, CDUM,
     *         IRET)
            IF (IRET.NE.0) GO TO 990
            IANTEN(NEWANT)= IANTEN(NEWANT) + 1
 110        CONTINUE
C                                       determine most frequent
C                                       refant
            NEWANT = 1
            DO 120 I = 1,NANT
               IF (IANTEN(I).GT.IANTEN(NEWANT)) NEWANT=I
 120           CONTINUE
C                                       Close CL table
            CALL TABCLO (CLTAB,IRET)
            IF (IRET.NE.0) GO TO 990
         END IF
      IF (METHOD.EQ.1) THEN
C                                       Use least upper bound of
C                                       AN table
         NEWANT = NANT + 1
         END IF
C                                       Put NEWANT into UVINob
      CALL OGET (UVIN, 'INTEGER', TYPE, DIM, IDUM, CDUM, IRET)
      CALL OPUT (UVIN, 'NEWANT', TYPE, DIM, NEWANT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Put Vis Norm Flag into UVINob
      CALL OGET (UVIN, 'LOGICAL', TYPE, DIM, LDUM, CDUM, IRET)
      CALL OPUT (UVIN, 'L1VIS', TYPE, DIM, L1VIS, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Save ANTENNA and BASELINE
C                                       SELECTION PARAMETERS HERE
C
C                                       get ANTENNA adverb back
      CALL OGET (POPS, 'ANTENNAS', TYPE, DIM, ASELET, CDUM, IRET)
C                                       save it as ASELECT
      CALL OPUT (UVIN, 'ASELECT', TYPE, DIM, ASELET, CDUM, IRET)
C                                       save special antenna as BSELECT
      BSELET(1) = SPCANT
      CALL OPUT (UVIN, 'BSELECT', TYPE, DIM, BSELET, CDUM, IRET)
C                                       coun the non-zero length of
C                                       ASELECT
      DO 30 I = 1,50
         IF (ASELET(I).GT.0) ABSEL = I
 30      CONTINUE
C                                       encode lengths of ASELECT and
C                                       BSELECT
      ABSEL = ABSEL*256 + 1
      CALL OGET (UVIN, 'INTEGER', TYPE, DIM, IDUM, CDUM, IRET)
C                                       save lengths as ABSELECT
      CALL OPUT (UVIN, 'ABSELECT', TYPE, DIM, ABSEL, CDUM, IRET)
C
C                                       Close input object
      CALL OCLOSE (UVIN, IRET)
      IF (IRET.EQ.0) GO TO 999
C                                       Error handling stuff
 990  CONTINUE
      WRITE (MSGTXT, 1990) IRET
      CALL MSGWRT(6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('GUVIN: error ',I5)
      END
      SUBROUTINE GUVOUT (UVIN, UVOUT, IRET)
C-----------------------------------------------------------------------
C   GUVOUT clones UVINob into UVOUTob
C   Inputs:
C      UVIN    C*?   Name of input uvdata object.
C   Output:
C      UVOUT   C*?   Name of output uvdata object.
C      IRET    I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IRET
C
      INTEGER   NKEY0
      PARAMETER (NKEY0=3)
      CHARACTER IKEY0(NKEY0)*8, OKEY0(NKEY0)*16
C
      INTEGER TYPE, DIM(3)
      CHARACTER CDUM*1
C
      INTEGER NEWANT, COUNT
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA IKEY0 /'INTEGER ', 'LOGICAL ', 'REAL'/
      DATA OKEY0 /'INTEGER ', 'LOGICAL ', 'REAL'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Create UVOUTob from UVINob
      CALL OCLONE (UVIN, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL IN2OBJ (UVIN, NKEY0, IKEY0, OKEY0, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       NOTE: UVIN, UVOUT need not be
C                                         open for OGET and OPUT
C                                           this is information stored
C                                           in the object and not in
C                                           the underlying file
C                                       Get new antenna number
      CALL OGET (UVIN, 'NEWANT' , TYPE, DIM, NEWANT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Save new antenna number
      CALL OPUT (UVOUT, 'NEWANT', TYPE, DIM, NEWANT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Zero numvis. in UVOUTob
      CALL OGET (UVOUT, 'UV_DESC.GCOUNT', TYPE, DIM, COUNT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
      COUNT = 0
      CALL OPUT (UVOUT, 'UV_DESC.GCOUNT', TYPE, DIM, COUNT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Done
      GO TO 999
C                                       Error handling stuff
 990  CONTINUE
      WRITE (MSGTXT, 1990) IRET
      CALL MSGWRT(6)
 999  RETURN
 1990 FORMAT ('GUVOUT: error ',I5)
      END
      SUBROUTINE GUVSCR (UVIN, UVSCR, MFIELD, IRET)
C-----------------------------------------------------------------------
C   GUVSCR creates UVOUTob
C   Inputs:
C      UVIN    C*?  memory object describing input UV data object
C      UVSCR   C*?  memory object describing output UV data object
C      MFIELD  I    Number of fields
C   Output:
C      IRET    I    Error code: 0 => ok, else failed.
C-----------------------------------------------------------------------
      INTEGER   MFIELD, IRET
      CHARACTER UVIN*(*), UVSCR*(*)
C
      INTEGER   NKEY0
      PARAMETER (NKEY0=3)
      CHARACTER IKEY0(NKEY0)*8, OKEY0(NKEY0)*16
C
      INTEGER   NKEY1
      PARAMETER (NKEY1=11)
      CHARACTER IKEY1(NKEY1)*8, OKEY1(NKEY1)*16
C
      INTEGER   TYPE, DIM(7), IDUM
      CHARACTER CDUM*1
      LOGICAL   LDUM
      REAL      RDUM
C
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   T, DOPTMD, NONNEG, DIVMOD, WASOME
      REAL      SMODEL(4), PTFLUX, PTRA, PTDEC
      INTEGER   COMPNO(MAXFLD), I
      CHARACTER CMODEL*4, CMETH*4
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA IKEY0 /'INTEGER ', 'LOGICAL ', 'REAL'/
      DATA OKEY0 /'INTEGER ', 'LOGICAL ', 'REAL'/
C
      DATA IKEY1 /'OUTNAME ', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *            'REFANT  ', 'SOLINT  ', 'NEWANT  ', 'ANVER',
     *            'DIVMOD  ', 'L1VIS   ', 'DOUVCOMP'/
      DATA OKEY1 /'OUTNAME ', 'OUTCLASS', 'OUTSEQ', 'OUTDISK',
     *            'REFANT  ', 'SOLINT  ', 'NEWANT  ', 'ANVER',
     *            'DIVMOD  ', 'L1VIS   ', 'DOUVCOMP'/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Create UVOUTob
      CALL CREATE (UVSCR, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 990
C
      CALL IN2OBJ (UVIN, NKEY0, IKEY0, OKEY0, UVSCR, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy UVINob to UVOUTob
C                                        perform calibration/selection
      CALL IN2OBJ (UVIN, NKEY1, IKEY1, OKEY1, UVSCR, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UV2SCR (UVIN, UVSCR, T, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Open UVOUTob
      CALL OOPEN (UVSCR, 'READ', IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close UVOUTob
      CALL OCLOSE (UVSCR, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Check if model division requested
      CALL OGET (UVSCR, 'DIVMOD', TYPE, DIM, DIVMOD, CDUM, IRET)
      IF (DIVMOD) THEN
C                                       What type of model? (default=CC)
         CALL OGET (UVIN, 'CMODEL'   , TYPE, DIM, IDUM, CMODEL, IRET)
         IF (IRET.NE.0) GO TO 990
         IF (CMODEL.NE.'PT  ') CMODEL = 'CC  '
         CALL OPUT (UVSCR, 'MODMODEL', TYPE, DIM, IDUM, CMODEL, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Point source model is default
         DOPTMD = CMODEL.EQ.'PT  '
         CALL OGET (UVSCR, 'LOGICAL', TYPE, DIM, LDUM  , CDUM, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL OPUT (UVSCR, 'MODDOPT', TYPE, DIM, DOPTMD, CDUM, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       what kind of FT to do?
         CALL OGET (UVIN, 'CMETHOD' , TYPE, DIM, IDUM, CMETH, IRET)
         IF (IRET.NE.0) GO TO 990
         IF (DOPTMD) CMETH = 'DFT '
         IF ((CMETH.NE.'DFT ').AND.(CMETH.NE.'GRID')) CMETH = '    '
         CALL OPUT (UVSCR, 'MODMETH', TYPE, DIM, IDUM, CMETH,  IRET)
         IF (IRET.NE.0) GO TO 990
C                                       save pt source parameters
         IF (CMODEL.EQ.'PT  ') THEN
            MSGTXT = 'GUVSCR: use pt source model'
            CALL MSGWRT(6)
            CALL OGET (UVIN, 'SMODEL', TYPE, DIM, SMODEL, CDUM, IRET)
            IF (IRET.NE.0) GO TO 990
            PTFLUX = SMODEL(1)
            IF (PTFLUX.LE.0.0) THEN
               MSGTXT = 'GUVSCR: CMODEL=PT, specified flux <=0,'
               CALL MSGWRT(6)
               MSGTXT = '       setting Flux = 1'
               PTFLUX = 1.0
               END IF
            PTRA = SMODEL(2)
            PTDEC = SMODEL(3)
            CALL OGET (UVSCR, 'REAL',     TYPE, DIM, RDUM  , CDUM, IRET)
            CALL OPUT (UVSCR, 'MODPTFLX', TYPE, DIM, PTFLUX, CDUM, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL OPUT (UVSCR, 'MODPTXOF', TYPE, DIM, PTRA,   CDUM, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL OPUT (UVSCR, 'MODPTYOF', TYPE, DIM, PTDEC,  CDUM, IRET)
            IF (IRET.NE.0) GO TO 990
C
         ELSE
C                                       save CC table parameters
            CALL OGET (UVIN, 'FLUX',    TYPE, DIM, RDUM, CDUM, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL OPUT (UVSCR, 'MODFLUX', TYPE, DIM, RDUM, CDUM, IRET)
            IF (IRET.NE.0) GO TO 990
C
            CALL OGET (UVIN, 'INVERS',    TYPE, DIM, IDUM, CDUM, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL FILL (MAXFLD, IDUM, COMPNO)
            DIM(1) = MFIELD
            CALL OPUT (UVSCR, 'MODCCVER', TYPE, DIM, COMPNO, CDUM, IRET)
            IF (IRET.NE.0) GO TO 990
C
            CALL FILL (MAXFLD, 1, COMPNO)
            CALL OGET (UVIN, 'BCOMP',     TYPE, DIM, COMPNO, CDUM, IRET)
            IF (IRET.NE.0) GO TO 990
            COMPNO(1) = MAX(1,COMPNO(1))
            DIM(1) = MFIELD
            CALL OPUT (UVSCR, 'MODCCBEG', TYPE, DIM, COMPNO, CDUM, IRET)
            IF (IRET.NE.0) GO TO 990
C
            CALL FILL (MAXFLD, 0, COMPNO)
            CALL OGET (UVIN, 'NCOMP',     TYPE, DIM, COMPNO, CDUM, IRET)
            IF (IRET.NE.0) GO TO 990
            NONNEG = .FALSE.
            WASOME = .FALSE.
            DO 20 I = 1,MFIELD
               IF (COMPNO(I).LT.0) THEN
                  NONNEG = .TRUE.
                  COMPNO(I) = -COMPNO(1)
                  END IF
               IF (COMPNO(I).GT.0) WASOME = .TRUE.
 20            CONTINUE
            IF (WASOME) CALL FILL (MAXFLD-MAXAFL, 1000000000,
     *         COMPNO(MAXAFL+1))
            DIM(1) = MFIELD
            CALL OPUT (UVSCR, 'MODCCEND', TYPE, DIM, COMPNO, CDUM, IRET)
            IF (IRET.NE.0) GO TO 990
C
            CALL OGET (UVSCR, 'LOGICAL',  TYPE, DIM, LDUM  , CDUM, IRET)
            CALL OPUT (UVSCR, 'MODNONEG', TYPE, DIM, NONNEG, CDUM, IRET)
            IF (IRET.NE.0) GO TO 990
            END IF
         END IF
C                                       Done
      GO TO 999
C                                       Error handling stuff
 990  WRITE (MSGTXT,1990) IRET
      CALL MSGWRT(6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('GUVSCR: error ',I5)
      END
      SUBROUTINE GIMIN (POPS, MFIELD, IMIN, IRET)
C-----------------------------------------------------------------------
C   GIMIN preps IMINob using POPSob for later use
C   Inputs:
C      POPS    C*?     Program name
C      MFIELD  I       Number of fields
C   Output:
C      IMIN    C(*)*?  memory object describing input image
C      IRET    I       Error code: 0 => ok, else failed.
C-----------------------------------------------------------------------
      INTEGER   MFIELD, IRET
      CHARACTER POPS*(*), IMIN(*)*(*)
C
      INTEGER   NKEY0
      PARAMETER (NKEY0=3)
      CHARACTER IKEY0(NKEY0)*8, OKEY0(NKEY0)*16
C
      INTEGER   NKEY1
      PARAMETER (NKEY1=4)
      CHARACTER IKEY1(NKEY1)*8, OKEY1(NKEY1)*16
      INTEGER   I, J, TYPE, DIM(7), IDUM, J0, JJ
      LOGICAL   OLDNAM
      CHARACTER INCLAS*6
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA IKEY0 /'INTEGER ', 'LOGICAL ', 'REAL'/
      DATA OKEY0 /'INTEGER ', 'LOGICAL ', 'REAL'/
C
      DATA IKEY1 /'IN2NAME ', 'IN2CLASS', 'IN2SEQ  ', 'IN2DISK '/
      DATA OKEY1 /'FILE_NAME.NAME  ', 'FILE_NAME.CLASS ',
     *            'FILE_NAME.IMSEQ ', 'FILE_NAME.DISK  '/
C-----------------------------------------------------------------------
      IRET = 0
      MFIELD = MAX (1, MFIELD)
      DO 100 I = 1,MFIELD
         WRITE (IMIN(I),1000) I
C                                       Create IMob
         CALL CREATE (IMIN(I), 'IMAGE', IRET)
         IF (IRET.NE.0) GO TO 990
         CALL IN2OBJ (POPS, NKEY0, IKEY0, OKEY0, IMIN(I), IRET)
         IF (IRET.NE.0) GO TO 990
C                                       get image file
C                                       Copy POPSob to IMob
         CALL IN2OBJ (POPS, NKEY1, IKEY1, OKEY1, IMIN(I), IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Fix class
         CALL OGET (IMIN(I), 'FILE_NAME.CLASS', TYPE, DIM, IDUM, INCLAS,
     *      IRET)
         IF (IRET.NE.0) GO TO 990
         IF (I.EQ.1) THEN
            OLDNAM = (INCLAS(4:4).LT.'0') .OR. (INCLAS(4:4).GT.'9') .OR.
     *         (INCLAS(5:5).LT.'0') .OR. (INCLAS(5:5).GT.'9') .OR.
     *         (INCLAS(6:6).LT.'0') .OR. (INCLAS(6:6).GT.'9')
            IF (.NOT.OLDNAM) THEN
               IF ((INCLAS(3:3).LT.'0') .OR. (INCLAS(3:3).GT.'9')) THEN
                  READ (INCLAS(4:6),'(I3)') J0
               ELSE
                  READ (INCLAS(3:6),'(I4)') J0
                  END IF
               END IF
         ELSE
            IF (OLDNAM) THEN
               J = I - 1
               CALL ZEHEX (J, 2, INCLAS(5:6))
            ELSE
               J = I - 1 + J0
               IF (J.LE.999) THEN
                  WRITE (INCLAS(4:6),1001) J
               ELSE
                  WRITE (INCLAS(3:6),1002) J
                  END IF
               END IF
            CALL OPUT (IMIN(I), 'FILE_NAME.CLASS', TYPE, DIM, IDUM,
     *         INCLAS, IRET)
            IF (IRET.NE.0) GO TO 990
            END IF
C                                       Test for file existence
C                                       Open/Close IMob
         CALL OOPEN (IMIN(I), 'READ', IRET)
         IF (IRET.NE.0) GO TO 990
         CALL OCLOSE (IMIN(I), IRET)
         IF (IRET.NE.0) GO TO 990
 100     CONTINUE
C                                       Done
      GO TO 999
C                                       Error handling stuff
 990  CONTINUE
      WRITE (MSGTXT,1990) IRET
      CALL MSGWRT(6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Image object',I4)
 1001 FORMAT (I3.3)
 1002 FORMAT (I4.4)
 1990 FORMAT ('GIMIN: error ',I5)
      END
      SUBROUTINE GUVDIV (UVIN, MFIELD, IMIN, IRET)
C-----------------------------------------------------------------------
C   GUVDIV divides UVIN by IMIN
C   Inputs:
C      UVIN    C*?  memory object describing input UV data object
C      IMIN    C*?  memory object describing Input IM data object
C      MFIELD  I    Number of fields
C   Output:
C      UVIN    C*?  memory object describing output UV data object
C      IRET    I    Error code: 0 => ok, else failed.
C-----------------------------------------------------------------------
      INTEGER   MFIELD, IRET
      CHARACTER UVIN*(*), IMIN(*)*(*)
C
      INTEGER   TYPE, DIM(7)
      CHARACTER CDUM*1
C
      INTEGER   ICHANF, ICHANL
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       selected/calibrated data is now
C                                         in UVIN [uncompressed!]
C
C                                       First divided channel is 1
      ICHANF = 1
C                                       Last divided channel is NCORR
      CALL OGET (UVIN, 'UV_DESC.NCORR', TYPE, DIM, ICHANL, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       in-place divide UVINob by the
C                                       NFIELD IMobs given by IMLIST
C                                       over channels ICHANF to ICHANL
      CALL UVDIVM (UVIN, UVIN, MFIELD, IMIN, ICHANF, ICHANL, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Done
      GO TO 999
C                                       Error handling stuff
 990  WRITE (MSGTXT, 1990) IRET
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('GUVDIV: error ',I5)
      END
      SUBROUTINE GPHASE (UVIN, UVOUT, IRET)
C-----------------------------------------------------------------------
C   GPHASE averages selected baselines into a single effective baseline
C   + time averaging of a uv data set.
C   Inputs:
C      UVIN    C*?   memory object describing input UV data object
C      UVOUT   C*?   memory object describing output UV data object
C   Output:
C       IRET   I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IRET
C                                       MXVS = maximum #corr in a record
      INTEGER   MXVS
      PARAMETER (MXVS = 2048)
C
      INTEGER TYPE, DIM(3), IDUM
      CHARACTER CDUM*1, SORD*2
C
      REAL SOLINT
      INTEGER   ILOCU, ILOCV, ILOCW, ILOCT, ILOCB, ILOCSU, ILOCFQ,
     *     ILOCIT, LREC, NRPARM, NCOR, SPCANT, COUNT
C                                        Local Info for uv util.
      REAL     RP(50), VS(3,MXVS), VSSAMP
C
      LOGICAL DOSU, DOFQ, NEXT, LAST, T, F, NEXTSU, NEXTFQ, NEXTT,
     *   L1VIS
C
      INTEGER   CNT, LOOP, ISTATA, ISTATB, NEWANT
      INTEGER   VSCNT(MXVS)
      REAL ENDTIM, RPS(50), VSS(3,MXVS), SENSE
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA T,F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Open UVINob
      CALL OOPEN (UVIN, 'READ', IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get pointer for baseline code
      CALL UVDFND (UVIN, 1, 'BASELINE', ILOCB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get pointer for u-coordinate
      CALL UVDFND (UVIN, 1, 'UU', ILOCU, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get pointer for v-coordinate
      CALL UVDFND (UVIN, 1, 'VV', ILOCV, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get pointer for w-coordinate
      CALL UVDFND (UVIN, 1, 'WW', ILOCW, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get pointer for integration time
      CALL UVDFND (UVIN, 1, 'TIME', ILOCT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get pointer for baseline code
      CALL UVDFND (UVIN, 1, 'BASELINE', ILOCB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get pointer for source parameter
      CALL UVDFND (UVIN, 1, 'SOURCE', ILOCSU, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'GPHASE: No source id random parameter found'
         CALL MSGWRT(6)
         ILOCSU = -1
         IRET = 0
         END IF
      DOSU = ILOCSU.GT.0
C                                       Get pointer for frequency
      CALL UVDFND (UVIN, 1, 'FREQ', ILOCFQ, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'GPHASE: No freq random parameter found'
         CALL MSGWRT(6)
         ILOCFQ = -1
         IRET = 0
         END IF
      DOFQ = ILOCFQ.GT.0
C                                       Get pointer for integration time
      CALL UVDFND (UVIN, 1, 'INTTIM', ILOCIT, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'GPHASE: No inttim random parameter found'
         CALL MSGWRT(6)
         GO TO 990
         END IF
      CALL OCLOSE (UVIN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get length of each vis. record
      CALL OGET (UVIN, 'UV_DESC.LREC', TYPE, DIM, LREC, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get number of random params
      CALL OGET (UVIN, 'UV_DESC.NRPARM', TYPE, DIM, NRPARM, CDUM,
     *           IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get # correlations/vis record
      CALL OGET (UVIN, 'UV_DESC.NCORR', TYPE, DIM, NCOR, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Is vis. size adequate?
      IF (NCOR.GT.MXVS) THEN
         WRITE (MSGTXT, 1000) NCOR, MXVS
         CALL MSGWRT(6)
         IRET = 5
         GO TO 990
         END IF
C                                       Max averaging time (day)
      CALL OGET (UVIN, 'SOLINT', TYPE, DIM, SOLINT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
      SOLINT = SOLINT/86400.0
C                                       Get special antenna
      CALL OGET (UVIN, 'REFANT ', TYPE, DIM, SPCANT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get new antenna number
      CALL OGET (UVIN, 'NEWANT ', TYPE, DIM, NEWANT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Get Vis Norm Flag
      CALL OGET (UVIN, 'L1VIS ', TYPE, DIM, L1VIS, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Got all info needed from UVIN
C                                         leave open for later use
C
C                                       Clear accumulator counter
      CNT = 0
C                                       Open INPUT/OUTPUT files
      CALL OOPEN (UVIN, 'READ', IRET)
      IF (IRET.NE.0) GO TO 990
      CALL OOPEN (UVOUT, 'WRIT', IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Loop through input data
 100  CONTINUE
C                                       Get vis record
         CALL UVREAD (UVIN, RP, VS, IRET)
         IF (IRET.GT.0) GO TO 990

         IF (IRET.EQ.0) THEN
C                                       Acept iff one stat is refstat
            ISTATA = RP(ILOCB)/256.0 + 0.1
            ISTATB = RP(ILOCB) - ISTATA*256.0 + 0.1
            SENSE = 0
            IF (ISTATA.EQ.SPCANT) SENSE = 1
            IF (ISTATB.EQ.SPCANT) SENSE = -1
C                                       reject auto-corr baselines
            IF (ISTATA.EQ.ISTATB) SENSE = 0
            IF (SENSE.EQ.0) GO TO 100
C                                       IF data has been accumulated,
            IF (CNT.GT.0) THEN
C                                           reached end of timestamp?
               NEXTT = RP(ILOCT).GT.ENDTIM
C                                           reached new source?
               NEXTSU = DOSU
               IF (DOSU) NEXTSU = RP(ILOCSU).NE.RPS(ILOCSU)
C                                           reached new FQid?
               NEXTFQ = DOFQ
               IF (DOFQ) NEXTFQ = RP(ILOCFQ).NE.RPS(ILOCFQ)
               END IF
C                                       Set marker to write data.
            NEXT = (CNT.GT.0).AND.(NEXTT .OR. NEXTSU .OR. NEXTFQ)
            LAST = F
            END IF
         IF (IRET.LT.0) THEN
C                                       note that EOF was reached
            LAST = T
            IRET = 0
C                                       Set marker to write data.
            NEXT = CNT.GT.0
            END IF
C                                       If marker is set to write data
         IF (NEXT) THEN
            NEXT = F
C                                       U
            RPS(ILOCU) = RPS(ILOCU) / CNT
C                                       V
            RPS(ILOCV) = RPS(ILOCV) / CNT
C                                       W
            RPS(ILOCW) = RPS(ILOCW) / CNT
C                                       Time
            RPS(ILOCT) = RPS(ILOCT) / CNT
C                                       recode the baseline
            IF (NEWANT.LT.SPCANT) THEN
               RPS(ILOCB) = 256*NEWANT + SPCANT
            ELSE
               RPS(ILOCB) = NEWANT + 256*SPCANT
               END IF
C                                       preserve the orientation of the
C                                       baseline NEWANT - SPCANT
            IF (NEWANT.LT.SPCANT) THEN
               CNT = 1
            ELSE
               CNT = -1
               END IF
C                                       RPS(ILOCIT) should be updated!
C                                       Vis
            DO 150 LOOP = 1,NCOR
               IF (VSCNT(LOOP).GT.0) THEN
C                  VSS(1,LOOP) = VSS(1,LOOP) / VSCNT(LOOP)
C                  VSS(2,LOOP) = VSS(2,LOOP) / VSCNT(LOOP)
                  VSS(1,LOOP) =       VSS(1,LOOP) / VSS(3,LOOP)
                  VSS(2,LOOP) = CNT * VSS(2,LOOP) / VSS(3,LOOP)
                  END IF
 150           CONTINUE
            CNT = 0
            CALL UVWRIT (UVOUT, RPS, VSS, IRET)
            IF (IRET.GT.0) GO TO 990
            COUNT = COUNT + 1
            END IF
         IF (.NOT.LAST) THEN
C                                       If first vis for this avg
            IF (CNT.EQ.0) THEN
C                                       Fill random params
               CALL RCOPY (NRPARM, RP, RPS)
C                                       Set time for end of accumulation
               IF (SOLINT.GT.0.0) THEN
                  LOOP = INT(RP(ILOCT)/SOLINT)
                  ENDTIM = SOLINT*(LOOP+1)
               ELSE
                  ENDTIM = 0.0
                  END IF
               DO 170 LOOP = 1,NCOR
                  VSCNT(LOOP) = 0
                  VSS(1,LOOP) = 0.0
                  VSS(2,LOOP) = 0.0
                  VSS(3,LOOP) = 0.0
 170              CONTINUE
               RPS(ILOCU) = 0.0
               RPS(ILOCV) = 0.0
               RPS(ILOCW) = 0.0
               RPS(ILOCT) = 0.0
               END IF
C                                       Acumulate random parms
            RPS(ILOCU) = RPS(ILOCU) + RP(ILOCU)*SENSE
            RPS(ILOCV) = RPS(ILOCV) + RP(ILOCV)*SENSE
            RPS(ILOCW) = RPS(ILOCW) + RP(ILOCW)*SENSE
            RPS(ILOCT) = RPS(ILOCT) + RP(ILOCT)
            RPS(ILOCIT) = RPS(ILOCIT) + RP(ILOCIT)
C                                       Acumulate vis info
            DO 180 LOOP = 1,NCOR
               IF (VS(3,LOOP).GT.0.0) THEN
                  VSCNT(LOOP) = VSCNT(LOOP) + 1
                  IF (L1VIS) THEN
                     VSSAMP = VS(1,LOOP)**2 + VS(2,LOOP)**2
                     IF (VSSAMP.GT.0.0) VSSAMP = 1/SQRT(VSSAMP)
                  ELSE
                     VSSAMP = 1.0
                     END IF
                  VSS(1,LOOP) = VSS(1,LOOP) +
     *                  VSSAMP*VS(1,LOOP)*VS(3,LOOP)
                  VSS(2,LOOP) = VSS(2,LOOP) +
     *                  VSSAMP*VS(2,LOOP)*VS(3,LOOP)*SENSE
                  VSS(3,LOOP) = VSS(3,LOOP) + VS(3,LOOP)
                  END IF
 180           CONTINUE
C                                       Accumulate
            CNT = CNT + 1
            ENDIF
C                                       Loop for next vis
         IF (.NOT.LAST) GO TO 100
C                                       Done
C                                       Better be some data
      IF (COUNT.LE.0) THEN
         IRET = 7
         MSGTXT = 'GPHASE: error no data selected'
         CALL MSGWRT(6)
         GO TO 990
      ELSE
         WRITE (MSGTXT,1250) COUNT
         CALL MSGWRT (4)
         END IF
C                                       Set output sort order to be 'TB'
      CALL OGET (UVIN, 'UV_DESC.SORTORD', TYPE, DIM, IDUM, SORD, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL OPUT (UVOUT, 'UV_DESC.SORTORD', TYPE, DIM, IDUM, SORD,IRET)
      IF (IRET.NE.0) GO TO 990
      CALL OCLOSE (UVIN, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL OCLOSE (UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Done
      GO TO 999
C                                       Error handling stuff
 990  CONTINUE
      WRITE (MSGTXT, 1990) IRET
      CALL MSGWRT(6)
 999  RETURN
 1000 FORMAT ('GPHASE: too many correlations ', I6,' > ', I6)
 1250 FORMAT ('GPHASE: Wrote ',I7,' averaged visibilities')
 1990 FORMAT ('GPHASE: error ',I5)
      END
      SUBROUTINE GCPTAB (UVIN, UVOUT, IRET)
C-----------------------------------------------------------------------
C   GCPTB copies all tables from UVIN to UVOUT
C   Inputs:
C      UVIN    C*?   memory object describing input UV data object
C      UVOUT   C*?   memory object describing output UV data object
C   Output:
C       IRET   I     Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), UVOUT*(*)
      INTEGER   IRET
C                                       Std OOP declarations
      INTEGER TYPE, DIM(3), IDUM, METHOD
      CHARACTER CDUM*1
C
      INTEGER ANVER, ANROW, NUMORB, NOPCAL, ANFQID, NEWANT, LOOP, NOSTA,
     *     MNTSTA
      REAL POLRXY(2), UT1UTC, DATUTC, STAXOF, POLAA, POLCA(10), POLAB,
     *     POLCB(10)
      DOUBLE PRECISION ARRAYC(3), GSTIA0, DEGPDY, SAFREQ, STAXYZ(3),
     *     ORBPRM(6)
      CHARACTER ANTAB*36
      CHARACTER RDATE*8, TIMSYS*8, ANNAME*8, POLTYA*2, POLTYB*2
      CHARACTER DRPTBS(20)*2
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA DRPTBS /'CL', 'SN', 'NX', 'SU', 'FQ', 'FG', 'CH', 'BP', 'TY',
     *   'IM', 'AT', 'CT', 'MC', 'VT', 'PC', 'HF', 'GC', 'CQ', 'BL',
     *   '  ' /
      DATA ANTAB /'New AN Table'/
C-----------------------------------------------------------------------
      IRET = 0
      TYPE = OOACAR
      DIM(1) = 2
      DIM(2) = 20
      CALL OPUT (UVIN, 'DROPTABS', TYPE, DIM, IDUM, DRPTBS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy all tables except those
C                                       listed in DROPTABS
      CALL UVCALT (UVIN, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy FQ tables with selection
      CALL UVFCOP (UVIN, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL OGET (UVOUT, 'METHOD', TYPE, DIM, METHOD, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (METHOD.EQ.1) THEN
C                                       Cycle through antenna table and
C                                       add new antenna to end!
C                                       Get AN table number from UVINob
         CALL OGET (UVOUT, 'ANVER', TYPE, DIM, ANVER, CDUM, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       CREATE/FILL ANob
         CALL UV2TAB (UVOUT, ANTAB, 'AN', ANVER, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       OPEN/INIT ANob
         CALL OANINI (ANTAB, 'WRIT', ANROW, ARRAYC, GSTIA0, DEGPDY,
     *      SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANNAME,
     *      NUMORB, NOPCAL, ANFQID, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Get new antenna number
         CALL OGET (UVOUT, 'NEWANT ', TYPE, DIM, NEWANT, CDUM, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Check number of antennas
C                                       ANROW should reflect the
C                                       number of rows + 1.
         IF (NEWANT.NE.ANROW) THEN
            MSGTXT = 'GCPTB: #antennas <> #rows in AN table'
            CALL MSGWRT(6)
            MSGTXT = 'GCPTB:   adding new antenna to end of AN table'
            CALL MSGWRT(6)
            ENDIF
C                                       Get last full AN row
         LOOP = ANROW
 200     CONTINUE
         ANROW = ANROW - 1
         CALL OTABAN (ANTAB, 'READ', ANROW, ANNAME, STAXYZ, ORBPRM,
     *      NOSTA, MNTSTA, STAXOF, POLTYA, POLAA, POLCA, POLTYB,
     *      POLAB, POLCB, IRET)
         IF (IRET.EQ.-1) GO TO 200
         IF (IRET.NE.0) GO TO 990
C                                       Adjust for Phased antenna
         ANROW  = LOOP
         ANNAME = 'EQUIVANT'
         STAXYZ(1) = 0.0
         STAXYZ(2) = 0.0
         STAXYZ(3) = 0.0
C         ORBPRM(*) = ??
         NOSTA  = NEWANT
         MNTSTA = 0
         STAXOF = 0.0
         POLTYA = POLTYA
         POLAA  = POLAA
C         POLCA(*)  = ??
         POLTYB = POLTYB
         POLAB  = POLAB
C         POLCB(*)  = ??
C                                       Write new AN record
         CALL OTABAN (ANTAB, 'WRIT', ANROW, ANNAME, STAXYZ, ORBPRM,
     *      NOSTA, MNTSTA, STAXOF, POLTYA, POLAA, POLCA, POLTYB, POLAB,
     *      POLCB, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Close ANob
         CALL OTABAN (ANTAB, 'CLOS', ANROW, ANNAME, STAXYZ, ORBPRM,
     *      NOSTA, MNTSTA, STAXOF, POLTYA, POLAA, POLCA, POLTYB, POLAB,
     *      POLCB, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Done
         END IF
         GO TO 999
C                                       Error handling stuff
 990  CONTINUE
      WRITE (MSGTXT,1990) IRET
      CALL MSGWRT(6)
 999  RETURN
 1990 FORMAT ('GCPTAB: error ',I5)
      END
      SUBROUTINE GWRTHI (POPS, UVIN, UVOUT, IRET)
C-----------------------------------------------------------------------
C   GWRHI copies history file from UVINob to UVOUTob + description of
C      current activities
C   Inputs:
C      UVIN   C*?  Memory object describing input UV data
C      UVOUT  C*?  Memory object describing output UV data
C-----------------------------------------------------------------------
      CHARACTER POPS*(*), UVIN*(*), UVOUT*(*)
      INTEGER   IRET
C                                       OOP declarations
      INTEGER   NADV
      PARAMETER (NADV=4)
      CHARACTER LIST(NADV)*8
C                                       For I/O to user
      INCLUDE 'INCS:DMSG.INC'
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK'/
C-----------------------------------------------------------------------
      IRET = 0
C                                        Copy old history
      CALL OHCOPY (UVIN, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                        values.
      CALL OHLIST (POPS, LIST, NADV, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Done
      GO TO 999
C                                       Error handling stuff
 990  CONTINUE
      WRITE (MSGTXT,1990) IRET
      CALL MSGWRT(6)
 999  RETURN
 1990 FORMAT ('GWRTHI: error ',I5)
      END

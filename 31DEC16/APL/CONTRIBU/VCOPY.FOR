LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
      PARAMETER (NPARMS=25)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'VNPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
C                                       Uses PAOOF.INC
      DATA AVNAME /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
C                                       2
     *             'OUTNAME ', 'OUTCLASS', 'OUTSEQ  ', 'OUTDISK ',
C                                       6
     *             'SOURCES ', 'QUAL    ', 'CALCODE ', 'TIMERANG',
C                                       10
     *             'UVRANGE ', 'STOKES  ', 'BCHAN   ', 'ECHAN   ',
C                                       14
     *             'BIF     ', 'EIF     ',
C                                       18
     *                         'FREQID  ', 'SELBAND ', 'SELFREQ ',
C                                       19
     *             'ANTENNAS', 'SUBARRAY', 'FLAGVER ', 'UVCOPPRM'/
C                                       23
      DATA AVTYPE / OOACAR   ,  OOACAR   ,  OOAINT   ,  OOAINT   ,
C                                       2
     *              OOACAR   ,  OOACAR   ,  OOAINT   ,  OOAINT   ,
C                                       6
     *              OOACAR   ,  OOAINT   ,  OOACAR   ,  OOARE    ,
C                                       10
     *              OOARE    ,  OOACAR   ,  OOAINT   ,  OOAINT   ,
C                                       12
     *              OOAINT   ,  OOAINT   ,
C                                       16
     *                          OOAINT   ,  OOARE    ,  OOARE    ,
C                                       17
     *              OOAINT   ,  OOAINT   ,  OOAINT   ,  OOARE    /
C                                       21
      DATA AVDIM  / 12,1     ,  6,1      ,  1,1      ,  1,1      ,
C                                       2
     *              12,1     ,  6,1      ,  1,1      ,  1,1      ,
C                                       6
     *              16,30    ,  1,1      ,  4,1      ,  8,1      ,
C                                       10
     *              2,1      ,  4,1      ,  1,1      ,  1,1      ,
C                                       12
     *              1,1      ,  1,1      ,
C                                       16
     *                          1,1      ,  1,1      ,  1,1      ,
C                                       17
     *              50,1     ,  1,1      ,  1,1      ,  10,1     /
C                                       21
LOCAL END
      PROGRAM VCOPY
C-----------------------------------------------------------------------
C! Copy visibilities from one data set to another
C# UV-UTIL SPECTRAL CALIBRATION OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998
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
C   All available selection criteria are available to select data from
C   one data set to copy over to another data set.  This task is meant
C   to be more fully featured than UVCOP, but otherwise identical in
C   function.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, POPS*36, UVIN*36, UVOUT*36
      INTEGER  IRET
C
      PARAMETER (PRGM = 'VCOPY ')
      PARAMETER (POPS = 'Input ADVob')
      PARAMETER (UVIN = 'Input UVob')
      PARAMETER (UVOUT = 'Output UVob')
C
      INTEGER BUFF(256)
C                                       Are these includes NECESSARY?
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Get AIPS Adverb values
      CALL GETADV (PRGM, POPS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy UVIN to UVOUT
C                                         under POPS control
      CALL GUVIN (POPS, UVIN, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
      GO TO 999
C                                       Error handling stuff
 990  WRITE (MSGTXT,1990) IRET
      CALL MSGWRT(6)
C
 999  CALL DIE (IRET, BUFF)
      STOP
C-----------------------------------------------------------------------
 1990 FORMAT ('VCOPY: error ',I5)
      END
      SUBROUTINE GETADV (PRGM, POPS, IRET)
C-----------------------------------------------------------------------
C   GETADV gets adverb values from AIPS and fills them into POPSob
C   Inputs:
C      PRGM    C*?  program name
C   Output:
C      POPS    C*?  memory object describing AIPS adverb values
C      IRET    I    error code: 0 => ok
C-----------------------------------------------------------------------
C                                       I/O parameters
      INTEGER   IRET
      CHARACTER PRGM*(*), POPS*(*)
C                                       Std. OOP declarations
      INTEGER   DIM(7), TYPE, IDUM
      CHARACTER CDUM*1
      REAL      RDUM, UPARM(10)
      LOGICAL   LDUM, DOACOR, DOXCOR
C                                       Enumerate number of adverbs
      INCLUDE 'INPUT.INC'
C                                       Enumerate available data types
      INCLUDE 'INCS:PAOOF.INC'
C                                       AIPS errorlog facilities
      INCLUDE 'INCS:DMSG.INC'
C                                       AIPS baddisk mechanism
      INCLUDE 'INCS:DFIL.INC'
C                                       List adverb names
      INCLUDE 'VNPUTDATA.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Create/Fill adverbs into POPSob
      CALL AV2INP (PRGM, NPARMS, AVNAME, AVTYPE, AVDIM, POPS, IRET)
      IF (IRET.NE.0) GO TO 990
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
C                                       get XCOR and ACOR from UVCOPPRM
      CALL OGET (POPS, 'UVCOPPRM', TYPE, DIM, UPARM, CDUM, IRET)
      DOACOR = (UPARM(3).EQ.0) .OR. (UPARM(3).EQ.1)
      DOXCOR = (UPARM(3).EQ.0) .OR. (UPARM(3).EQ.2)
      CALL OGET (POPS, 'LOGICAL', TYPE, DIM, LDUM, CDUM, IRET)
      CALL OPUT (POPS, 'KEEPACOR', TYPE, DIM, DOACOR, CDUM, IRET)
      CALL OPUT (POPS, 'KEEPXCOR', TYPE, DIM, DOXCOR, CDUM, IRET)
C                                       Done
      GO TO 999
C                                       Error handling stuff
 990  CONTINUE
      WRITE (MSGTXT,1990) IRET
      CALL MSGWRT(6)
 999  RETURN
 1990 FORMAT ('GETADV: error ',I5)
      END
      SUBROUTINE GUVIN (POPS, UVIN, UVOUT, IRET)
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
      CHARACTER POPS*(*), UVIN*(*), UVOUT*(*)
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
      PARAMETER (NKEY3=4)
      CHARACTER IKEY3(NKEY3)*8, OKEY3(NKEY3)*16
C                                       OOP declarations
      INTEGER   NADV
      PARAMETER (NADV=4)
      CHARACTER LIST(NADV)*8
C
C                                       Std OOP declarations
      INTEGER   TYPE, DIM(7), IDUM
      CHARACTER CDUM*1
C                                       Internal variables
      LOGICAL T, F, ISCOMP
      INTEGER ABSEL, BSELET(50), ASELET(50), GCOUNT, SUBA, NSUB
      CHARACTER DRPTBS(20)*2
      CHARACTER ANTAB*36
C                                       For I/O to user
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C                                       for table transfer
      DATA DRPTBS /'AN', 'NX', 'FQ', 'HI', 'SU', 15*'  '/
C                                       Xfer keyword types
      DATA IKEY0 /'INTEGER ', 'LOGICAL ', 'REAL'/
      DATA OKEY0 /'INTEGER ', 'LOGICAL ', 'REAL'/
C                                       File specification info
      DATA IKEY1 /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  '/
      DATA OKEY1 /'FILE_NAME.NAME  ', 'FILE_NAME.CLASS ',
     *            'FILE_NAME.IMSEQ ', 'FILE_NAME.DISK  '/
C                                       Cal/Selection info
      DATA IKEY2 /'SOURCES ', 'QUAL    ', 'CALCODE ', 'TIMERANG',
     *            'UVRANGE ', 'STOKES  ', 'BCHAN   ', 'ECHAN   ',
     *            'BIF     ', 'EIF     ', 'KEEPACOR', 'KEEPXCOR',
     *                        'FREQID  ', 'SELBAND ', 'SELFREQ ',
     *            'ANTENNAS', 'SUBARRAY', 'FLAGVER '/
      DATA OKEY2 /'CALEDIT.SOURCS  ', 'CALEDIT.SELQUA  ',
     *            'CALEDIT.SELCOD  ', 'CALEDIT.TIMRNG  ',
     *            'CALEDIT.UVRNG   ', 'CALEDIT.STOKES  ',
     *            'CALEDIT.BCHAN   ', 'CALEDIT.ECHAN   ',
     *            'CALEDIT.BIF     ', 'CALEDIT.EIF     ',
     *            'CALEDIT.DOACOR  ', 'CALEDIT.DOXCOR  ',
     *                                'CALEDIT.FRQSEL  ',
     *            'CALEDIT.SELBAN  ', 'CALEDIT.SELFRQ  ',
     *            'CALEDIT.ANTENNS ', 'CALEDIT.SUBARR  ',
     *            'CALEDIT.FGVER   '/
C                                       Output file specification info
      DATA IKEY3 /'OUTNAME ', 'OUTCLASS', 'OUTSEQ  ', 'OUTDISK '/
      DATA OKEY3 /'OUTNAME ', 'OUTCLASS', 'OUTSEQ  ', 'OUTDISK '/
      DATA T, F /.TRUE.,.FALSE./
C                                       Adverbs to copy to history
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK'/
      DATA ANTAB /'temp AN table'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Create UVINob
      CALL CREATE (UVIN, 'UVDATA', IRET)
      IF (IRET.NE.0) GO TO 990
C                                       copy over fundamental data types
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
C                                       get UVINob info
      CALL OUVATT (UVIN, F, IRET)
C                                       specifically, get compression stae
      CALL OGET (UVIN, 'UV_DESC.ISCOMP', TYPE, DIM, ISCOMP, CDUM, IRET)
C                                       set state request for UVOUTob
      CALL OPUT (UVIN, 'DOUVCOMP', TYPE, DIM, ISCOMP, CDUM, IRET)
C                                       get subarray number
      CALL OGET (UVIN, 'CALEDIT.SUBARR  ',TYPE, DIM, SUBA, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       copy all subarrays
      IF (SUBA.EQ.0) THEN
C                                       get max subarray number
         CALL UV2TAB (UVIN, ANTAB, 'AN', 1, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL TBLHIV (ANTAB, NSUB, IRET)
         IF (IRET.NE.0) GO TO 990
         DO 100 SUBA = 1, NSUB
C                                       select subarray
            CALL OPUT (UVIN, 'CALEDIT.SUBARR  ',TYPE, DIM, SUBA,
     *         CDUM,IRET)
            IF (IRET.NE.0) GO TO 990
C                                       copy this subarray
            CALL UVCOPY (UVIN, UVOUT, IRET)
            IF (IRET.NE.0) GO TO 990
C                                       synchronize the file sizes
            CALL OOPEN (UVOUT, 'READ', IRET)
            IF (IRET.NE.0) GO TO 990
            CALL OCLOSE (UVOUT, IRET)
            IF (IRET.NE.0) GO TO 990
C                                       get current output data set size
            CALL OGET (UVOUT, 'UV_DESC.GCOUNT',TYPE, DIM, GCOUNT,
     *         CDUM,IRET)
            IF (IRET.NE.0) GO TO 990
C                                       put back in as offset size
            CALL OPUT (UVOUT, 'UV_DESC.VISOFF',TYPE, DIM, GCOUNT,
     *         CDUM,IRET)
            IF (IRET.NE.0) GO TO 990
 100        CONTINUE
      ELSE
C                                       copy over the requested subarray
         CALL UVCOPY (UVIN, UVOUT, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       Open UVOUTob
      CALL OOPEN (UVOUT, 'READ', IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close UVOUTob
      CALL OCLOSE (UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       get current output data set size
      CALL OGET (UVOUT, 'UV_DESC.GCOUNT',TYPE, DIM, GCOUNT, CDUM, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (GCOUNT.EQ.0) THEN
         IRET = 1
         GO TO 990
         END IF
C                                       Copy FQ tables with selection
      CALL UVFCOP (UVIN, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        Copy old history
      CALL OHCOPY (UVIN, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                        New additions - copy adverb
C                                        values.
      CALL OHLIST (POPS, LIST, NADV, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       prepare to copy tables
      TYPE = OOACAR
      DIM(1) = 2
      DIM(2) = 20
      CALL OPUT (UVIN, 'DROPTABS', TYPE, DIM, IDUM, DRPTBS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy all tables except those
C                                       listed in DROPTABS
      CALL UVCALT (UVIN, UVOUT, IRET)
      IF (IRET.NE.0) GO TO 990
C
C                                       Done
      GO TO 999
C                                       Error handling stuff
 990  CONTINUE
      WRITE (MSGTXT,1990) IRET
      CALL MSGWRT(6)
 999  RETURN
 1990 FORMAT ('GUVOUT: error ',I5)
      END

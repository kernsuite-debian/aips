      PROGRAM LDGPS
C-----------------------------------------------------------------------
C! Loads GPS delay data from a text file
C# Task Calibration VLA
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1999, 2009
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
C   LDGPS reads GPS delay data from a CONAN-ASCII file, corrects it
C   for receiver and satellite time offsets and writes it to a GP
C   table for use by calibration programs.
C-----------------------------------------------------------------------
C
C   Local variables
C     INPUTS    INPUTS object for access to adverbs
C     UVFILE    UVDATA object for access to uv file
C     GPSTAB    TABLE object for access to GP table
C     INFILE    name of input file
C     DIEBUF    scratch space for DIE subroutine
C     IRET      status indicator
C
      CHARACTER INPUTS*13
      PARAMETER (INPUTS = 'Inputs object')
      CHARACTER UVFILE*14
      PARAMETER (UVFILE = 'UV data object')
      CHARACTER GPSTAB*15
      PARAMETER (GPSTAB = 'GP table object')
      CHARACTER INFILE*48
      INTEGER   DIEBUF(256), IRET
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Read input adverbs
C
      CALL SETUP (INPUTS, UVFILE, GPSTAB, INFILE, IRET)
      IF (IRET .NE. 0) GO TO 990
C
C     Read the data from the text file.
C
      CALL RDFILE (INFILE, GPSTAB, UVFILE, IRET)
      IF (IRET .NE. 0) GO TO 990
C
C     Update the history file
C
      CALL LGPHIS (INPUTS, GPSTAB, UVFILE, IRET)
      IF (IRET .NE. 0) GO TO 990
C
  990 CALL DIE (IRET, DIEBUF)
      END
      SUBROUTINE SETUP (INPUTS, UVFILE, GPSTAB, INFILE, IRET)
C-----------------------------------------------------------------------
C   Read the input adverbs.
C
C   Inputs:
C      INPUTS    C*(*)    INPUTS object used to access adverbs
C      UVFILE    C*(*)    UVDATA object used to access uv data file
C      GPSTAB    C*(*)    TABLE object used to access GP table
C
C   Outputs:
C      INFILE    C*(*)    Name of input ffile
C      IRET      I        Status code:
C                           0: input adverbs read
C                           anything else: an error was detected
C
C   Preconditions:
C      INPUTS is not blank
C      UVFILE is not blank
C      GPSTAB is not blank
C
C   Postconditions
C      if IRET is 0 then:
C         INPUTS is initialized
C         UVFILE is initialized and corresponds to the INNAME, INCLASS,
C            INSEQ and INDISK adverbs
C         UVFILE is closed
C         GPSTAB is initialized and corresponds to UVFILE and the
C            OUTVERS adverb
C         GPSTAB is closed
C         INFILE has the value of the INFILE adverb
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), UVFILE*(*), GPSTAB*(*), INFILE*(*)
      INTEGER   IRET
C
C   Local variables
C
C      PRGN      Task name
C      NPARM     Number of input adverbs
C      AVNAME    Adverb names
C      AVTYPE    Adverb types
C      AVDIM     Adverb dimensions
C      OUTVER    Value of OUTVERS keyword
C      TYPE      AIPS object attribute type code
C      DIM       AIPS object attribute dimensions
C      NKEYS     Number of adverbs to copy to UVFILE
C      INKEY     Adverbs to copy to UVFILE
C      OUTKEY    UVFILE attributes to receive adverb values
C
      CHARACTER PRGN*6
      PARAMETER (PRGN = 'LDGPS ')
      INTEGER   NPARM
      PARAMETER (NPARM = 6)
      CHARACTER AVNAME(NPARM)*8
      INTEGER   AVTYPE(NPARM), AVDIM(2, NPARM)
      INTEGER   NKEYS
      PARAMETER (NKEYS = 4)
      CHARACTER INKEY(NKEYS)*8, OUTKEY(NKEYS)*16
      INTEGER   OUTVER, TYPE, DIM(3)
      CHARACTER CDUMMY
      INTEGER   NDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA AVNAME / 'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *              'INFILE  ', 'OUTVERS ' /
      DATA AVTYPE / OOACAR, OOACAR, OOAINT, OOAINT, OOACAR, OOAINT /
      DATA AVDIM / 12,1, 6,1, 1,1, 1,1, 48,1, 1,1 /
C
      DATA INKEY / 'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ' /
      DATA OUTKEY / 'FILE_NAME.NAME  ', 'FILE_NAME.CLASS ',
     *              'FILE_NAME.IMSEQ ', 'FILE_NAME.DISK  ' /
C-----------------------------------------------------------------------
      CALL AV2INP (PRGN, NPARM, AVNAME, AVTYPE, AVDIM, INPUTS, IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Initialize the UVDATA object:
C
      CALL OUVCRE (UVFILE, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL IN2OBJ (INPUTS, NKEYS, INKEY, OUTKEY, UVFILE, IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Initialize GPSTAB:
C
      CALL INGET (INPUTS, 'OUTVERS', TYPE, DIM, OUTVER, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL UV2TAB (UVFILE, GPSTAB, 'GP', OUTVER, IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Read input file name and check for null string:
C
      CALL INGET (INPUTS, 'INFILE', TYPE, DIM, NDUMMY, INFILE, IRET)
      IF (IRET .NE. 0) GO TO 999
      IF (INFILE .EQ. ' ') THEN
         MSGTXT = 'YOU MUST SPECIFY AN INPUT FILE (INFILE)'
         CALL MSGWRT (9)
         IRET = 1
         GO TO 999
      END IF
C
  999 RETURN
      END
      SUBROUTINE LGPHIS (INPUTS, GPSTAB, UVFILE, IRET)
C-----------------------------------------------------------------------
C   Update the history file for UVFILE.
C
C   Inputs:
C      INPUTS    C*(*)    INPUTS object for access to adverbs
C      GPSTAB    C*(*)    TABLE object for access to GP table
C      UVFILE    C*(*)    UVDATA object for access to UV file
C
C   Outputs:
C      IRET      I        Error code (0 => no error)
C-----------------------------------------------------------------------
      CHARACTER INPUTS*(*), GPSTAB*(*), UVFILE*(*)
      INTEGER   IRET
C
C   Local variables:
C
C      VERS      Table version number
C      TYPE      Attribute type code
C      DIM       Attribute dimensions
C      MSG       History file message buffer
C      NUMADV    Number of adverbs (parameter)
C      AVNAME    Adverb names
C
      INTEGER   VERS, TYPE, DIM(3), NUMADV
      PARAMETER (NUMADV = 6)
      CHARACTER MSG*72, AVNAME(NUMADV)*8, CDUMMY
C
      INCLUDE 'INCS:PAOOF.INC'
C
      DATA AVNAME / 'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *              'INFILE  ', 'OUTVERS ' /
C-----------------------------------------------------------------------
C
C     Open and close the uv file to establish the mapping between the
C     UVDATA object and the disk file
C
      CALL OUVOPN (UVFILE, 'READ', IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL OUVCLO (UVFILE, IRET)
      IF (IRET .NE. 0) GO TO 999
C
      CALL OHTIME (UVFILE, IRET)
      IF (IRET .NE. 0) GO TO 999
      CALL OHLIST (INPUTS, AVNAME, NUMADV, UVFILE, IRET)
      IF (IRET .NE. 0) GO TO 999
C
C     Write a comment giving the actual output table version number
C     if the default version was used
C
      CALL INGET (INPUTS, 'OUTVERS', TYPE, DIM, VERS, CDUMMY, IRET)
      IF (IRET .NE. 0) GO TO 999
      IF (VERS .LE. 0) THEN
         CALL TABGET (GPSTAB, 'VER', TYPE, DIM, VERS, CDUMMY, IRET)
         IF (IRET .NE. 0) GO TO 999
         WRITE (MSG, 1000) VERS
         CALL OHWRIT (MSG, UVFILE, IRET)
         IF (IRET .NE. 0) GO TO 999
      END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('/ Wrote GP table version ', I4)
      END
      SUBROUTINE RDFILE (INFILE, GPSTAB, UVFILE, IRET)
C-----------------------------------------------------------------------
C   Read GPS data from INFILE and write it to GPSTAB.
C
C   Inputs:
C      INFILE    C*(*)     Name of input file
C      GPSTAB    C*(*)     Name of TABLE object used to access the GP
C                            table
C      UVFILE    C*(*)     Name of UVDATA object used to access the
C                            parent data file of the GP table
C
C   Output:
C      IRET      I         Status code
C                             0: data written to table successfully
C                             anything else: an error was detected
C
C   Preconditions:
C      INFILE is not blank
C      GPSTAB is not blank
C      UVFILE is not blank
C      GPSTAB is not the same as UVFILE
C      The UVDATA object with name UVFILE is initialized
C      The TABLE object with name GPSTAB is initialized and closed.
C
C   Postconditions:
C      IRET = 0 implies that
C         Data in INFILE has been written to the table object with
C            name GPSTAB
C         The table object with name GPSTAB is closed.
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*), GPSTAB*(*), UVFILE*(*)
      INTEGER   IRET
C
C     Local variables:
C
C     TXTLUN    LUN for text file
C     TXTIND    FTAB index for text file
C
C     TXTEOF    Has the end of the text file been reached?
C
C     MAXPRN    Maximum satellite PRN
C     SATOFF    Satellite offsets in TECU indexed by PRN
C     OFFINI    Have satellite offsets been initialized?
C
C     IRET2     Temporary status code
C
      INTEGER   TXTLUN, TXTIND
      PARAMETER (TXTLUN = 10)
C
      LOGICAL   TXTEOF
C
      INTEGER   MAXPRN
      PARAMETER (MAXPRN = 32)
      REAL      SATOFF(MAXPRN)
      LOGICAL   OFFINI
C
      INTEGER   IRET2
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA SATOFF / MAXPRN * 0.0 /
C-----------------------------------------------------------------------
      CALL ZTXOPN ('READ', TXTLUN, TXTIND, INFILE, .FALSE., IRET)
      IF (IRET .EQ. 0) THEN
         TXTEOF = .FALSE.
         OFFINI = .FALSE.
C
C        Loop invariant 1: OFFINI implies that a satellite bias record
C                          occurred in the set of records already
C                          processed
C        Loop invariant 2: OFFINI implies that SATOFF contains the
C                          biases from the last satellite bias record
C                          before the current point in the input text
C                          file
C        Loop invariant 3: All data occurring in records before the
C                          current point in the input text file have
C                          been transferred to GPSTAB
C        Bound: The number of after the current point in the file
C
   10    IF ((IRET .EQ. 0) .AND. (.NOT. TXTEOF)) THEN
            CALL PRRECD (TXTLUN, TXTIND, GPSTAB, UVFILE, SATOFF,
     *                   OFFINI, TXTEOF, IRET)
            GO TO 10
         END IF
         CALL ZTXCLS (TXTLUN, TXTIND, IRET2)
         IF (IRET2 .NE. 0) THEN
            MSGTXT = 'FAILED TO CLOSE ' // INFILE
            CALL MSGWRT (8)
            IRET = 1
         END IF
      ELSE
         MSGTXT = 'FAILED TO OPEN ' // INFILE
         CALL MSGWRT (8)
         IRET = 1
      END IF
      END
      SUBROUTINE PRRECD (TXTLUN, TXTIND, GPSTAB, UVFILE, SATOFF, OFFINI,
     *                   TXTEOF, IRET)
C-----------------------------------------------------------------------
C   Process the next record from the text file open for reading on
C   TXTLUN with FTAB index TXTIND.
C
C   Inputs:
C      TXTLUN   I        LUN of text file
C      TXTIND   I        FTAB index of text file
C      GPSTAB   C*(*)    Name of TABLE object used to access GP table
C      UVFILE   C*(*)    Name of UVDATA object used to access data file
C
C   Input/ouput:
C      SATOFF   R(*)     List of satellite biases in TEC units indexed
C                          by PRN
C      OFFINI   L        Has SATOFF been initialized?
C
C   Output:
C      TXTEOF   L        Has the text file been exhausted?
C      IRET     I        Status code:
C                          0 - record processed or no records left
C                          1 - syntax error in input file
C                          2 - receiver changed in input file
C                          3 - data encountered without satellite biases
C                          4 - error reading input file
C                          5 - error writing table
C
C   Preconditions:
C      TXTLUN and TXTIND refer to an open text file
C      GPSTAB is not blank and refers to a TABLE object
C      UVFILE is not blank and refers to a UVDATA object
C      The UVDATA object with name UVFILE encapsulates the parent data
C        file of the table encapsulated by GPSTAB.
C      The TABLE object with name GPSTAB is closed.
C
C   Postconditions:
C      If there were no records left to read in the input file then
C         IRET = 0 and TXTEOF is true
C      If the next record was a satellite offset record then IRET = 0
C         implies that SATOFF has been updated and OFFINI is true
C      If the next record was a data header than IRET = 0 implies that
C         the data following the header have been transferred to GPSTAB
C      GPSTAB is closed.
C-----------------------------------------------------------------------
      INTEGER   TXTLUN, TXTIND
      CHARACTER GPSTAB*(*), UVFILE*(*)
      REAL      SATOFF(*)
      LOGICAL   OFFINI, TXTEOF
      INTEGER   IRET
C
C     Local variables:
C
C     MAXPRN    Maximum GPS PRN
C
C     NUMVAL    Maximum number of values that can be returned by KEYIN
C     KEYS      Keyword list for KEYIN
C     VALUES    Numeric values from KEYIN
C     VALCHR    Character values from KEYIN
C     NPARM     Number of KEYIN parameters
C     ENDMRK    End-of-record marker for KEYIN
C
      INTEGER   MAXPRN
      PARAMETER (MAXPRN = 32)
C
      INTEGER   NUMVAL
      PARAMETER (NUMVAL = 9 + MAXPRN - 1)
      CHARACTER KEYS(NUMVAL)*8
      DOUBLE PRECISION VALUES(NUMVAL)
      CHARACTER VALCHR(NUMVAL)*8
      INTEGER   NPARM
      CHARACTER ENDMRK*8
      PARAMETER (ENDMRK = '/        ')
C
      INCLUDE 'INCS:DMSG.INC'
C
      DATA KEYS / 'GPS     ', 'GPSDATA ', 'TECU    ', 'NANOSEC ',
     *            'RECEIVER', 'X       ', 'Y       ', 'Z       ',
     *            MAXPRN * 'BIAS    ' /
C-----------------------------------------------------------------------
C
C     Initialize VALUES to a non-zero value since KEYIN will set the
C     entries for keywords that it finds that do not have explicit
C     values to zero:
C
      CALL DFILL (NUMVAL, -9999.9D0, VALUES)
C
C     Set receiver name to blank so we can tell if has been set by
C     KEYIN:
C
      VALCHR(5) = '        '
C
      NPARM = NUMVAL
      CALL KEYIN (KEYS, VALUES, VALCHR, NPARM, ENDMRK, 0, TXTLUN,
     *            TXTIND, IRET)
      IF (IRET .EQ. 0) THEN
         IF ((VALUES(1) .NE. -9999.9D0)
     *       .AND. (VALUES(2) .EQ. -9999.9D0))  THEN
C
C           The record was a satellite bias record so extract the
C           biases:
C
            CALL PRBIAS (VALUES, SATOFF, OFFINI, IRET)
C
C           If IRET is non-zero then it already has the correct
C           value.
C
         ELSE IF ((VALUES(1) .EQ. -9999.9D0)
     *            .AND. (VALUES(2) .NE. -9999.9D0)) THEN
C
C           The record was a data header so process the data block that
C           follows it:
C
            IF (OFFINI) THEN
               CALL PRDATA (TXTLUN, TXTIND, VALUES, VALCHR, GPSTAB,
     *                      UVFILE, SATOFF, IRET)
C
C              Error codes 0, 4, and 5 are returned unchanged.
C
               IF ((IRET .EQ. 1) .OR. (IRET .EQ. 2)) THEN
                  IRET = 1
               ELSE IF (IRET .EQ. 3) THEN
                  IRET = 2
               END IF
            ELSE
               MSGTXT = 'DATA MUST BE PRECEDED BY A SATELLITE BIAS '
     *                  // 'RECORD'
               CALL MSGWRT (8)
               IRET = 3
            END IF
         ELSE IF ((VALUES(1) .EQ. -9999.9D0)
     *            .AND. (VALUES(2) .EQ. -9999.9D0)) THEN
            MSGTXT = 'INVALID RECORD FOUND: NEITHER GPS NOR GPSDATA'
            CALL MSGWRT (8)
            IRET = 1
         ELSE
            MSGTXT = 'INVALID RECORD FOUND: BOTH GPS AND GPSDATA'
            CALL MSGWRT (8)
            IRET = 1
         END IF
      ELSE IF (IRET .EQ. 1) THEN
C
C        Reached the end of the text file. Note this and clear status:
C
         TXTEOF = .TRUE.
         IRET = 0
      ELSE
         WRITE (MSGTXT, 9000) IRET
         CALL MSGWRT (8)
         IRET = 4
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('ERROR ', I6, ' READING INPUT FILE')
      END
      SUBROUTINE PRBIAS (VALUES, SATOFF, OFFINI, IRET)
C-----------------------------------------------------------------------
C   Process a satellite bias record.
C
C   Input:
C      VALUES   D(*)     Values from KEYIN
C
C   Output:
C      SATOFF   R(*)     Satellite biases in TEC units, indexed by PRN
C      OFFINI   L        Has SATOFF been initialized?
C      IRET     I        Status code:
C                           0 - biases updated
C                           1 - invalid record
C
C   Preconditions:
C      VALUES has been set by KEYIN from a satellite bias record
C
C   Postconditions:
C      IRET = 0 implies that OFFINI is true and SATOFF has been updated
C-----------------------------------------------------------------------
      DOUBLE PRECISION VALUES(*)
      REAL      SATOFF(*)
      LOGICAL   OFFINI
      INTEGER   IRET
C
C     Local variables
C
C     SCALE     Nanoseconds of delay between L1 and L2 for 1 TECU
C     FACTOR    Factor to scale values to TECU
C
C     MAXPRN    Maximum satellite PRN
C     BIAS      Index of first bias in VALUES
C     TECU      Index of TECU in VALUES
C     NANOS     Index of NANOSEC in VALUES
C     SET       Have any bias values been set?
C     I         Loop counter
C
      REAL      SCALE
      PARAMETER (SCALE = 2.854)
      REAL      FACTOR
C
      INTEGER   MAXPRN, BIAS, TECU, NANOS
      PARAMETER (MAXPRN = 32)
      PARAMETER (BIAS = 9)
      PARAMETER (TECU = 3)
      PARAMETER (NANOS = 4)
      LOGICAL   SET
      INTEGER   I
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      SET = .TRUE.
C
C     Loop invariant: SET is equivalent to VALUES(BIAS + j) not being
C                     equal to -9999.9D0 (the marker for missing values)
C                     for all j, 0 <= j < I
C
      DO 10 I = 0, MAXPRN - 1
         IF (VALUES(BIAS + I) .EQ. -9999.9D0) THEN
            SET = .FALSE.
         END IF
   10 CONTINUE
C
C     SET implies that there is a new set of biases.
C
      IF (SET) THEN
C
C        Find the units:
C
         IF ((VALUES(TECU) .EQ. -9999.9D0)
     *       .OR. (VALUES(NANOS) .EQ. -9999.9D0)) THEN
            IF (VALUES(TECU) .EQ. -9999.9D0) THEN
               FACTOR = SCALE
            ELSE
               FACTOR = 1.0
            END IF
C
C           Copy the bias values to SATOFF:
C
            DO 20 I = 0, MAXPRN - 1
               SATOFF(I + 1) = FACTOR * VALUES(BIAS + I)
   20       CONTINUE
            OFFINI = .TRUE.
         ELSE
            MSGTXT = 'INVALID BIAS RECORD FOUND: AMBIGUOUS UNITS'
            CALL MSGWRT (8)
            IRET = 1
         END IF
      ELSE
         MSGTXT = 'INVALID BIAS RECORD FOUND: SOME BIASES MISSING'
         CALL MSGWRT (8)
         IRET = 1
      END IF
C
      END
      SUBROUTINE PRDATA (TXTLUN, TXTIND, VALUES, VALCHR, GPSTAB, UVFILE,
     *                   SATOFF, IRET)
C-----------------------------------------------------------------------
C   Process a data block.
C
C   Inputs:
C      TXTLUN   I      LUN of text file
C      TXTIND   I      FTAB index of text file
C      VALUES   D(*)   Values array from KEYIN
C      VALCHR   C(*)*8 Character value array from KEYIN
C      GPSTAB   C*(*)  Name of TABLE object used to access GP table
C      UVFILE   C*(*)  Name of UVDATA object used to access parent
C                       file of GPSTAB
C      SATOFF   R(*)   Satellite biases in TEC units, indexed by PRN
C
C   Output:
C      IRET     I      Status code:
C                        0 - data transferred to table
C                        1 - data incomplete
C                        2 - data block too large
C                        3 - receiver changed
C                        4 - error reading text file
C                        5 - error writing table
C
C   Preconditions:
C      TXTLUN and TXTIND refer to an open text file positioned after
C         a data header record
C      VALUES has been set by KEYIN from the data header record
C         immediately preceding the current position of the text file
C      GPSTAB is not blank and refers to a TABLE object
C      GPSTAB is closed
C      UVFILE is not blank and refers to a UVDATA object associated
C         with the parent file of GPSDATA
C
C   Postconditions:
C      IRET = 0 implies that the data following the header has been
C         transferred to GPSTAB and the text file is positioned at
C         the next record
C      GPSTAB is closed
C-----------------------------------------------------------------------
      INTEGER   TXTLUN, TXTIND
      DOUBLE PRECISION VALUES(*)
      CHARACTER VALCHR(*)*8, GPSTAB*(*), UVFILE*(*)
      REAL      SATOFF(*)
      INTEGER   IRET
C
C     Local variables:
C
C     IRCVR     Index of receiver name in VALCHR
C     IX        Index of X coordinate in VALUES
C     IY        Index of Y coordinate in VALUES
C     IZ        Index of Z coordinate in VALUES
C     IBIAS     Index of bias in VALUES
C     INANOS    Index of NANOSEC keyword in VALUES
C     ITECU     Index of TECU keyword in VALUES
C
C     SCALE     Nanoseconds of delay between L1 and L2 for 1 TECU
C     FACTOR    Factor to scale values to TECU
C
C     BIAS      Receiver bias in TEC units
C
C     LAT       Receiver latitude in degrees
C     LON       Receiver longitude in degrees
C     HT        Receiver height in metres
C     R         Distance of antenna from coordinate origin
C     RE        Mean radius of the Earth in metres
C
C     TRCVR     Receiver name in table
C     TLON      Receiver longitude in table in degrees
C     TLAT      Reciever latitude in table in degrees
C     THT       Receiver height in table
C     GPROW     New row to read or write in table
C
C     MAXENT    Maximum number of entries in data block
C     ENTRY     Entries in data block
C     NVAL      Number of values in data block
C     ENDMRK    End marker for data block
C     KEYS      Dummy keyword list
C
C     POSERR    Maximum error in latitude or longitude in degrees
C     HTERR     Maximum error in height in metres
C
C     IRET2     Temporary return status
C
      INTEGER   IRCVR, IX, IY, IZ, IBIAS, INANOS, ITECU
      PARAMETER (IRCVR  = 5)
      PARAMETER (IX     = 6)
      PARAMETER (IY     = 7)
      PARAMETER (IZ     = 8)
      PARAMETER (IBIAS  = 9)
      PARAMETER (INANOS = 4)
      PARAMETER (ITECU  = 3)
C
      REAL      SCALE
      PARAMETER (SCALE = 2.854)
      REAL      FACTOR
C
      REAL      BIAS
C
      DOUBLE PRECISION LAT, LON, HT, R, RE
      PARAMETER (RE = 6371000.0)
C
      CHARACTER TRCVR*8
      REAL      TLAT, TLON, THT
      INTEGER   GPROW
C
      INTEGER   MAXENT
      PARAMETER (MAXENT = 36000)
      DOUBLE PRECISION ENTRY(8 * MAXENT)
      INTEGER   NVAL
      CHARACTER ENDMRK*8, KEYS(1)*8
      PARAMETER (ENDMRK = '/       ')
C
      REAL      POSERR, HTERR
C                                       Allowable position change is 5
C                                       seconds in lat or long
      PARAMETER (POSERR = 5.0 / 3600.0)
C                                       Allowable height change is 10
C                                       metres
      PARAMETER (HTERR = 10.0)
C
      INTEGER   IRET2
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Process data block only if all required values are present in the
C     header:
C
      IF ((VALCHR(IRCVR) .NE. '        ')
     *    .AND. (VALUES(IX) .NE. -9999.9D0)
     *    .AND. (VALUES(IY) .NE. -9999.9D0)
     *    .AND. (VALUES(IZ) .NE. -9999.9D0)
     *    .AND. (VALUES(IBIAS) .NE. -9999.9D0)
     *    .AND. ((VALUES(INANOS) .EQ. -9999.9D0)
     *           .OR. (VALUES(ITECU) .EQ. -9999.9D0))) THEN
C
C        Find bias units:
C
         IF (VALUES(ITECU) .EQ. -9999.9D0) THEN
            FACTOR = SCALE
         ELSE
            FACTOR = 1.0
         END IF
C
         BIAS = FACTOR * VALUES(IBIAS)
C
C        Convert geocentric coordinates to latitude, longitude, and
C        height (it should be enough to assume a spherical Earth):
C
         R = SQRT (VALUES(IX) ** 2 + VALUES(IY) ** 2 + VALUES(IZ) ** 2)
         HT = R - RE
         LAT = RAD2DG * ASIN (VALUES(IZ) / R)
         IF (VALUES(IX) .NE. 0.0D0) THEN
            LON = RAD2DG * ATAN2 (VALUES(IY), VALUES(IX))
         ELSE IF (VALUES(IY) .GT. 0.0D0) THEN
            LON = +90.0D0
         ELSE
            LON = -90.0D0
         END IF
C
C        Copy receiver information so that the values returned by OGPINI
C        can be compared with those read from the file:
C
         TRCVR = VALCHR(IRCVR)
         TLAT = LAT
         TLON = LON
         THT = HT
C
         CALL OGPINI (GPSTAB, 'WRIT', GPROW, TRCVR, TLON, TLAT, THT,
     *                IRET)
         IF (IRET .EQ. 0) THEN
            IF ((TRCVR .EQ. VALCHR(IRCVR))
     *          .AND. (ABS (TLON - LON) .LE. POSERR)
     *          .AND. (ABS (TLAT - LAT) .LE. POSERR)
     *          .AND. (ABS (THT - HT) .LE. HTERR)) THEN
C
C              Reciever has not changed so read the data:
C
               NVAL = 8 * MAXENT
               CALL KEYIN (KEYS, ENTRY, VALCHR, NVAL, ENDMRK, 3, TXTLUN,
     *                     TXTIND, IRET)
               IF (IRET .EQ. 0) THEN
                  IF ((NVAL .LE. 8 * MAXENT)
     *                .AND. (MOD (NVAL, 8) .EQ. 0)) THEN
                     CALL CPYDAT (ENTRY, NVAL / 8, BIAS, SATOFF,
     *                            UVFILE, GPSTAB, GPROW, IRET)
                     IF (IRET .NE. 0) THEN
                        MSGTXT = 'FAILED TO UPDATE OUTPUT TABLE'
                        CALL MSGWRT (8)
                        IRET = 5
                     END IF
                  ELSE IF (NVAL .GT. 8 * MAXENT) THEN
                     WRITE (MSGTXT, 9000) MAXENT
                     CALL MSGWRT (8)
                     IRET = 2
                  ELSE
                     MSGTXT = 'DATA BLOCK CONTAINS AN INCOMPLETE ENTRY'
                     CALL MSGWRT (8)
                     IRET = 1
                  END IF
               ELSE IF (IRET .EQ. 1) THEN
                  MSGTXT = 'DATA BLOCK MISSING AT END OF FILE'
                  CALL MSGWRT (8)
                  IRET = 1
               ELSE
                  WRITE (MSGTXT, 9001) IRET
                  CALL MSGWRT (8)
                  IRET = 4
               END IF
            ELSE
               MSGTXT = 'CAN NOT HANDLE MORE THAN ONE RECEIVER'
               CALL MSGWRT (8)
               WRITE (MSGTXT, 9002) 'RECEIVER WAS', TRCVR, TLAT, TLON,
     *                              THT
               CALL MSGWRT (8)
               WRITE (MSGTXT, 9002) 'RECEIVER NOW', VALCHR(IRCVR), LAT,
     *                              LON, HT
               CALL MSGWRT (8)
               IRET = 3
            END IF
            CALL TABCLO (GPSTAB, IRET2)
            IF (IRET2 .NE. 0) THEN
               MSGTXT = 'FAILED TO CLOSE OUTPUT TABLE'
               CALL MSGWRT (8)
               IRET = 5
            END IF
         ELSE
            MSGTXT = 'FAILED TO OPEN OUTPUT TABLE FOR WRITING'
            CALL MSGWRT (8)
            IRET = 5
         END IF
      ELSE IF (VALCHR(IRCVR) .EQ. '        ') THEN
         MSGTXT = 'INVALID DATA HEADER: RECEIVER NAME MISSING'
         CALL MSGWRT (8)
         IRET = 1
      ELSE IF (VALUES(IX) .EQ. -9999.9D0) THEN
         MSGTXT = 'INVALID DATA HEADER: X COORDINATE MISSING'
         CALL MSGWRT (8)
         IRET = 1
      ELSE IF (VALUES(IY) .EQ. -9999.9D0) THEN
         MSGTXT = 'INVALID DATA HEADER: Y COORDINATE MISSING'
         CALL MSGWRT (8)
         IRET = 1
      ELSE IF (VALUES(IZ) .EQ. -9999.9D0) THEN
         MSGTXT = 'INVALID DATA HEADER: Z COORDINATE MISSING'
         CALL MSGWRT (8)
         IRET = 1
      ELSE IF (VALUES(IBIAS) .EQ. -9999.9D0) THEN
         MSGTXT = 'INVALID DATA HEADER: RECEIVER BIAS MISSING'
         CALL MSGWRT (8)
         IRET = 1
      ELSE
         MSGTXT = 'INVALID DATA HEADER: AMBIGUOUS BIAS UNITS'
         CALL MSGWRT (8)
         IRET = 1
      END IF
C-----------------------------------------------------------------------
 9000 FORMAT ('DATA BLOCK TOO LONG: LIMIT ', I7, ' ENTRIES')
 9001 FORMAT ('ERROR ', I3, ' READING FROM TEXT FILE')
 9002 FORMAT (A12, ': ', A8, ' LT = ', F7.2, ' LN = ', F7.2, ' HT = ',
     *        F7.1)
      END
      SUBROUTINE CPYDAT (VALUES, NENTRY, BIAS, SATOFF, UVFILE, GPSTAB,
     *                   GPROW, IRET)
C-----------------------------------------------------------------------
C   Translate data entries in VALUES and add them to GPSTAB starting at
C   row GPROW.
C
C   Inputs:
C      VALUES  D(8, *)   Data entries -- first index is
C                         1  year number
C                         2  day of year
C                         3  time of observation (UTC hour)
C                         4  satellite PRN
C                         5  satellite azimuth in degrees
C                         6  satellite elevation in degrees
C                         7  TEC from phase difference in TEC units
C                         8  TEC from delay difference in TEC units
C      NENTRY  I         Number of data entries
C      BIAS    R         Receiver bias in TEC units
C      SATOFF  R(*)      List of satellite offsets in TEC units indexed
C                         by PRN
C      UVFILE  C*(*)     Name of UVDATA object used to access data file
C      GPSTAB  C*(*)     Name of TABLE object used to access output
C                         table
C
C   Input/Output:
C      GPROW   I         Next row to write in table
C
C   Output:
C      IRET    I         Status code:
C                          0 - data written to table
C                          1 - error writing to table
C                          2 - other error
C
C   Preconditions:
C      UVFILE is not blank and is the name of a UVDATA object
C      GPSTAB is not blank and is the name of a TABLE object
C      GPSTAB is open for writing
C      GPSTAB is attached to UVFILE
C      GPROW > 0
C      NENTRY >= 0
C      VALUES has been populated from a GPS data block by KEYIN
C
C   Postconditions:
C      The data have been written to GPSTAB
C-----------------------------------------------------------------------
      DOUBLE PRECISION VALUES(8, *)
      INTEGER   NENTRY
      REAL      BIAS, SATOFF(*)
      CHARACTER UVFILE*(*), GPSTAB*(*)
      INTEGER   GPROW, ITEMP, IRET
C
C     Local variables:
C
C     RTIME     Julian date of file reference time
C     ETIME     Julian date of measurement
C
C     ANTAB     TABLE object used to access AN table
C     ANVER     Antenna table version
C     ANROW     Next antenna table row to read (ignored)
C     ARRAYC    Coordinates of array centre (ignored)
C     GSTIA0    GST at 00:00:00 IAT on reference date (ignored)
C     DEGPDY    Rotational rate of the Earth (ignored)
C     SAFREQ    Subarray frequency offset (ignored)
C     OBSDAT    Observing date
C     POLRXY    Coordinates of North pole (ignored)
C     UT1UTC    UT1 - UTC (ignored)
C     DATUTC    Data time - UTC in seconds
C     TIMSYS    Time system (ignored)
C     ANAME     Array name (ignored)
C     NUMORB    Number of orbital parameters (ignored)
C     NOPCAL    Number of polarization parameters (ignored)
C     ANFQID    Frequency ID (ignored)
C
C     ENTRY     Number of entries processed so far
C     IT        Broken down time (year, month, day, hour, min, sec)
C     PRN       Current PRN
C
C     HRSPDY    Number of UTC hours in a day
C     SECPDY    Number of UTC seconds in a day
C
      DOUBLE PRECISION RTIME, ETIME
C
      CHARACTER ANTAB*15
      PARAMETER (ANTAB = 'AN table object')
      INTEGER   ANVER, ANROW
      DOUBLE PRECISION ARRAYC(3), GSTIA0, DEGPDY, SAFREQ
      CHARACTER OBSDAT*8
      REAL      POLRXY(2), UT1UTC, DATUTC
      CHARACTER TIMSYS*8, ANAME*8
      INTEGER   NUMORB, NOPCAL, ANFQID
C
      INTEGER   ENTRY, IT(6), PRN
C
      REAL      HRSPDY, SECPDY
      PARAMETER (HRSPDY = 24.0)
      PARAMETER (SECPDY = HRSPDY * 60.0 * 60.0)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C
C     Extract reference time from antenna table 1
C
      ANVER = 1
      CALL UV2TAB (UVFILE, ANTAB, 'AN', ANVER, IRET)
      IF (IRET .EQ. 0) THEN
         CALL OANINI (ANTAB, 'READ', ANROW, ARRAYC, GSTIA0, DEGPDY,
     *      SAFREQ, OBSDAT, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *      NUMORB, NOPCAL, ITEMP, ANFQID, IRET)
         IF (IRET .EQ. 0) THEN
            CALL TABCLO (ANTAB, IRET)
            IF (IRET .EQ. 0) THEN
               CALL TABDES (ANTAB, IRET)
               IF (IRET .EQ. 0) THEN
                  CALL JULDAY (OBSDAT, RTIME)
C
C                 Shift to UTC:
C
                  RTIME = RTIME - DATUTC / SECPDY
                  ENTRY = 0
C
C                 Transfer entries to output table:
C
C                 Loop invariant: IRET == 0 implies that the first ENTRY
C                                 entries in values have been copied to
C                                 GPSTAB
C                 Bound: NENTRY - ENTRY
C
   10             IF ((IRET .EQ. 0) .AND. (ENTRY .NE. NENTRY)) THEN
                     ENTRY = ENTRY + 1
C
C                    Convert time to Julian date to find offset from
C                    reference time:
C
                     CALL FILL (6, 0, IT)
                     IT(1) = NINT (VALUES(1, ENTRY))
                     IT(2) = 1
                     CALL DAT2JD (IT, ETIME)
                     ETIME = ETIME + VALUES(2, ENTRY)
     *                       + VALUES(3, ENTRY) / HRSPDY
C
                     PRN = NINT (VALUES(4, ENTRY))
                     CALL OTABGP (GPSTAB, 'WRIT', GPROW, ETIME - RTIME,
     *                            PRN,
     *                            REAL (VALUES(5, ENTRY)),
     *                            REAL (VALUES(6, ENTRY)),
     *                            1.0E16 * REAL (VALUES(8, ENTRY)
     *                               - (BIAS + SATOFF(PRN))),
     *                            1.0E16 * REAL (VALUES(7, ENTRY))
     *                               - (BIAS + SATOFF(PRN)), IRET)
                     IF (IRET .NE. 0) THEN
                        WRITE (MSGTXT, 9010) IRET
                        CALL MSGWRT (8)
                        IRET = 1
                     END IF
                     GO TO 10
                  END IF
               ELSE
                  MSGTXT = 'FAILED TO RECYCLE TEMPORARY TABLE OBJECT'
                  CALL MSGWRT (8)
                  IRET = 2
               END IF
            ELSE
               MSGTXT = 'FAILED TO CLOSE ANTENNA TABLE'
               CALL MSGWRT (8)
               IRET = 2
            END IF
         ELSE
            MSGTXT = 'FAILED TO READ REFERENCE DATE FROM AN TABLE 1'
            CALL MSGWRT (8)
            IRET = 2
         END IF
      ELSE
         MSGTXT = 'FAILED TO CREATE TEMPORARY TABLE OBJECT'
         CALL MSGWRT (8)
         IRET = 2
      END IF
C-----------------------------------------------------------------------
 9010 FORMAT ('ERROR ', I3, ' WRITING OUPUT TABLE')
      END

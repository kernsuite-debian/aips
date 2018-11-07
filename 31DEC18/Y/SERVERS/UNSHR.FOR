      PROGRAM UNSHR
C-----------------------------------------------------------------------
C! unpack AIPS shell archives on non-Unix systems
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C   This program is used to unpack AIPS shell archives on systems that
C   do not have a Bourne shell (or a compatible shell, such as the
C   KornShell) available.  It is a standalone program written in ANSI
C   standard FORTRAN 77.  See the file README.1ST in this directory for
C   instructions on how to compile and run this program.
C-----------------------------------------------------------------------
C   Constants:
C     MXFNAM   I     Maximum length of a file name
C     INFILE   I     Unit number for input archive
C     OUTFIL   I     Unit number for output archive

      INTEGER INFILE, MXFNAM, OUTFIL
      PARAMETER (MXFNAM = 80)
      PARAMETER (INFILE = 20)
      PARAMETER (OUTFIL = 21)

C   Local variables:
C    ARNAME    C*(MXFNAM)      Archive name.
C    DONE      L               Set to true when there are no unpacked
C                              files left in the archive, false otherwise.
C    ERROR     L               Error flag: true if an error has
C                              occurred.

      CHARACTER*(MXFNAM) ARNAME
      LOGICAL            DONE, ERROR

C   External functions:

      LOGICAL  UNPACK
      EXTERNAL UNPACK

      DATA DONE  /.FALSE./
      DATA ERROR /.FALSE./
C-----------------------------------------------------------------------
C     Obtain archive name from user:
         CALL GETARC (ARNAME, ERROR)

C     Open archive and verify format:
         IF (.NOT.ERROR) THEN
            CALL OPNARC (ARNAME, INFILE, ERROR)
            END IF

C     Read files from archive:
 10      IF ((.NOT.ERROR) .AND. (.NOT.DONE)) THEN
            DONE = .NOT.UNPACK (INFILE, OUTFIL)
            GO TO 10
            END IF

      END

      SUBROUTINE GETARC (ARCNAM, ERROR)
C-----------------------------------------------------------------------
C   Prompt the user for the name of the archive and read it from the
C   standard input.  Error is assumed to be set to false when this
C   subroutine is called.
C
C   Outputs:
C     ARCNAM      C*(*)       Name of archive
C     ERROR       L           True if an error occurred, false otherwise
C-----------------------------------------------------------------------
      CHARACTER*(*) ARCNAM
      LOGICAL       ERROR

C   Local variables
C     STATUS      I          Status return from I/O operations

      INTEGER STATUS
C-----------------------------------------------------------------------
C     Prompt user for archive name:
         WRITE (*, *, IOSTAT=STATUS) ' Enter the name of the archive:'
         ERROR = STATUS.NE.0
C                                       If we can't talk to the standard
C                                       output unit then there is no
C                                       point in trying to output an
C                                       error message.

C     Read archive name:
         IF (.NOT.ERROR) THEN
            READ (*, '(A)', IOSTAT=STATUS) ARCNAM
            IF (STATUS.NE.0) THEN
               ERROR = .TRUE.
               WRITE (*, '(''Error '', I6, '' reading archive name'')')
     *            STATUS
               END IF
            END IF

      END

      SUBROUTINE OPNARC (ARNAME, INFILE, ERROR)
C-----------------------------------------------------------------------
C   Open the archive file with name ARNAME on unit INFILE and verify
C   that it is an AIPS shell archive.
C
C   Inputs:
C     ARNAME   C*(*)    Archive name
C     INFILE   I        FORTRAN unit for archive
C
C   Output:
C     ERROR    L        Set to true if an error occurred; assumed to be
C                       set to false on subroutine call
C-----------------------------------------------------------------------
      CHARACTER*(*) ARNAME
      INTEGER       INFILE
      LOGICAL       ERROR

C   Constants:
C     MAXLIN    I    Maximum length of line in archive

      INTEGER MAXLIN
      PARAMETER (MAXLIN = 255)

C   Local variables:
C     ARXIST    L           True if the named file exists
C     ARCHIV    L           True if the input file is an archive
C     LINE      C*(MAXLIN)  Line buffer
C     STATUS    I           Status code from I/O operations

      LOGICAL            ARXIST, ARCHIV
      CHARACTER*(MAXLIN) LINE
      INTEGER            STATUS
C-----------------------------------------------------------------------
C     Test named file for existence:
         INQUIRE (FILE=ARNAME, EXIST=ARXIST, IOSTAT=STATUS)
         IF (STATUS.NE.0) THEN
            ERROR = .TRUE.
            WRITE (*, '('' Error '', I6, '' inquiring about archive'')')
     *         STATUS
         ELSE IF (.NOT.ARXIST) THEN
            ERROR = .TRUE.
            WRITE (*, '('' Named archive does not exist'')')
            END IF

      IF (.NOT.ERROR) THEN

C        Open named file:
            OPEN (INFILE, FILE=ARNAME, STATUS='OLD', FORM='FORMATTED',
     *         IOSTAT=STATUS)
            IF (STATUS.NE.0) THEN
               ERROR = .TRUE.
               WRITE (*, '('' Error '', I6, '' opening archive'')')
     *            STATUS
               END IF

         END IF

      IF (.NOT.ERROR) THEN

C        Check header:
            READ (INFILE, '(A)', IOSTAT=STATUS) LINE
            ERROR = STATUS .NE. 0
            IF (.NOT.ERROR) ARCHIV = LINE .EQ. '#! /bin/sh'
            IF (.NOT.ERROR) THEN
               READ (INFILE, '(A)', IOSTAT=STATUS) LINE
               ERROR = STATUS .NE. 0
               END IF
            IF (.NOT.ERROR) ARCHIV = ARCHIV .AND.
     *                               (LINE(1:8) .EQ. '#-------')
            IF (.NOT.ERROR) THEN
               READ (INFILE, '(A)', IOSTAT=STATUS) LINE
               ERROR = STATUS .NE. 0
               END IF
            IF (.NOT.ERROR) ARCHIV = ARCHIV .AND.
     *         (LINE. EQ. '# This is an AIPS self-unpacking ' //
     *                    'shell archive')
            IF (STATUS.NE.0) THEN
               WRITE (*, '('' Error '', I6, '' reading header'')')
     *            STATUS
            ELSE IF (.NOT.ARCHIV) THEN
               ERROR = .TRUE.
               WRITE (*, '('' File is not an AIPS shell archive'')')
               END IF

         END IF

      END

      LOGICAL FUNCTION UNPACK (INFILE, OUTFIL)
C-----------------------------------------------------------------------
C   Extract a single file from the archive on unit INFILE.  Returns
C   true if a file was extracted or false if no file was found to
C   extract or an error occurred.
C
C   Inputs:
C     INFILE    I     unit number of archive (already open)
C     OUTFIL    I     unit number to use for output file
C
C   Return value
C     UNPACK    L     true if a file was successfully extracted,
C                     otherwise false
C-----------------------------------------------------------------------
      INTEGER INFILE, OUTFIL

C   Constants:
C     MAXLIN    I     Maximum line length
C     MAXFNM    I     Maximum length of a file name

      INTEGER MAXLIN, MAXFNM
      PARAMETER (MAXLIN = 255)
      PARAMETER (MAXFNM = 80)

C   Local variables:
C     LINE    C*(MAXLIN)   Line buffer
C     OUTFNM  C*(MAXFNM)   output file name
C     DONE    L            true if no file to extract
C     STATUS  I            status from I/O operations
C     ERROR   L            true if an error has been detected otherwise
C                          false

      CHARACTER*(MAXLIN) LINE
      CHARACTER*(MAXFNM) OUTFNM
      LOGICAL            DONE
      INTEGER            STATUS
      LOGICAL            ERROR

      INTEGER I

C   External functions:
C     STRLEN    I    return the length of a character string without
C                    trailing blanks

      INTEGER  STRLEN
      EXTERNAL STRLEN

      DATA LINE  /'No data read in yet'/
      DATA DONE  /.FALSE./
      DATA ERROR /.FALSE./
C-----------------------------------------------------------------------
C     Skip records until next file header:
 10      IF ((.NOT.ERROR) .AND. (.NOT. DONE) .AND.
     *       (LINE(1:3) .NE. 'cat')) THEN
            READ (INFILE, '(A)', IOSTAT=STATUS) LINE
            IF (STATUS.LT.0) THEN
               DONE = .TRUE.
            ELSE IF (STATUS.NE.0) THEN
               ERROR = .TRUE.
               WRITE (*, '('' Error '', I6, '' reading archive '')')
     *            STATUS
               END IF
            GO TO 10
            END IF
 
      IF ((.NOT.DONE) .AND. (.NOT.ERROR)) THEN

C        Extract file name:
            READ (LINE(7:MAXLIN), '(A)') OUTFNM
            I = INDEX (OUTFNM, ' ')
            OUTFNM = OUTFNM(1:I-1)

C        Open output file:
            OPEN (OUTFIL, FILE=OUTFNM, STATUS='NEW', FORM='FORMATTED',
     *         IOSTAT=STATUS)
            IF (STATUS.NE.0) THEN
               ERROR = .TRUE.
               WRITE (*, '('' Error '', I6, '' opening output file '',
     *            A)') STATUS, OUTFNM(1:STRLEN(OUTFNM))
               END IF

         END IF
 
      IF ((.NOT.DONE) .AND. (.NOT.ERROR)) THEN

C        Copy archived file to output:
            READ (INFILE, '(A)', IOSTAT=STATUS) LINE
            IF (STATUS.NE.0) THEN
               ERROR = .TRUE.
               WRITE (*, '('' Error '', I6, '' reading archive'')')
     *            STATUS
               END IF
 20         IF ((.NOT.ERROR) .AND. (LINE.NE.'--XYZZY--')) THEN
               WRITE (OUTFIL, '(A)', IOSTAT=STATUS) LINE(1:STRLEN(LINE))
               IF (STATUS.NE.0) THEN
                  ERROR = .TRUE.
                  WRITE (*, '('' Error '', I6, '' writing '',  A)')
     *               STATUS, OUTFNM(1:STRLEN(OUTFNM))
                  END IF
               IF (.NOT.ERROR) THEN
                  READ (INFILE, '(A)', IOSTAT=STATUS) LINE
                  IF (STATUS.NE.0) THEN
                     ERROR = .TRUE.
                     WRITE (*,
     *                      '('' Error '', I6, '' reading archive'')')
     *                      STATUS
                     END IF
                  END IF
               GO TO 20
               END IF

         END IF

      UNPACK = .NOT.(ERROR .OR. DONE)

      END

      INTEGER FUNCTION STRLEN (STRING)
C-----------------------------------------------------------------------
C   Return the length of string STRING without trailing blanks.
C
C   Input:
C     STRING    C*(*)    input string
C
C   Return value:
C     STRLEN    I        length of string
C-----------------------------------------------------------------------
      CHARACTER*(*) STRING

      INTEGER I
C-----------------------------------------------------------------------
      STRLEN = 0
      DO 10 I = LEN (STRING), 1, -1
         IF (STRING(I:I) .NE. ' ') THEN
            STRLEN = I
            GO TO 20
            END IF
 10      CONTINUE

 20   CONTINUE
      END

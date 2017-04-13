LOCAL INCLUDE 'XLATE.INC'
C                                       Local include for XLATE
C                                       /CHRCOM/ variable declarations
C                                       and definition.
      CHARACTER NAMIN*128, NAMLIS*10, NAMOUT*128, NAMLOG*128,
     *   NAMERR*128, NAMND*128, NAMMOD*16, PGNAME*10, PGTYPE*4,
     *   CXUNIT*8, LININ*80, LINOUT*80
      COMMON /CHRCOM/ NAMIN, NAMLIS, NAMOUT, NAMLOG, NAMERR, NAMND,
     *   NAMMOD, PGNAME, PGTYPE, CXUNIT, LININ, LINOUT
C                                       /OTHCOM/ variable declarations
C                                       and definition.
      INTEGER   LUNIN, LUNLIS, LUNOUT, LUNLOG, LUNERR, LUNND
      LOGICAL   DEBUG, VERBO
      COMMON /OTHCOM/ LUNIN, LUNLIS, LUNOUT, LUNLOG, LUNERR, LUNND,
     *   DEBUG, VERBO
LOCAL END
      PROGRAM XLATE
C-----------------------------------------------------------------------
C! stand-alone, list driven AIPS source code translation program
C# Service
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
C   XLATE should be portable between operating systems given the
C   following requirements:
C
C   1) must be run from the directory in which the files to be processed
C      reside (the list of files to drive the program must also reside
C      there and must be called XLATE.LIS)
C
C   2) under VMS, the OPEN statement for the translated (i.e., output)
C      source code file should use the non-ANSI standard parameter
C      and keyword CARRIAGECONTROL='LIST' which is desirable (if
C      not absolutely required).
C
C   3) under VMS, the OPEN statement for the input files (i.e., file
C      name list and input source code) should use the non-ANSI standard
C      parameter READONLY which is desirable (if not absolutely
C      required).
C
C   It produces three (3) listings:
C
C      XLATE.LOG which contains all translations performed on the code
C                displaying the untranslated line followed by the
C                translated line
C
C      XLATE.ND  which contains all lines of code involving questionable
C                ENCODE statement translations and all DECODE statement
C                translations which may required further attention to
C                complete the conversion to their equivalent internal
C                WRITEs or READs
C
C      XLATE.ERR all cases of code that may require manual conversion
C                (i.e., suspected pseudo I*4 code) or that has been
C                detected or is suspected as non-ANSI standard (e.g.,
C                non-standard intrinsic functions)
C-----------------------------------------------------------------------
      INCLUDE 'XLATE.INC'
      INTEGER   IDEBUG, IVERBO, IOSVAL, IEXT, I, J, K, LNB, VERIFY
      LOGICAL   EXISTS
      CHARACTER LINTMP*80
C-----------------------------------------------------------------------
C                                       Initialize COMMON variables.
C
C                                       Current XLATE program unit.
      CXUNIT = 'XLATE'
C                                       Debug and/or verbose modes?
      DEBUG = .FALSE.
      VERBO = .FALSE.
      WRITE (6,*) 'ENTER MODES (DEBUG,VERBOSE) (I1,I1):'
      READ (5,'(I1,I1)') IDEBUG, IVERBO
      IF (IDEBUG.EQ.1) DEBUG = .TRUE.
      IF (IVERBO.EQ.1) VERBO = .TRUE.
C                                       Logical unit number for input
C                                       source code module (file names
C                                       read from list of source code
C                                       modules).
      LUNIN = 10
C                                       Logical unit number for list of
C                                       source code modules.
      LUNLIS = 11
C                                       File name containing list of
C                                       source code modules.
      NAMLIS = 'XLATE.LIS'
C                                       Logical unit number for output
C                                       source code module (file name
C                                       formed from input file name,
C                                       i.e., same but with extension
C                                       '.XXX').
      LUNOUT = 12
C                                       Logical unit number for master
C                                       log file.
      LUNLOG = 13
C                                       File name of master log file.
      NAMLOG = 'XLATE.LOG'
C                                       Logical unit number for error
C                                       file.
      LUNERR = 14
C                                       File name of error file.
      NAMERR = 'XLATE.ERR'
C                                       Logical unit number for special
C                                       ENCODE/DECODE log file.
      LUNND = 15
C                                       File name of special
C                                       ENCODE/DECODE log file.
      NAMND = 'XLATE.ND'
C                                       Open list of source code
C                                       modules.
      INQUIRE (FILE=NAMLIS, EXIST=EXISTS)
C                                       Does it exist?
      IF (.NOT.EXISTS) THEN
         WRITE (6,*) 'FILE ',NAMLIS,' NOT FOUND'
         GO TO 999
      ELSE
         OPEN (UNIT=LUNLIS, FILE=NAMLIS, STATUS='OLD', IOSTAT=IOSVAL)
         IF (IOSVAL.NE.0) THEN
            WRITE (6,*) 'FORTRAN OPEN ERROR = ',IOSVAL,
     *         ' ON FILE LIST = ',NAMLIS
            GO TO 999
            END IF
         END IF
C                                       Rewind file list.
      REWIND (LUNLIS)
C                                       Open master log file.
      INQUIRE (FILE=NAMLOG, EXIST=EXISTS)
C                                       Does it exist?
      IF (EXISTS) THEN
         WRITE (6,*) 'FILE ',NAMLOG,' ALREADY EXISTS'
         GO TO 999
      ELSE
         OPEN (UNIT=LUNLOG, FILE=NAMLOG, STATUS='NEW', IOSTAT=IOSVAL)
         IF (IOSVAL.NE.0) THEN
            WRITE (6,*) 'FORTRAN OPEN ERROR = ',IOSVAL,
     *         ' ON MASTER LOG FILE = ',NAMLOG
            GO TO 999
            END IF
         END IF
C                                       Rewind master log file.
      REWIND (LUNLOG)
C                                       Open error file.
      INQUIRE (FILE=NAMERR, EXIST=EXISTS)
C                                       Does it exist?
      IF (EXISTS) THEN
         WRITE (6,*) 'FILE ',NAMERR,' ALREADY EXISTS'
         GO TO 999
      ELSE
         OPEN (UNIT=LUNERR, FILE=NAMERR, STATUS='NEW', IOSTAT=IOSVAL)
         IF (IOSVAL.NE.0) THEN
            WRITE (6,*) 'FORTRAN OPEN ERROR = ',IOSVAL,
     *         ' ON ERROR FILE = ',NAMERR
            GO TO 999
            END IF
         END IF
C                                       Rewind error list.
      REWIND (LUNERR)
C                                       Open special ENCODE/DECODE log
C                                       file.
      INQUIRE (FILE=NAMND, EXIST=EXISTS)
C                                       Does it exist?
      IF (EXISTS) THEN
         WRITE (6,*) 'FILE ',NAMND,' ALREADY EXISTS'
         GO TO 999
      ELSE
         OPEN (UNIT=LUNND, FILE=NAMND, STATUS='NEW', IOSTAT=IOSVAL)
         IF (IOSVAL.NE.0) THEN
            WRITE (6,*) 'FORTRAN OPEN ERROR = ',IOSVAL,
     *         ' ON SPECIAL ENCODE/DECODE LOG FILE = ',NAMND
            GO TO 999
            END IF
         END IF
C                                       Rewind ENCODE/DECODE log file.
      REWIND (LUNND)
C                                       Process modules one at a time.
 10   READ (LUNLIS,'(A)',IOSTAT=IOSVAL) NAMIN
C                                       READ error?
      IF (IOSVAL.EQ.0) GO TO 20
C                                       EOF?
         IF (IOSVAL.LT.0) GO TO 900
C                                       Other error.
            WRITE (6,*) 'FORTRAN READ ERROR = ',IOSVAL,' ON FILE = ',
     *         NAMLIS
            GO TO 900
C                                       Find '.FOR' or '.INC' file name
C                                       extension.
 20   IEXT = INDEX (NAMIN, '.FOR')
      IF (IEXT.GT.0) GO TO 30
C                                       Try '.INC'.
         IEXT = INDEX (NAMIN, '.INC')
         IF (IEXT.GT.0) THEN
            PGTYPE = 'INCL'
            GO TO 30
            END IF
C                                       Valid extension not found.
C                                       Skip it.
         WRITE (6,*) 'SKIPPING FILE NAME = ',NAMIN(1:LNB (NAMIN))
C                                       Get another input source code
C                                       module file name.
         GO TO 10
C                                       OPEN input source code module.
 30   OPEN (UNIT=LUNIN, FILE=NAMIN, STATUS='OLD', IOSTAT=IOSVAL)
C                                       OPEN error?
      IF (IOSVAL.NE.0) THEN
         WRITE (6,*) 'FORTRAN OPEN ERROR = ',IOSVAL
         WRITE (6,*) 'ON INPUT FILE = ',NAMIN(1:LNB (NAMIN))
         GO TO 900
         END IF
C                                       Rewind input source code module.
      REWIND (LUNIN)
      WRITE (6,*) ' PROCESSING ', NAMIN(1:LNB (NAMIN))
C                                       Extract simple file name for
C                                       use in listings.
C      J = LNB (NAMIN)
C      I = IEXT - 1
C 31   I = I - 1
C      IF (I.EQ.0) GO TO 32
C      IF (VERIFY (NAMIN(I:I),
C         'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789').EQ.0) GO TO 31
C 32   IF (I.EQ.0) I = 1
C      NAMMOD = NAMIN(I:J)
      NAMMOD = NAMIN
C                                       Form output source code module
C                                       name from input name.
      NAMOUT = NAMIN(1:IEXT) // 'XXX'
C                                       OPEN output source code module.
      OPEN (UNIT=LUNOUT, FILE=NAMOUT, STATUS='UNKNOWN',
     *   IOSTAT=IOSVAL)
C                                       VMS peculiarity.
C     *   CARRIAGECONTROL='LIST', IOSTAT=IOSVAL)
C                                       OPEN error?
      IF (IOSVAL.NE.0) THEN
         WRITE (6,*) 'FORTRAN OPEN ERROR = ',IOSVAL
         WRITE (6,*) 'ON OUTPUT FILE = ',NAMOUT(1:LNB (NAMOUT))
         GO TO 900
         END IF
C                                       Rewind output source code
C                                       module.
      REWIND (LUNOUT)
C                                       Process one line at a time.
 40   READ (LUNIN,'(A)',IOSTAT=IOSVAL) LININ
C                                       READ error?
      IF (IOSVAL.EQ.0) GO TO 50
C                                       EOF?
         IF (IOSVAL.LT.0) GO TO 800
C                                       Other error.
            WRITE (6,*) 'FORTRAN READ ERROR = ',IOSVAL
            WRITE (6,*) ' ON FILE = ',NAMIN(1:LNB (NAMIN))
            GO TO 800
C                                       Process line.
 50   LINOUT = LININ
C                                       Check for begining of a new
C                                       program unit.
      IF ((INDEX (LINOUT, '      PROGRAM ').GT.0) .OR.
     *   (INDEX (LINOUT, '      SUBROUTINE ').GT.0) .OR.
     *   (INDEX (LINOUT, ' FUNCTION ').GT.0)) THEN
         CALL PGUNIT
         GO TO 90
         END IF
C                                       Comment?
      IF (LINOUT(1:1).EQ.'C') THEN
         CALL COMENT
         GO TO 90
         END IF
C                                       Check INCLUDE statements for
C                                       possible elmination.
      I = LNB (LINOUT)
      IF ((INDEX (LINOUT, '      INCLUDE ').GT.0) .AND.
     *   (LINOUT(I:I).EQ.'''')) THEN
         CALL INCLUD
         GO TO 90
         END IF
C                                       TYPE*N type declaration?
      IF ((INDEX (LINOUT, '      INTEGER*').GT.0) .OR.
     *   (INDEX (LINOUT, '      REAL*').GT.0) .OR.
     *   (INDEX (LINOUT, '      LOGICAL*').GT.0) .OR.
     *   (INDEX (LINOUT, '      BYTE ').GT.0)) THEN
         CALL DECLAR
         END IF
C                                       If this program unit is the
C                                       main program, check for and
C                                       eliminate any declaration of
C                                       the array FTAB, which is now
C                                       declared and dimensioned in
C                                       DDCH.INC.
      IF ((PGTYPE.EQ.'PROG') .AND. (INDEX (LINOUT, 'FTAB(').GT.0))
     *   CALL DEFTAB
C                                       ENCODE or DECODE statement?
      IF ((INDEX (LINOUT, ' ENCODE').GT.0) .OR.
     *   (INDEX (LINOUT, ' DECODE').GT.0)) THEN
         CALL NDCODE
         GO TO 90
         END IF
C                                       NWDPLI used in call to COPY?
      IF ((INDEX (LINOUT, 'NWDPLI').GT.0) .AND.
     *   (INDEX (LINOUT, 'CALL COPY').GT.0)) THEN
         CALL NWDPLI
         GO TO 90
         END IF
C                                       Check for use of non-generic
C                                       intrinsic functions (expensive).
      IF ((INDEX (LINOUT, 'ABS').GT.0) .OR.
     *   (INDEX (LINOUT, 'COS').GT.0) .OR.
     *   (INDEX (LINOUT, 'EXP').GT.0) .OR.
     *   (INDEX (LINOUT, 'FLOAT').GT.0) .OR.
     *   (INDEX (LINOUT, 'INT').GT.0) .OR.
     *   (INDEX (LINOUT, 'FIX').GT.0) .OR.
     *   (INDEX (LINOUT, 'LOG').GT.0) .OR.
     *   (INDEX (LINOUT, 'MAX').GT.0) .OR.
     *   (INDEX (LINOUT, 'MIN').GT.0) .OR.
     *   (INDEX (LINOUT, 'MOD').GT.0) .OR.
     *   (INDEX (LINOUT, 'SIGN').GT.0) .OR.
     *   (INDEX (LINOUT, 'SIN').GT.0) .OR.
     *   (INDEX (LINOUT, 'SQRT').GT.0) .OR.
     *   (INDEX (LINOUT, 'TAN').GT.0)) CALL NONGEN
C                                       Check for CALLs for calls
C                                       to pseudo I*4 routines.
      IF (INDEX (LINOUT, ' CALL ').GT.0) CALL P4CALL
C                                       Check for other things.
      CALL OTHER
C                                       Reset XLATE program unit name.
 90   CXUNIT = 'XLATE'
C                                       Get output line length (i.e.,
C                                       position of last non-blank).
      I = LNB (LINOUT)
C                                       If not totally blank, write the
C                                       processed line to the output
C                                       output source code module.
      IF (LNB (LINOUT).GT.0) WRITE (LUNOUT,'(A)') LINOUT(1:LNB (LINOUT))
C                                       Report long lines (i.e., > 72).
      IF (LNB (LINOUT).GT.72) THEN
         IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         ENDIF
C                                       Get another line.
      GO TO 40
C                                       Close the input and output
C                                       source module files.
 800  CLOSE (LUNIN)
      CLOSE (LUNOUT)
C                                       Get another input file.
      GO TO 10
C                                       Close the module list,
C                                       log, error and special
C                                       ENCODE/DECODE files.
 900  CLOSE (LUNLIS)
      CLOSE (LUNLOG)
      CLOSE (LUNERR)
      CLOSE (LUNND)
C                                       Exit.
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A,/)
      END
      SUBROUTINE PGUNIT
C-----------------------------------------------------------------------
C   Try to detect the beginning of a new program unit (i.e., PROGRAM,
C   SUBROUTINE or FUNCTION).
C
C   Inputs:
C      none
C   Output:
C      none
C-----------------------------------------------------------------------
      INTEGER   I, J, K, FNB, LNB, VERIFY
      CHARACTER LINSAV*80, LINTMP*80
      INCLUDE 'XLATE.INC'
C-----------------------------------------------------------------------
C                                       Set XLATE program unit name.
      CXUNIT = 'PGUNIT'
C                                       Save copy of LINOUT for logging
C                                       changes.
      LINSAV = LINOUT
C                                       Debug print.
      IF (DEBUG) WRITE (6,*) 'PGUNIT: ',LINOUT(1:LNB (LINOUT))
C                                       Check for '      PROGRAM ...'.
      I = INDEX (LINOUT, '      PROGRAM ')
      IF (I.GT.0) THEN
         J = I + LEN ('      PROGRAM ')
         DO 10 I = 1,LEN (LINOUT)
            IF (LINOUT(J:J).NE.' ') GO TO 20
            J = J + 1
 10         CONTINUE
 20      LINTMP = LINOUT(J:)
         J = INDEX (LINTMP, ' ')
         PGNAME = LINTMP(1:J)
         PGTYPE = 'PROG'
      ELSE
C                                       Check for
C                                       '      SUBROUTINE ...'.
         I = INDEX (LINOUT, '      SUBROUTINE ')
         IF (I.GT.0) THEN
            J = I + LEN ('      SUBROUTINE ')
            DO 30 I = 1,LEN (LINOUT)
               IF (LINOUT(J:J).NE.' ') GO TO 40
               J = J + 1
 30            CONTINUE
 40         LINTMP = LINOUT(J:)
            J = INDEX (LINTMP, '(') - 1
            IF (J.LE.0) J = INDEX (LINTMP, ' ')
            PGNAME = LINTMP(1:J)
            PGTYPE = 'SUBR'
         ELSE
C                                       Check for ' FUNCTION ...'.
            I = INDEX (LINOUT, ' FUNCTION ')
            IF (I.GT.0) THEN
               J = I + LEN (' FUNCTION ')
               DO 50 I = 1,LEN (LINOUT)
                  IF (LINOUT(J:J).NE.' ') GO TO 60
                  J = J + 1
 50               CONTINUE
 60            LINTMP = LINOUT(J:)
               J = INDEX (LINTMP, '(') - 1
               IF (J.LE.0) J = INDEX (LINTMP, ' ')
               PGNAME = LINTMP(1:J)
               PGTYPE = 'FUNC'
               END IF
            END IF
         END IF
C                                       Debug print.
      IF (DEBUG) WRITE (6,*) 'PGUNIT: PGTYPE = ',PGTYPE,
     *   ' PGNAME = ',PGNAME
C                                       Exit.
 999  RETURN
      END
      SUBROUTINE COMENT
C-----------------------------------------------------------------------
C   1) make sure all comments of the form 'C------...' consist of 'C'
C      followed by 71 '-'s
C
C   2) convert comments containing:
C
C      'I*2... ' and 'I*4... ' to 'I...   '
C      'R*4... '               to 'R...   '
C      'R*8... '               to 'D...   '
C      'L*2... ' and 'L*4... ' to 'L...   '
C
C   3) report comments containing 'L*1', 'P*4' or I*2(2) (i.e., possible
C      P*4) in the error file
C
C   Inputs:
C      none
C   Output:
C      none
C-----------------------------------------------------------------------
      INTEGER   I, J, K, FNB, LNB, VERIFY
      CHARACTER LINSAV*80, LINTMP*80
      INCLUDE 'XLATE.INC'
C-----------------------------------------------------------------------
C                                       Set XLATE program unit name.
      CXUNIT = 'COMENT'
C                                       Save copy of LINOUT for logging
C                                       changes.
      LINSAV = LINOUT
C                                       Debug print.
      IF (DEBUG) WRITE (6,*) 'COMENT: ',LINOUT(1:LNB (LINOUT))
C                                       Clean up 'C-----...' comments.
      IF (LINOUT(1:8).EQ.'C-------') THEN
C                  0        1         2         3
C                  123456789012345678901234567890123456
         LINOUT = 'C-----------------------------------' //
     *            '------------------------------------'
         GO TO 999
         END IF
C                                       Convert 'I*2' to 'I  '.
 10   I = INDEX (LINOUT, 'I*2')
      IF (I.GT.0) THEN
         J = I + INDEX (LINOUT(I:), ' ') - 1
         LINTMP = LINOUT(1:I-1) // 'I' // LINOUT(I+3:J) // ' ' //
     *      LINOUT(J:)
         LINOUT = LINTMP
         GO TO 10
         END IF
C                                       Convert 'I*4' to 'I  '.
 20   I = INDEX (LINOUT, 'I*4')
      IF (I.GT.0) THEN
         J = I + INDEX (LINOUT(I:), ' ') - 1
         LINTMP = LINOUT(1:I-1) // 'I' // LINOUT(I+3:J) // ' ' //
     *      LINOUT(J:)
         LINOUT = LINTMP
         GO TO 20
         END IF
C                                       Convert 'R*4' to 'R  '.
 30   I = INDEX (LINOUT, 'R*4')
      IF (I.GT.0) THEN
         J = I + INDEX (LINOUT(I:), ' ') - 1
         LINTMP = LINOUT(1:I-1) // 'R' // LINOUT(I+3:J) // ' ' //
     *      LINOUT(J:)
         LINOUT = LINTMP
         GO TO 30
         END IF
C                                       Convert 'R*8' to 'D  '.
 40   I = INDEX (LINOUT, 'R*8')
      IF (I.GT.0) THEN
         J = I + INDEX (LINOUT(I:), ' ') - 1
         LINTMP = LINOUT(1:I-1) // 'D' // LINOUT(I+3:J) // ' ' //
     *      LINOUT(J:)
         LINOUT = LINTMP
         GO TO 40
         END IF
C                                       Convert 'L*2' to 'L  '.
 50   I = INDEX (LINOUT, 'L*2')
      IF (I.GT.0) THEN
         J = I + INDEX (LINOUT(I:), ' ') - 1
         LINTMP = LINOUT(1:I-1) // 'L' // LINOUT(I+3:J) // ' ' //
     *      LINOUT(J:)
         LINOUT = LINTMP
         GO TO 50
         END IF
C                                       Convert 'L*4' to 'L  '.
 60   I = INDEX (LINOUT, 'L*2')
      IF (I.GT.0) THEN
         J = I + INDEX (LINOUT(I:), ' ') - 1
         LINTMP = LINOUT(1:I-1) // 'L' // LINOUT(I+3:J) // ' ' //
     *      LINOUT(J:)
         LINOUT = LINTMP
         GO TO 60
         END IF
C                                       Report 'P*4', 'I(2)' and 'L*1'.
      IF ((INDEX (LINOUT, 'P*4').GT.0) .OR.
     *   (INDEX (LINOUT, 'I(2)').GT.0) .OR.
     *   (INDEX (LINOUT, 'L*1').GT.0)) THEN
C                                       Report comment in error file.
         IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         ENDIF
C                                       Any changes?
      IF (LINOUT.NE.LINSAV) THEN
C                                       Debug print.
         IF (DEBUG) THEN
            WRITE (6,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *         LINSAV(1:LNB (LINSAV))
            WRITE (6,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *         LINOUT(1:LNB (LINOUT))
            END IF
C                                       Log changes.
         WRITE (LUNLOG,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *      LINSAV(1:LNB (LINSAV))
         WRITE (LUNLOG,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         END IF
C                                       Exit.
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A,/)
 2000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A)
      END
      SUBROUTINE INCLUD
C-----------------------------------------------------------------------
C   1) eliminates standard INCLUDEd file names of the form C*.INC and
C      E*.INC (these have been appended to D*.INC)
C
C   2) converts INCLUDEs of IDCH.INC to INCLUDEs of DDCH.INC and
C      eliminates INCLUDEs of ZFT5.INC (i.e., the only place FTAB is
C      declared and dimensioned now is in DDCH.INC)
C
C   3) reports in the error file non-standard INCLUDEd file names (i.e.,
C      missing 'INCS:', file names that don't start with C,D,E,I,P,V or
C      Z and file names > 8 characters)
C
C   Inputs:
C      none
C   Output:
C      none
C-----------------------------------------------------------------------
      INTEGER   I, J, K, FNB, LNB, VERIFY
      CHARACTER LINSAV*80, LINTMP*80
      INCLUDE 'XLATE.INC'
C-----------------------------------------------------------------------
C                                       Set XLATE program unit name.
      CXUNIT = 'INCLUD'
C                                       Save copy of LINOUT for logging
C                                       changes.
      LINSAV = LINOUT
C                                       Debug print.
      IF (DEBUG) WRITE (6,*) 'INCLUD: ',LINOUT(1:LNB (LINOUT))
C                                       Eliminate INCLUDE statements for
C                                       C*.INC and E*.INC files if file
C                                       names are standard (i.e. < 5
C                                       characters not counting '.INC').
      I = INDEX (LINOUT, 'INCS:C')
      IF (I.GT.0) THEN
         J = INDEX (LINOUT, '.INC')
         K = J - I - 5
         IF (K.LE.4) LINOUT = ' '
         GO TO 900
         END IF
C                                       Check for E*.INC.
      I = INDEX (LINOUT, 'INCS:E')
      IF (I.GT.0) THEN
         J = INDEX (LINOUT, '.INC')
         K = J - I - 5
         IF (K.LE.4) LINOUT = ' '
         GO TO 900
         END IF
C                                       Check for IDCH.INC and convert
C                                       it to DDCH.INC which is the only
C                                       place where FTAB is now declared
C                                       and dimensioned.
      I = INDEX (LINOUT, 'IDCH.INC')
      IF (I.GT.0) LINOUT(I:I) = 'D'
C                                       Check for non-standard INCLUDEd
C                                       files.
      I = INDEX (LINOUT, 'INCS:')
      IF (I.GT.0) THEN
C                                       Check for non-standard INCLUDEd
C                                       file name (i.e., not C*.INC,
C                                       D*.INC, E*.INC, I*.INC, P*.INC,
C                                       V*.INC, Z*.INC or *.INC > 8).
         LINTMP = LINOUT(I+5:)
         J = INDEX (LINTMP, '''') - 1
         IF ((J.GT.8) .OR. (VERIFY (LINTMP(1:1), 'CDEIPVZ').GT.0)) THEN
            IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME, CXUNIT,
     *         '>', LINOUT(1:LNB (LINOUT))
            WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *         LINOUT(1:LNB (LINOUT))
            END IF
C                                       Also eliminate ZFT5.INC
         IF (LINTMP(1:J).EQ.'ZFT5.INC') LINOUT = ' '
      ELSE
C                                       'INCS:' missing.
         IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         ENDIF
C                                       Any changes?
 900  IF (LINOUT.NE.LINSAV) THEN
C                                       Debug print.
         IF (DEBUG) THEN
            WRITE (6,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *         LINSAV(1:LNB (LINSAV))
            WRITE (6,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *         LINOUT(1:LNB (LINOUT))
            END IF
C                                       Log changes.
         WRITE (LUNLOG,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *      LINSAV(1:LNB (LINSAV))
         WRITE (LUNLOG,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         END IF
C                                       Exit.
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A,/)
 2000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A)
      END
      SUBROUTINE DECLAR
C-----------------------------------------------------------------------
C   1) convert type and length declarations to ANSI standard type
C      declarations
C
C   2) report in the error file any LOGICAL*1 or BYTE declarations
C
C   Inputs:
C      none
C   Output:
C      none
C-----------------------------------------------------------------------
      INTEGER   I, J, K, FNB, LNB, VERIFY
      CHARACTER LINSAV*80, LINTMP*80
      INCLUDE 'XLATE.INC'
C-----------------------------------------------------------------------
C                                       Set XLATE program unit name.
      CXUNIT = 'DECLAR'
C                                       Save copy of LINOUT for logging
C                                       changes.
      LINSAV = LINOUT
C                                       Debug print.
      IF (DEBUG) WRITE (6,*) 'DECLAR: ',LINOUT(1:LNB (LINOUT))
C                                       Convert 'INTEGER*n' to
C                                       'INTEGER  '.
      I = INDEX (LINOUT, 'INTEGER*')
      IF (I.GT.0) THEN
         IF (INDEX (LINOUT, ' FUNCTION').GT.0) THEN
            LINTMP = LINOUT(1:I-1) // 'INTEGER' // LINOUT(I+9:)
         ELSE
            LINTMP = LINOUT(1:I-1) // 'INTEGER' // '  ' // LINOUT(I+9:)
            END IF
         LINOUT = LINTMP
         GO TO 900
         END IF
C                                       Convert 'REAL*4' to 'REAL  '.
      I = INDEX (LINOUT, 'REAL*4')
      IF (I.GT.0) THEN
         IF (INDEX (LINOUT, ' FUNCTION').GT.0) THEN
            LINTMP = LINOUT(1:I-1) // 'REAL' // LINOUT(I+6:)
         ELSE
            LINTMP = LINOUT(1:I-1) // 'REAL' // '  ' // LINOUT(I+6:)
            END IF
         LINOUT = LINTMP
         GO TO 900
         END IF
C                                       Convert 'REAL*8' to
C                                       'DOUBLE PRECISION' plus
C                                       continuation on next line.
      I = INDEX (LINOUT, 'REAL*8')
      IF (I.GT.0) THEN
         WRITE (LUNOUT,'(A)') '      DOUBLE PRECISION'
         LINTMP = '     *  ' // LINOUT(I+6:)
         LINOUT = LINTMP
         GO TO 900
         END IF
C                                       Convert 'LOGICAL*2' to
C                                       'LOGICAL  '.
      I = INDEX (LINOUT, 'LOGICAL*2')
      IF (I.GT.0) THEN
         IF (INDEX (LINOUT, ' FUNCTION').GT.0) THEN
            LINTMP = LINOUT(1:I-1) // 'LOGICAL' // LINOUT(I+9:)
         ELSE
            LINTMP = LINOUT(1:I-1) // 'LOGICAL' // '  ' // LINOUT(I+9:)
            END IF
         LINOUT = LINTMP
         GO TO 900
         END IF
C                                       Convert 'LOGICAL*4' to
C                                       'LOGICAL  '.
      I = INDEX (LINOUT, 'LOGICAL*4')
      IF (I.GT.0) THEN
         IF (INDEX (LINOUT, ' FUNCTION').GT.0) THEN
            LINTMP = LINOUT(1:I-1) // 'LOGICAL' // LINOUT(I+9:)
         ELSE
            LINTMP = LINOUT(1:I-1) // 'LOGICAL' // '  ' // LINOUT(I+9:)
            END IF
         LINOUT = LINTMP
         GO TO 900
         END IF
C                                       Report LOGICAL*1 and BYTE.
      IF ((INDEX (LINOUT, 'LOGICAL*1').GT.0) .OR.
     *   (INDEX (LINOUT, '      BYTE').GT.0)) THEN
C                                       Report comment in error file.
         IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         ENDIF
C                                       Any changes?
 900  IF (LINOUT.NE.LINSAV) THEN
C                                       Debug print.
         IF (DEBUG) THEN
            WRITE (6,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *         LINSAV(1:LNB (LINSAV))
            WRITE (6,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *         LINOUT(1:LNB (LINOUT))
            END IF
C                                       Log changes.
         WRITE (LUNLOG,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *      LINSAV(1:LNB (LINSAV))
         WRITE (LUNLOG,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         END IF
C                                       Exit.
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A,/)
 2000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A)
      END
      SUBROUTINE DEFTAB
C-----------------------------------------------------------------------
C   Eliminate declarations of the array FTAB which is only declared and
C   dimensioned in DDCH.INC now.
C
C   Inputs:
C      none
C   Output:
C      none
C-----------------------------------------------------------------------
      INTEGER   I, J, K, FNB, LNB, VERIFY
      CHARACTER LINSAV*80, LINTMP*80
      INCLUDE 'XLATE.INC'
C-----------------------------------------------------------------------
C                                       Set XLATE program unit name.
      CXUNIT = 'DEFTAB'
C                                       Save copy of LINOUT for logging
C                                       changes.
      LINSAV = LINOUT
C                                       Debug print.
      IF (DEBUG) WRITE (6,*) 'DEFTAB: ',LINOUT(1:LNB (LINOUT))
C                                       Only eliminate in INTEGER or
C                                       continuation statememts.
      IF ((INDEX (LINOUT, '      INTEGER').GT.0) .OR.
     *   (LINOUT(6:6).NE.' ')) THEN
C                                       Locate 'FTAB(...)' and extract
C                                       '...'.
         I = INDEX (LINOUT, 'FTAB(') - 1
         J = I + INDEX (LINOUT(I:), ')')
         LINTMP = LINOUT (I+6:J-2)
C                                       Verify that '...' is all digits
C                                       (i.e., try to leave non-
C                                       declaration continuation
C                                       statements involving FTAB alone;
C                                       no guarantee, just a good faith
C                                       effort).
         K = LNB (LINTMP)
         IF (VERIFY (LINTMP(1:K), '0123456789').EQ.0) THEN
C                                       Look backward for ','.
            K = LNB (LINOUT(1:I))
            IF (LINOUT(K:K).EQ.',') THEN
               I = K - 1
            ELSE
C                                       Look forward for ','.
               DO 10 K = 1,LEN (LINOUT) - J
                  IF ((LINOUT(J:J).NE.' ') .AND.
     *               (LINOUT(J:J).NE.',')) GO TO 20
                  J = J + 1
 10               CONTINUE
               END IF
 20         LINTMP = LINOUT(1:I) // LINOUT(J:)
            LINOUT = LINTMP
C                                       Zap otherwise empty INTEGER
C                                       or continuation statements.
            I = INDEX (LINOUT, 'INTEGER')
            IF (I.GT.0) THEN
               I = I + 7
            ELSE IF (LINOUT(6:6).NE.' ') THEN
               I = 7
               END IF
            IF (I.GT.0) THEN
               LINTMP = LINOUT(I:)
               IF (VERIFY (LINTMP, ' ,').EQ.0) LINOUT = ' '
               END IF
            END IF
         END IF
C                                       Any changes?
      IF (LINOUT.NE.LINSAV) THEN
C                                       Debug print.
         IF (DEBUG) THEN
            WRITE (6,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *         LINSAV(1:LNB (LINSAV))
            WRITE (6,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *         LINOUT(1:LNB (LINOUT))
            END IF
C                                       Log changes.
         WRITE (LUNLOG,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *      LINSAV(1:LNB (LINSAV))
         WRITE (LUNLOG,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         END IF
C                                       Exit.
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A,/)
 2000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A)
      END
      SUBROUTINE NDCODE
C-----------------------------------------------------------------------
C   Attempts to convert ENCODE/DECODE statements into equivalent Fortran
C   77 internal WRITE/READ statements.
C
C   NOTE: Apart from the conversion of ENCODE statements of the form:
C
C   'ENCODE (80,nnnn,MSGTXT)' to 'WRITE (MSGTXT,nnnn)'
C
C   all other translations MUST be regarded as incomplete and MUST be
C   examined for the additional code conversions that will almost always
C   be required.  For this reason, for all but the simple case cited
C   above, the original ENCODE/DECODE statements are preserved as
C   comments in the output source code module.  Furthermore, the
C   original and translated forms are recorded in a special log file
C   (i.e., XLATE.ND), whereas the original and translated forms of the
C   simple case only recorded in the "catch-all" log file (i.e.,
C   XLATE.LOG).
C
C   Inputs:
C      none
C   Output:
C      none
C-----------------------------------------------------------------------
      INTEGER   I, J, K, FNB, LNB, VERIFY, ICOD, ICHR, JCHR, NCHR,
     *   IFMT, JFMT, NFMT, ITGT, JTGT, KTGT, NTGT
      LOGICAL   DCODE
      CHARACTER LINSAV*80, LINTMP*80, ALPHNU*36, CHARS*16, FRMT*16,
     *   TARGET*32
      INCLUDE 'XLATE.INC'
      DATA ALPHNU /'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-----------------------------------------------------------------------
C                                       Set XLATE program unit name.
      CXUNIT = 'NDCODE'
C                                       Save copy of LINOUT for logging
C                                       changes.
      LINSAV = LINOUT
C                                       Assume this is an ENCODE
C                                       statement until proven
C                                       otherwise.
      DCODE = .FALSE.
C                                       Locate the beginning of the
C                                       'ENCODE' or 'DECODE' statement.
      ICOD = INDEX (LINOUT, 'ENCODE')
C                                       ENCODE?
      IF (ICOD.LE.0) THEN
C                                       Try DECODE.
         DCODE = .TRUE.
         ICOD = INDEX (LINOUT, 'DECODE')
         END IF
C                                       Either?
      IF (ICOD.GT.0) THEN
C                                       Locate and extract the number
C                                       of characters to transfer (i.e.,
C                                       first parameter in the ENCODE or
C                                       DECODE statement).  This must be
C                                       '80' to qualify as one of the
C                                       simple cases that should not
C                                       require any further attention,
C                                       e.g.,
C                                       ... ENCODE (80,1000,MSGTXT) ...
C                                       12345678901234567890123456789012
         ICHR = ICOD + INDEX (LINOUT(ICOD:), '(') - 1
C        ICHR = 5    + 8                          - 1 = 12
         ICHR = ICHR + VERIFY (LINOUT(ICHR:), '( ') - 1
C        ICHR = 12   + 2                            - 1 = 13
         JCHR = ICHR + VERIFY (LINOUT(ICHR:), ALPHNU) - 2
C        JCHR = 13   + 3                              - 2 = 14
         CHARS = LINOUT(ICHR:JCHR)
C        CHARS = LINOUT(13  :14  ) = '80'
         NCHR = JCHR - ICHR + 1
C         WRITE (6,*) 'CHARS(1:',NCHR,') = :',CHARS(1:NCHR),':'
C        NCHR = 14   - 13   + 1 = 2
C                                       Locate and extract the format
C                                       statement parameter (computed or
C                                       statement label), e.g., the
C                                       format statemet label '1000' in
C                                       ... ENCODE (80,1000,MSGTXT) ...
C                                       12345678901234567890123456789012
         IFMT = JCHR + INDEX (LINOUT(JCHR:), ',') - 1
C        IFMT = 14   + 2                           - 1 = 15
         IFMT = IFMT + VERIFY (LINOUT(IFMT:), ', ') - 1
C        IFMT = 15     + 2                        - 1 = 16
         JFMT = IFMT + VERIFY (LINOUT(IFMT:), ALPHNU) - 2
C        JFMT = 16   + 5                              - 2 = 19
         FRMT = LINOUT(IFMT:JFMT)
C        FRMT = LINOUT(16  :19  ) = '1000'
         NFMT = JFMT - IFMT + 1
C        NFMT = 19   - 16   + 1 = 4
C         WRITE (6,*) 'FRMT(1:',NFMT,') = :',FRMT(1:NFMT),':'
C                                       Locate and extract the output
C                                       variable, array or array element
C                                       (i.e., what is to become the
C                                       internal file in an internal
C                                       WRITE, only they can't be non-
C                                       CHARACTER arrays, like MSGTXT
C                                       used to be in AIPS), e.g.,
C                                       ... ENCODE (80,1000,MSGTXT) ...
C                                       12345678901234567890123456789012
         ITGT = JFMT + INDEX (LINOUT(JFMT:), ',') - 1
C        ITGT = 19   + 2                          - 1 = 20
         ITGT = ITGT + VERIFY (LINOUT(ITGT:), ', ') - 1
C        ITGT = 20   + 2                            - 1 = 21
C                                       Process only one level of
C                                       parentheses.
         JTGT = ITGT + INDEX (LINOUT(ITGT:), ')') - 1
C        JTGT = 21   + 7                          - 1 = 27
         KTGT = ITGT + INDEX (LINOUT(ITGT:), '(') - 1
C        KTGT = 21   + 0                          - 1 = 20
         IF ((KTGT.GT.ITGT) .AND. (KTGT.LT.JTGT)) THEN
            JTGT = JTGT
C           27   = 27
         ELSE
            JTGT = ITGT + VERIFY (LINOUT(ITGT:), ALPHNU) - 2
C           JTGT = 21   + 7                                - 2 = 26
            END IF
C                                       Error branching sometimes
C                                       follows the target, but these
C                                       will stay intact.
         TARGET = LINOUT(ITGT:JTGT)
C        TARGET = LINOUT(21  :26) = 'MSGTXT'
         NTGT = JTGT - ITGT + 1
C        NTGT = 26   - 21   + 1 = 6
C         WRITE (6,*) 'TARGET(1:',NTGT,') = :',TARGET(1:NTGT),':'
C                                       Finally, form the "equivalent"
C                                       internal WRITE (for ENCODEs) or
C                                       internal READ (for DECODEs).
         IF (DCODE) THEN
            LINTMP = LINOUT (1:ICOD-1) // 'READ (' // TARGET(1:NTGT) //
     *         ',' // FRMT(1:NFMT) // LINOUT(JTGT+1:)
         ELSE
            LINTMP = LINOUT (1:ICOD-1) // 'WRITE (' // TARGET(1:NTGT) //
     *         ',' // FRMT(1:NFMT) // LINOUT(JTGT+1:)
            END IF
C                                       Copy the conversion to the
C                                       output buffer.
         LINOUT = LINTMP
C                                       Debug?
         IF (DEBUG) THEN
            WRITE (6,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *         LINSAV(1:LNB (LINSAV))
            WRITE (6,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *         LINOUT(1:LNB (LINOUT))
            END IF
C                                       Record only the conversions that
C                                       require further attention in the
C                                       special ENCODE/DECODE log file
C                                       (i.e., XLATE.ND).
         IF ((CHARS.EQ.'80') .AND. (TARGET.EQ.'MSGTXT')) GO TO 900
C                                       Insert the orginal line as a
C                                       comment in the output source
C                                       code module.
            LINTMP = 'C ??? ' // LINSAV(7:)
            WRITE (LUNOUT,'(A)') LINTMP(1:LNB (LINTMP))
C                                       Record conversion in XLATE.ND.
            WRITE (LUNND,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *         LINSAV(1:LNB (LINSAV))
            WRITE (LUNND,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *         LINOUT(1:LNB (LINOUT))
C                                       Record ALL conversions in the
C                                       "catch-all" log file.
 900     WRITE (LUNLOG,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *      LINSAV(1:LNB (LINSAV))
         WRITE (LUNLOG,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         END IF
C                                       Exit.
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A,/)
 2000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A)
      END
      SUBROUTINE NWDPLI
C-----------------------------------------------------------------------
C   Looks for calls to COPY using NWDPLI as an argument and converts
C   them into simple assignment statements.
C-----------------------------------------------------------------------
      INTEGER   I, J, K, FNB, LNB, VERIFY
      CHARACTER LINSAV*80, LINTMP*80, KFROM*80, KTO*80
      INCLUDE 'XLATE.INC'
C-----------------------------------------------------------------------
C                                       Set XLATE program unit name.
      CXUNIT = 'NWDPLI'
C                                       Save copy of LINOUT for logging
C                                       changes.
      LINSAV = LINOUT
C                                       Debug print.
      IF (DEBUG) WRITE (6,*) 'NWDPLI: ',LINOUT(1:LNB (LINOUT))
C                                       Locate CALL statement.
      I = INDEX (LINOUT, 'CALL COPY') - 1
C                                       Locate second argument.
      J = I + INDEX (LINOUT(I:), ',') + 1
 10   IF (LINOUT(J:J).NE.' ') GO TO 20
         J = J + 1
         GO TO 10
 20   LINTMP = LINOUT(J:)
      K = INDEX (LINTMP, ' ') - 2
      KFROM = LINTMP(1:K)
C                                       Locate third argument.
      J = J + K + 1
 30   IF (LINOUT(J:J).NE.' ') GO TO 40
         J = J + 1
         GO TO 30
 40   LINTMP = LINOUT(J:)
      K = INDEX (LINTMP, ' ') - 2
      KTO = LINTMP(1:K)
C                                       Replace call with an assignment
C                                       statement.
      LINTMP = LINOUT(1:I) // KTO(1:LNB (KTO)) // ' = ' // KFROM
      LINOUT = LINTMP
C                                       Debug print.
      IF (DEBUG) THEN
         WRITE (6,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *      LINSAV(1:LNB (LINSAV))
         WRITE (6,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         END IF
C                                       Log changes.
      WRITE (LUNLOG,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *   LINSAV(1:LNB (LINSAV))
      WRITE (LUNLOG,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *   LINOUT(1:LNB (LINOUT))
C                                       Exit.
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A,/)
 2000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A)
      END
      SUBROUTINE NONGEN
C-----------------------------------------------------------------------
C   1) convert type-specific to generic intrinsic functions:
C
C      DSQRT                     -> SQRT
C      ALOG, DLOG                -> LOG
C      ALOG10, DLOG10            -> LOG10
C      DEXP                      -> EXP
C      DSIN                      -> SIN
C      DASIN, ARSIN, DARSIN      -> ASIN
C      DCOS                      -> COS
C      DACOS, ARCOS, DARCOS      -> ACOS
C      DTAN                      -> TAN
C      DATAN                     -> ATAN
C      DATAN2                    -> ATAN2
C      IABS, DABS, IIABS, JIABS  -> ABS
C      IFIX, IINT, JINT, IDINT   -> INT
C      FLOAT                     -> REAL
C      ISIGN, DSIGN              -> SIGN
C      MAX0, AMAX1, DMAX1        -> MAX
C      MIN0, AMIN1, DMIN1        -> MIN
C      IMOD, JMOD, AMOD, DMOD    -> MOD
C
C   2) report uses of questionable intrinsic functions:
C
C      MAX1     AMAX0    MIN1     AMIN0    AINT
C
C   2) report uses of non-standard intrinsic functions:
C
C      SIND     ASIND    DASIND   COSD     ACOSD    DACOSD
C      TAND     ATAND    DATAND   ATAN2D   DATAN2D
C
C-----------------------------------------------------------------------
      INTEGER   I, J, K, FNB, LNB, VERIFY
      CHARACTER LINSAV*80, LINTMP*80, PUNCT*9
      LOGICAL   CHANGE
      INCLUDE 'XLATE.INC'
      DATA PUNCT /' +-*/()=,'/
C-----------------------------------------------------------------------
C                                       Set XLATE program unit name.
      CXUNIT = 'NONGEN'
C                                       Save copy of LINOUT for logging
C                                       changes.
      LINSAV = LINOUT
C                                       Debug print.
      IF (DEBUG) WRITE (6,*) 'NONGEN: ',LINOUT(1:LNB (LINOUT))
C                                       Prime main loop.
      CHANGE = .TRUE.
C                                       Loop until no change.
 1    IF (CHANGE) THEN
C                                       Toggle change status.
         CHANGE = .FALSE.
C                                       Check for DSQRT.
         I = INDEX (LINOUT, 'SQRT')
         IF (I.GT.0) THEN
C                                       Convert DSQRT to SQRT.
            I = INDEX (LINOUT, 'DSQRT')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DSQRT')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'SQRT' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Check for ALOG, DLOG, ALOG10
C                                       and DLOG10.
         I = INDEX (LINOUT, 'LOG')
         IF (I.GT.0) THEN
C                                       Convert ALOG10 to LOG10.
            I = INDEX (LINOUT, 'ALOG10')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('ALOG10')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'LOG10' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert DLOG10 to LOG10.
            I = INDEX (LINOUT, 'DLOG10')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DLOG10')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'LOG10' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Convert ALOG to LOG.
            I = INDEX (LINOUT, 'ALOG')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('ALOG')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'LOG' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert DLOG to LOG.
            I = INDEX (LINOUT, 'DLOG')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DLOG')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'LOG' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Check for DEXP.
         I = INDEX (LINOUT, 'EXP')
         IF (I.GT.0) THEN
C                                       Convert DEXP to EXP.
            I = INDEX (LINOUT, 'DEXP')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DEXP')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'EXP' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Check for DSIN, DASIN, ARSIN
C                                       and DARSIN.
         I = INDEX (LINOUT, 'SIN')
         IF (I.GT.0) THEN
C                                       Convert DSIN to SIN.
            I = INDEX (LINOUT, 'DSIN')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DSIN')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'SIN' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert DASIN to ASIN.
            I = INDEX (LINOUT, 'DASIN')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DASIN')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'ASIN' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert ARSIN to ASIN.
            I = INDEX (LINOUT, 'ARSIN')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('ARSIN')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'ASIN' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert DARSIN to ASIN.
            I = INDEX (LINOUT, 'DARSIN')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DARSIN')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'ASIN' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Check for DCOS, DACOS, ARCOS
C                                       and DARCOS.
         I = INDEX (LINOUT, 'COS')
         IF (I.GT.0) THEN
C                                       Convert DCOS to COS.
            I = INDEX (LINOUT, 'DCOS')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DCOS')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'COS' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert DACOS to ACOS.
            I = INDEX (LINOUT, 'DACOS')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DACOS')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'ACOS' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert ARCOS to ACOS.
            I = INDEX (LINOUT, 'ARCOS')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('ARCOS')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'ACOS' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert DARCOS to ACOS.
            I = INDEX (LINOUT, 'DARCOS')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DARCOS')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'ACOS' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Check for DTAN, DATAN and
C                                       DATAN2.
         I = INDEX (LINOUT, 'TAN')
         IF (I.GT.0) THEN
C                                       Convert DTAN to TAN.
            I = INDEX (LINOUT, 'DTAN')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DTAN')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'TAN' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert DATAN to ATAN.
            I = INDEX (LINOUT, 'DATAN')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DATAN')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'ATAN' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert DATAN2 to ATAN2.
            I = INDEX (LINOUT, 'DATAN2')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DATAN2')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'ATAN2' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Check for IABS, DABS, IIABS
C                                       and JIABS.
         I = INDEX (LINOUT, 'ABS')
         IF (I.GT.0) THEN
C                                       Convert IABS to ABS.
            I = INDEX (LINOUT, 'IABS')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('IABS')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'ABS' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert DABS to ABS.
            I = INDEX (LINOUT, 'DABS')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DABS')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'ABS' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert IIABS to ABS.
            I = INDEX (LINOUT, 'IIABS')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('IIABS')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'ABS' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert JIABS to ABS.
            I = INDEX (LINOUT, 'JIABS')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('JIABS')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'ABS' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Check for IFIX.
         I = INDEX (LINOUT, 'FIX')
         IF (I.GT.0) THEN
C                                       Convert IFIX to INT.
            I = INDEX (LINOUT, 'IFIX')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('IFIX')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'INT' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Check for IINT, JINT and IDINT.
         I = INDEX (LINOUT, 'INT')
         IF (I.GT.0) THEN
C                                       Convert IINT to INT.
            I = INDEX (LINOUT, 'IINT')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('IINT')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'INT' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert JINT to INT.
            I = INDEX (LINOUT, 'JINT')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('JINT')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'INT' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert IDINT to INT.
            I = INDEX (LINOUT, 'IDINT')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('IDINT')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'INT' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Check for FLOAT.
         I = INDEX (LINOUT, 'FLOAT')
         IF (I.GT.0) THEN
C                                       Convert FLOAT to REAL.
            I = INDEX (LINOUT, 'FLOAT')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('FLOAT')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'REAL' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Check for ISIGN and DSIGN.
         I = INDEX (LINOUT, 'SIGN')
         IF (I.GT.0) THEN
C                                       Convert ISIGN to SIGN.
            I = INDEX (LINOUT, 'ISIGN')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('ISIGN')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'SIGN' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert DSIGN to SIGN.
            I = INDEX (LINOUT, 'DSIGN')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DSIGN')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'SIGN' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Check for MAX0, AMAX1 and DMAX1.
         I = INDEX (LINOUT, 'MAX')
         IF (I.GT.0) THEN
C                                       Convert MAX0 to MAX.
            I = INDEX (LINOUT, 'MAX0')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('MAX0')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'MAX' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert AMAX1 to MAX.
            I = INDEX (LINOUT, 'AMAX1')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('AMAX1')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'MAX' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert DMAX1 to MAX.
            I = INDEX (LINOUT, 'DMAX1')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DMAX1')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'MAX' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Check for MIN0, AMIN1 and DMIN1.
         I = INDEX (LINOUT, 'MIN')
         IF (I.GT.0) THEN
C                                       Convert MIN0 to MIN.
            I = INDEX (LINOUT, 'MIN0')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('MIN0')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'MIN' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert AMIN1 to MIN.
            I = INDEX (LINOUT, 'AMIN1')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('AMIN1')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'MIN' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert DMIN1 to MIN.
            I = INDEX (LINOUT, 'DMIN1')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DMIN1')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'MIN' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Check for IMOD, JMOD, AMOD
C                                       and DMOD.
         I = INDEX (LINOUT, 'MOD')
         IF (I.GT.0) THEN
C                                       Convert IMOD to MOD.
            I = INDEX (LINOUT, 'IMOD')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('IMOD')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'MOD' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert JMOD to MOD.
            I = INDEX (LINOUT, 'JMOD')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('JMOD')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'MOD' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert AMOD to MOD.
            I = INDEX (LINOUT, 'AMOD')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('AMOD')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'MOD' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Convert DMOD to MOD.
            I = INDEX (LINOUT, 'DMOD')
            IF (I.GT.0) THEN
               IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
                  J = I + LEN ('DMOD')
                  LINTMP = LINOUT(J:)
                  K = VERIFY (LINTMP, ' ')
                  IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
                     LINTMP = LINOUT(1:I-1) // 'MOD' // LINOUT(J:)
                     LINOUT = LINTMP
                     CHANGE = .TRUE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Continue.
         GO TO 1
         END IF
C                                       Any changes?
      IF (LINOUT.NE.LINSAV) THEN
C                                       Debug print.
         IF (DEBUG) THEN
            WRITE (6,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *         LINSAV(1:LNB (LINSAV))
            WRITE (6,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *         LINOUT(1:LNB (LINOUT))
            END IF
C                                       Log changes.
         WRITE (LUNLOG,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *      LINSAV(1:LNB (LINSAV))
         WRITE (LUNLOG,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         END IF
C                                       Report questionable use of
C                                       MAX1.
      I = INDEX (LINOUT, 'MAX1')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('MAX1')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report questionable use of
C                                       AMAX0.
      I = INDEX (LINOUT, 'AMAX0')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('AMAX0')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report questionable use of
C                                       MIN1.
      I = INDEX (LINOUT, 'MIN1')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('MIN1')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report questionable use of
C                                       AMIN0.
      I = INDEX (LINOUT, 'AMIN0')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('AMIN0')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report questionable use of
C                                       AINT.
      I = INDEX (LINOUT, 'AINT')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('AINT')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report uses of non-standard
C                                       SIND.
      I = INDEX (LINOUT, 'SIND')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('SIND')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report uses of non-standard
C                                       ASIND.
      I = INDEX (LINOUT, 'ASIND')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('ASIND')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report uses of non-standard
C                                       DASIND.
      I = INDEX (LINOUT, 'DASIND')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('DASIND')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report uses of non-standard
C                                       COSD.
      I = INDEX (LINOUT, 'COSD')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('COSD')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report uses of non-standard
C                                       ACOSD.
      I = INDEX (LINOUT, 'ACOSD')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('ACOSD')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report uses of non-standard
C                                       DACOSD.
      I = INDEX (LINOUT, 'DACOSD')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('DACOSD')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report uses of non-standard
C                                       TAND.
      I = INDEX (LINOUT, 'TAND')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('TAND')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report uses of non-standard
C                                       ATAND.
      I = INDEX (LINOUT, 'ATAND')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('ATAND')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report uses of non-standard
C                                       DATAND.
      I = INDEX (LINOUT, 'DATAND')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('DATAND')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report uses of non-standard
C                                       ATAN2D.
      I = INDEX (LINOUT, 'ATAN2D')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('ATAN2D')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Report uses of non-standard
C                                       DATAN2D.
      I = INDEX (LINOUT, 'DATAN2D')
      IF (I.GT.0) THEN
         IF (VERIFY (LINOUT(I-1:I-1), PUNCT).EQ.0) THEN
            J = I + LEN ('DATAN2D')
            LINTMP = LINOUT(J:)
            K = VERIFY (LINTMP, ' ')
            IF ((K.NE.0) .AND. (LINTMP(K:K).EQ.'(')) THEN
               IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME,
     *            CXUNIT, '>', LINOUT(1:LNB (LINOUT))
               WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *            LINOUT(1:LNB (LINOUT))
               ENDIF
            END IF
         END IF
C                                       Exit.
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A,/)
 2000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A)
      END
      SUBROUTINE P4CALL
C-----------------------------------------------------------------------
C   Checks for calls to routines known to take pseudo I*4 arguments and
C   simply reports them in the error file, except for calls to ZFI3
C   (takes I*2 NREC) which are replaced with calls to ZFIO (takes I*4
C   NREC).
C
C   Inputs:
C      none
C   Output:
C      none
C-----------------------------------------------------------------------
      INTEGER   I, J, K, FNB, LNB, VERIFY
      CHARACTER LINSAV*80, LINTMP*80
      INCLUDE 'XLATE.INC'
C-----------------------------------------------------------------------
C                                       Set XLATE program unit name.
      CXUNIT = 'P4CALL'
C                                       Save copy of LINOUT for logging
C                                       changes.
      LINSAV = LINOUT
C                                       Debug print.
      IF (DEBUG) WRITE (6,*) 'P4CALL: ',LINOUT(1:LNB (LINOUT))
C                                       Check for CALLs to routines that
C                                       take pseudo I*4 arguments.
      IF ((INDEX (LINOUT, 'API3').GT.0) .OR.
     *   (INDEX (LINOUT, 'APXPO3').GT.0) .OR.
     *   (INDEX (LINOUT, 'COMOF3').GT.0) .OR.
     *   (INDEX (LINOUT, 'CONVF3').GT.0) .OR.
     *   (INDEX (LINOUT, 'FSWTC3').GT.0) .OR.
     *   (INDEX (LINOUT, 'GRDTA3').GT.0) .OR.
     *   (INDEX (LINOUT, 'I2TOR4').GT.0) .OR.
     *   (INDEX (LINOUT, 'MDIS3').GT.0) .OR.
     *   (INDEX (LINOUT, 'MINI3').GT.0) .OR.
     *   (INDEX (LINOUT, 'MINS3').GT.0) .OR.
     *   (INDEX (LINOUT, 'MSKI3').GT.0) .OR.
     *   (INDEX (LINOUT, 'ZCREA3').GT.0) .OR.
     *   (INDEX (LINOUT, 'ZMATH4').GT.0) .OR.
     *   (INDEX (LINOUT, 'ZMI3').GT.0) .OR.
     *   (INDEX (LINOUT, 'ZR8P4').GT.0)) THEN
C                                       Report CALL in error file.
         IF (DEBUG) WRITE (6,1000) NAMMOD,  PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         GO TO 999
         END IF
C                                       Replace calls to ZFI3 with
C                                       calls to ZFIO.
      I = INDEX (LINOUT, ' ZFI3')
      IF (I.GT.0) THEN
         I = I + 4
         LINTMP = LINOUT
         LINTMP(I:I) = 'O'
C                                       Debug print.
         IF (DEBUG) THEN
            WRITE (6,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *         LINSAV(1:LNB (LINSAV))
            WRITE (6,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *         LINOUT(1:LNB (LINOUT))
            END IF
C                                       Log changes.
         WRITE (LUNLOG,2000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '<',
     *      LINSAV(1:LNB (LINSAV))
         WRITE (LUNLOG,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         END IF
C                                       Exit.
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A,/)
 2000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A)
      END
      SUBROUTINE OTHER
C-----------------------------------------------------------------------
C   Checks for other things worth noting, but doesn't change anything.
C
C   Inputs:
C      none
C   Output:
C      none
C-----------------------------------------------------------------------
      INTEGER   I, J, K, FNB, LNB, VERIFY
      CHARACTER LINTMP*80
      INCLUDE 'XLATE.INC'
C-----------------------------------------------------------------------
C                                       Set XLATE program unit name.
      CXUNIT = 'OTHER'
C                                       Debug print.
      IF (DEBUG) WRITE (6,*) 'OTHER: ',LINOUT(1:LNB (LINOUT))
C                                       Check for '!' not in FORMAT
C                                       statements (i.e., in-line
C                                       comments).
      IF ((INDEX (LINOUT, 'FORMAT').LE.0) .AND.
     *   (INDEX (LINOUT, '!').GT.0)) THEN
C                                       Debug print.
         IF (DEBUG) WRITE (6,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
C                                       Report probable in-line
C                                       comment in the error file.
         WRITE (LUNERR,1000) NAMMOD, PGTYPE, PGNAME, CXUNIT, '>',
     *      LINOUT(1:LNB (LINOUT))
         END IF
C                                       Exit.
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X,A,2X,A,2X,A,2X,A,1X,A,1X,A,/)
      END
      INTEGER FUNCTION FNB (STRING)
C-----------------------------------------------------------------------
C   Locates the position of the first non-blank character in a string.
C
C   Inputs:
C      STRING   C*(*)      String to search
C   Output:
C      none
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
C
      INTEGER SLEN
      INCLUDE 'XLATE.INC'
C-----------------------------------------------------------------------
      FNB = 0
      SLEN = LEN (STRING) + 1
C                                       Look forwards for first
C                                       non-blank character.
 10   FNB = FNB + 1
      IF (FNB.GT.SLEN) GO TO 999
         IF (STRING(FNB:FNB).EQ.' ') GO TO 10
C                                       Exit.
 999  RETURN
      END
      INTEGER FUNCTION LNB (STRING)
C-----------------------------------------------------------------------
C   Locates the position of the last non-blank character in a string.
C
C   Inputs:
C      STRING   C*(*)      String to search
C   Output:
C      none
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
C
      INCLUDE 'XLATE.INC'
C-----------------------------------------------------------------------
      LNB = LEN (STRING) + 1
C                                       Look backwards for first
C                                       non-blank character.
 10   LNB = LNB - 1
      IF (LNB.LT.1) GO TO 999
         IF (STRING(LNB:LNB).EQ.' ') GO TO 10
C                                       Exit.
 999  RETURN
      END
      INTEGER FUNCTION VERIFY (STR1, STR2)
C-----------------------------------------------------------------------
C   Finds the first occurence of a character in STR1 that is not in
C   STR2.
C
C   Inputs:
C      STR1     C*(*)      String to verify
C      STR2     C*(*)      String containing characters to verify
C   Output:
C      none
C-----------------------------------------------------------------------
      CHARACTER STR1*(*), STR2*(*)
C
      INTEGER   LSTR1, LSTR2, I, J
      INCLUDE 'XLATE.INC'
C-----------------------------------------------------------------------
      VERIFY = 0
      LSTR1 = LEN (STR1)
      LSTR2 = LEN (STR2)
      IF (DEBUG) WRITE (6,*) 'VERIFY: STR2 = ',STR2,' LSTR2 = ',LSTR2
      I = 0
 10   I = I + 1
      IF (I.GT.LSTR1) GO TO 999
         J = 0
 20      J = J + 1
         IF (J.GT.LSTR2) GO TO 30
            IF (STR1(I:I).EQ.STR2(J:J)) GO TO 10
            GO TO 20
 30      VERIFY = I
C                                       Exit.
 999  RETURN
      END

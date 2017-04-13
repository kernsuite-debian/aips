      PROGRAM MRTAPE
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C    VAX/VMS program for operations involving tapes containing source
C    code in 80 byte records, with EOFs between files.  The 3.0 system
C    service SYS$MOUNT is used to make the program self contained and
C    more convienent for users.
C    This program was derived from TCOPY.
C
C   TCOPY notes:
C       The operation of this program depends on undocumented features
C   of the VMS I/O system. We are reading a foreign tape which the OPEN
C   declares to be RECORDTYPE='VARIABLE'. The RMS algorithms appear to
C   just pass us the raw bytes in this case (exactly what we want). The
C   Q format allows us to determine the block length. On output we are
C   writing a foreign tape with the VARIABLE record type and RMS again
C   does just what we want (i.e., it does not put the byte count on the
C   front of the block!). The correct operation of this process is
C   quite remarkable considering that there is no hint that it might
C   work in any DEC manual.
C
C       The use of CLOSE to generate a tapemark on a foreign tape is
C   documented in a response to a problem report. See:
C         VAX System Dispatch, September 1981 (AD-L034A-B2), p. 45,
C         VAX/VMS V2.3 RUN-TIME LIBRARY FORTRAN LANGUAGE SUPPORT,
C         "ENDFILE statement gets QIO error".
C   The discussion given in this response includes several remarks
C   about the properties of, use of, and abuse of RMS with foreign
C   tapes, and experiments based on these hints resulted in this
C   program which shows that there are other interesting
C   undocumented features in the system.
C   DCW, NRAO-CV, 18Mar82.
C-----------------------------------------------------------------------
      INTEGER        CRT, OPTION, INPUT, OUTPUT, MAXBLK, INPUT_I2, WEFN
      CHARACTER*64   TAPE_NAME
      CHARACTER      BLK*(32000)
      DATA CRT /6/, INPUT /10/, OUTPUT /11/, MAXBLK /32000/
C-----------------------------------------------------------------------
C
C     Select, open, mount the input and output devices.
        CALL MOUNT (TAPE_NAME)
C
      OPTION = 0
      DO WHILE (OPTION.NE.-1)
C       Print current I/O files, master menu. Let user select an option.
          WRITE (CRT, '(/)' )
          WRITE (CRT, '('' MRTAPE MASTER MENU'')' )
          WRITE (CRT, '(/)' )
          WRITE (CRT,
     *      '(''  1  COPY DISK FILES TO TAPE (80 BYTE BLOCKS)'')' )
          WRITE (CRT,
     *      '(''  2  CATALOG TAPE FILES (80 BYTE BLOCKS)'')' )
          WRITE (CRT, '(''  3  DUMP 1ST LINE FROM INPUT FILES'')' )
          WRITE (CRT, '(''  4  REWIND A DEVICE'')' )
          WRITE (CRT,
     *      '(''  5  COPY DISK FILES FOUND IN TEXT FILE, TO TAPE'')' )
          WRITE (CRT, '(/)' )
C
          OPTION = INPUT_I2 ('ENTER OPTION, -1 TO STOP:',-1,5)
C
          IF (OPTION.EQ.1) THEN
C           Copy disk files to tape.
              CALL TCOPY (BLK, INPUT, OUTPUT, TAPE_NAME)
C
          ELSE IF (OPTION.EQ.2) THEN
C           Copy tape files to disk.
              CALL DCOPY (BLK, INPUT, OUTPUT, TAPE_NAME)
C
          ELSE IF (OPTION.EQ.3) THEN
C           Print 1st line of each file.
             CALL DUMP (BLK, INPUT, TAPE_NAME)
C
          ELSE IF (OPTION.EQ.4) THEN
C           Rewind a device.
              CALL REW (TAPE_NAME, INPUT)
C
          ELSE IF (OPTION.EQ.5) THEN
C           Copy list of files to tape.
              CALL LCOPY (BLK, INPUT, OUTPUT, TAPE_NAME)
            END IF
C
        END DO
      STOP
      END
      SUBROUTINE MOUNT (TAPE_NAME)
C-----------------------------------------------------------------------
C   Select or change the input and output device or file.  Open files
C   and mount devices.
C-----------------------------------------------------------------------
      PARAMETER DVI$_MOUNTCNT = '0000001E'X
      INTEGER   I4ERR, SYS$MOUNT, FLAG, MAXBLK, SYS$GETDVI, MNTCNT
      INTEGER   ADD1, ADD2, ADD3, NOP1, NOP2, NOP3, ENDLIST
      INTEGER   GADD1, GNOP1, GENDLST, DEVNAM(2)
      INTEGER   CODE1, CODE2, CODE3, LEN1, LEN2, LEN3
      INTEGER   GLEN1, GCODE1
      INTEGER   CRT, IFIND
      CHARACTER*64 TAPE_NAME, INPUT_CHAR
      BYTE IOSTR(64)
      INCLUDE '($MNTDEF)'
      COMMON /ARGLIST/ LEN1, CODE1, ADD1, NOP1,
     *                 LEN2, CODE2, ADD2, NOP2,
     *                 LEN3, CODE3, ADD3, NOP3,
     *                 ENDLIST
      COMMON /ARGL005/ GLEN1, GCODE1, GADD1, GNOP1,
     *                 GENDLST
      DATA CRT /6/
      DATA LEN1 /10/, LEN2 /4/, LEN3 /4/
      DATA GLEN1 /4/
      DATA NOP1 /0/, NOP2 /0/, NOP3 /0/, ENDLIST /0/
      DATA GNOP1 /0/, GENDLST /0/
      DATA MAXBLK /32000/, FLAG /MNT$M_FOREIGN/
      DATA CODE1 /MNT$_DEVNAM/, CODE2 /MNT$_FLAGS/,
     *  CODE3 /MNT$_BLOCKSIZE/
      DATA GCODE1 /DVI$_MOUNTCNT/
C-----------------------------------------------------------------------
C
      I4ERR = 0
      DO WHILE (I4ERR.NE.1)
        TAPE_NAME = INPUT_CHAR ('ENTER TAPE DRIVE NAME ')
C         See if we need to add a colun.
            IFIND = INDEX (TAPE_NAME, ':')
            IF (IFIND.LE.0) THEN
              IFIND = INDEX (TAPE_NAME, ' ')
              TAPE_NAME(IFIND:IFIND) = ':'
              END IF
        DO I = 1,64
          IOSTR(I) = ICHAR (TAPE_NAME(I:I))
          END DO
C                                       Find out if dev already mounted
        MNTCNT = 0
        WEFN = 10
        GADD1 = %LOC (MNTCNT)
        DEVNAM(1) = IFIND
        DEVNAM(2) = %LOC (IOSTR)
        I4ERR = SYS$GETDVI (%VAL(WEFN), , DEVNAM, GLEN1, , , ,)
        I4ERR = SYS$WAITFR (%VAL(WEFN))
        IF (MNTCNT.EQ.0) THEN
          ADD1 = %LOC (IOSTR)
          ADD2 = %LOC (FLAG)
          ADD3 = %LOC (MAXBLK)
          I4ERR = SYS$MOUNT (LEN1)
          CALL PRINT_ERR (I4ERR)
        ELSE
          WRITE (CRT,
     *      '('' Device was already mounted. I hope by you.'' )' )
          I4ERR = 1
          END IF
        END DO
C
      RETURN
      END
      SUBROUTINE TCOPY (BLK, INPUT, OUTPUT, TAPE_NAME)
C-----------------------------------------------------------------------
C  TCOPY will copy user selected disk source files (80 byte records)
C  to tape.  An end of file will be written on tape after each selected
C  file.
C   INPUTS:
C     BLK       C(*)   A large working buffer.
C     INPUT     I      Fortran logical unit no. of an open input file.
C     OUTPUT    I      Fortran logical unit no. of an open output file.
C     TAPE_NAME C*64   Device name of the mounted tape.
C-----------------------------------------------------------------------
      INTEGER   INPUT, OUTPUT, IERR, NBYT, INPUT_I2, CRT, IFIND
      CHARACTER*64 TAPE_NAME, DISK_FILE, INPUT_CHAR
      CHARACTER*(*) BLK
      DATA CRT /6/
C-----------------------------------------------------------------------
C
      DISK_FILE = 'JUNK'
C     Loop until user enters <cr> for disk file.
        DO WHILE (DISK_FILE.NE.' ')
          DISK_FILE = INPUT_CHAR ('ENTER DISK FILE NAME. <CR>=EXIT')
          IF (DISK_FILE.NE.' ') THEN
C           See if we need to add default extension.
              IFIND = INDEX (DISK_FILE, '.')
              IF (IFIND.LE.0) THEN
C               Add '.FOR' to end of DISK_FILE.
                  IFIND = INDEX (DISK_FILE, ' ') - 1
                  DISK_FILE = DISK_FILE(1:IFIND) // '.FOR'
                END IF
C           Open the disk file.
              OPEN (UNIT=INPUT, FILE=DISK_FILE, STATUS='OLD',
     *          ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *          READONLY, IOSTAT=IERR,
     *         RECORDTYPE='VARIABLE', RECL=MAXBLK, BUFFERCOUNT=2)
            IF (IERR.NE.0) THEN
              CALL PRINT_ERR (0)
            ELSE
C             Open the tape file.
                OPEN (UNIT=OUTPUT, FILE=TAPE_NAME, STATUS='OLD',
     *            ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *            IOSTAT=IERR,
     *            RECORDTYPE='VARIABLE', RECL=80, BUFFERCOUNT=2)
                IF (IERR.NE.0) THEN
                   CALL PRINT_ERR (0)
                ELSE
C                 Copy disk to tape.
                    IERR = 0
                    DO WHILE (IERR.EQ.0)
                      READ (INPUT, '(Q, A)', IOSTAT=IERR)
     *                  NBYT, BLK(1:NBYT)
                      IF (IERR.EQ.0) THEN
C                       Pad with blanks if necessary.
                          IF (NBYT.LT.80) BLK(NBYT+1:80) = ' '
                        WRITE (OUTPUT, '(A)', IOSTAT=IERR) BLK(1:80)
                        IF (IERR.NE.0) CALL PRINT_ERR (0)
                        END IF
                      END DO
                  CLOSE (UNIT=INPUT)
C                 Close the tape file, implicitly writting an EOF.
                    CLOSE (UNIT=OUTPUT)
                  END IF
              END IF
            END IF
          END DO
C     See if user wants EOT mark.
        OPTION = INPUT_I2 (' 1 = WRITE END OF TAPE MARK, 0 = NO',0,1)
        IF (OPTION.EQ.1) THEN
          WRITE (CRT, '(1X,''Writing end of tape'')' )
          OPEN (UNIT=OUTPUT, FILE=TAPE_NAME, STATUS='OLD',
     *      ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *      IOSTAT=IERR,
     *      RECORDTYPE='VARIABLE', RECL=80, BUFFERCOUNT=2)
          CLOSE (UNIT=OUTPUT)
          END IF
      RETURN
      END
      SUBROUTINE DCOPY (BLK, INPUT, OUTPUT, TAPE_NAME)
C-----------------------------------------------------------------------
C   DCOPY will copy variable length record blocks from tape, printing
C   the first line of each file and allowing the user to choose a disk
C   file name for the file.
C   INPUTS:
C     BLK       C(*)   A large working buffer.
C     INPUT     I      Fortran logical unit no. of an open input file.
C     OUTPUT    I      Fortran logical unit no. of an open output file.
C     TAPE_NAME C*64   Device name of the mounted tape.
C-----------------------------------------------------------------------
      INTEGER   CRT, INPUT, OUTPUT, IERR, MAXBLK,
     *  NBYT, ITRIM, IFIND, N70
      CHARACTER*(*) BLK
      CHARACTER*64 TAPE_NAME, DISK_FILE, INPUT_CHAR
      LOGICAL*1 NOTEOT, NOTEOF, SKIP
      DATA CRT,N70 /6,70/
C-----------------------------------------------------------------------
C
      MAXBLK = LEN (BLK)
C
C     Open the input file.
        OPEN (UNIT=INPUT, FILE=TAPE_NAME, STATUS='OLD',
     *    ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *    READONLY, IOSTAT=IERR,
     *    RECORDTYPE='VARIABLE', RECL=MAXBLK, BUFFERCOUNT=2)
C
      IF (IERR.NE.0) THEN
        CALL PRINT_ERR (0)
      ELSE
C       Loop until double end of file or error or <cr> input.
          NFILE = 0
          NOTEOT = .TRUE.
          DO WHILE (NOTEOT)
C
            NOTEOF = .TRUE.
            DO WHILE (NOTEOF)
C             Read first line of input file.
                READ (INPUT, '(Q, A)', IOSTAT=IERR) NBYT, BLK(1:NBYT)
              IF (IERR.LT.0) THEN
C               Eot
                  NOTEOF = .FALSE.
                  NOTEOT = .FALSE.
              ELSE IF (IERR.GT.0) THEN
C               Tape read error.
                  NOTEOF = .FALSE.
                  NOTEOT = .FALSE.
                  CALL PRINT_ERR (0)
              ELSE
C               Good read.
C                 Print 1st line of file for users.
                     LENGTH = MIN (NBYT, N70)
                     NFILE = NFILE + 1
                     WRITE (CRT, '(I4,3X,A70)') NFILE, BLK(1:LENGTH)
                  DISK_FILE = INPUT_CHAR
     *              ('ENTER DISK FILE NAME (<cr>=EXIT, $=SKIP)')
                  IF (DISK_FILE.EQ.' ') THEN
                    NOTEOF = .FALSE.
                    NOTEOT = .FALSE.
C
                  ELSE
                    SKIP = (DISK_FILE .EQ. '$')
                    IF (.NOT.SKIP) THEN
C                     See if we need to add default extension.
                        IFIND = INDEX (DISK_FILE, '.')
                        IF (IFIND.LE.0) THEN
C                         Add '.FOR' to end of DISK_FILE.
                            IFIND = INDEX (DISK_FILE, ' ') - 1
                            DISK_FILE = DISK_FILE(1:IFIND) // '.FOR'
                          END IF
                      OPEN (UNIT=OUTPUT, FILE=DISK_FILE, STATUS='NEW',
     *                  ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *                  CARRIAGECONTROL='LIST', IOSTAT=IERR,
     *                  RECORDTYPE='VARIABLE', RECL=MAXBLK,
     *                  BUFFERCOUNT=2)
                      END IF
C
C                   Write to disk file.
                      DO WHILE (NOTEOF)
                        IF (.NOT.SKIP) THEN
                          NBYT = ITRIM (BLK, NBYT)
                          WRITE (OUTPUT, '(A)', IOSTAT=IERR) BLK(1:NBYT)
                          END IF
                        IF (IERR.GT.0) THEN
C                         Tape write error.
                            NOTEOT = .FALSE.
                            NOTEOF = .FALSE.
                            CALL PRINT_ERR (0)
                            CLOSE (UNIT=OUTPUT)
                        ELSE IF (IERR.LT.0) THEN
C                         End of file
                            CLOSE (UNIT=OUTPUT)
                            NOTEOF = .FALSE.
                        ELSE
                          READ (INPUT, '(Q, A)', IOSTAT=IERR)
     *                      NBYT, BLK(1:NBYT)
                          IF (IERR.GT.0) THEN
C                           Tape read error.
                              NOTEOT = .FALSE.
                              NOTEOF = .FALSE.
                              CALL PRINT_ERR (0)
                              CLOSE (UNIT=OUTPUT)
                          ELSE IF (IERR.LT.0) THEN
C                           End of file
                              CLOSE (UNIT=OUTPUT)
                              NOTEOF = .FALSE.
                            END IF
                          END IF
                      END DO
                    END IF
                END IF
              END DO
            END DO
          CLOSE (UNIT=INPUT)
        END IF
C
      RETURN
      END
      SUBROUTINE DUMP (BLK, INPUT, IO_DEVICE)
C-----------------------------------------------------------------------
C   DUMP will read variable length record blocks from an open input
C   file or device and print an hex and ASCII line for the 1st line
C   in each file.  The file will be closed on exit.
C   INPUTS:
C     BLK       C(*)   A large working buffer.
C     INPUT     I      Fortran logical unit no. of an open input file.
C     IO_DEVICE C*64(2) Input device name in 1st element.
C     DISK      L*1    .TRUE. if the output file is on disk, .FALSE. if
C                      the output device is a tape drive.
C-----------------------------------------------------------------------
      INTEGER   CRT, INPUT, OUTPUT, IERR, MAXBLK, ILEN,
     *  NBLKS, NBYTMN, NBYTMX, NBYT, MAX_FILE, N62
      CHARACTER*(*) BLK
      CHARACTER*64 IO_DEVICE(2)
      LOGICAL*1 DISK, EOF
      DATA CRT,N62 /6,62/
C-----------------------------------------------------------------------
C
      MAXBLK = LEN (BLK)
C
C     Open the input file.
        OPEN (UNIT=INPUT, FILE=IO_DEVICE(1), STATUS='OLD',
     *    ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *    READONLY, IOSTAT=IERR,
     *    RECORDTYPE='VARIABLE', RECL=MAXBLK, BUFFERCOUNT=2)
C
      IF (IERR.NE.0) THEN
C       Open error.
          CALL PRINT_ERR (0)
      ELSE
C
C       Loop until double end of file.
          NFILE = 1
          NBLKS = 999
          MAX_FILE = INPUT_I2 ('ENTER NO. OF FILES, <CR>=ALL',0,32767)
          IF (MAX_FILE.EQ.0) MAX_FILE = 32767
          DO WHILE ((NBLKS.NE.0) .AND. (NFILE.LE.MAX_FILE))
            NBLKS = 0
            NBYTMN = MAXBLK
            NBYTMX = 0
C
C           Loop until end of file.
              EOF = .FALSE.
              DO WHILE (.NOT.EOF)
C               Read input file.
                  READ (INPUT, '(Q, A)', IOSTAT=IERR) NBYT, BLK(1:NBYT)
                IF (IERR.EQ.0) THEN
C
                  NBLKS = NBLKS + 1
                  IF (NBLKS.EQ.1) THEN
                    ILEN = MIN (NBYT, N62)
                    WRITE (CRT, '('' FILE:'',I5,A62)')
     *                NFILE, BLK(1:ILEN)
                    END IF
                ELSE IF (IERR.LT.0) THEN
C                 End of file found.
                    EOF = .TRUE.
                ELSE
C
C                 Read error.
                    CALL PRINT_ERR (0)
                    EOF = .TRUE.
C
                  END IF
                END DO
C
C
C           End of file.
              NFILE = NFILE + 1
            END DO
C
        IF (NBLKS.EQ.0) WRITE
     *    (CRT, '('' Double tapemark = end of information.'')' )
        CLOSE (UNIT=INPUT)
        END IF
      RETURN
      END
      SUBROUTINE REW (TAPE_NAME, ILUN)
C-----------------------------------------------------------------------
C   Prompt for input or output.  Open, rewind, close.
C   INPUT:
C     IO_DEVICE   C*64(2)  Names for input and output devices.
C-----------------------------------------------------------------------
      INTEGER   ILUN
      CHARACTER*64 TAPE_NAME
C-----------------------------------------------------------------------
C
      OPEN (UNIT=ILUN, FILE=TAPE_NAME, STATUS='OLD',
     *  ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *  READONLY, IOSTAT=IERR,
     *  RECORDTYPE='VARIABLE', RECL=32000)
      IF (IERR.EQ.0) THEN
        REWIND (UNIT=ILUN)
        CLOSE (UNIT=ILUN)
        END IF
      RETURN
      END
      FUNCTION INPUT_I2 (PROMPT, MIN_VALUE, MAX_VALUE)
C-----------------------------------------------------------------------
C  INPUT_I2 will print a prompt at a given line on a Visual 400 and
C  accept an integer*2 input.  If this value is between the range
C  -1 (standard 'return' option) and MAX_VALUE the input value will be
C  returned, else an error message is printed, the prompt is reprinted
C  and another value is accepted.
C  INPUTS
C     PROMPT       C*80 the prompt message.
C     MIN_VALUE    I   the minimum acceptable value.
C     MAX_VALUE    I   the maximum acceptable value.
C-----------------------------------------------------------------------
      INTEGER   ERROR, INPUT_I2, MAX_VALUE, MIN_VALUE, REST
      CHARACTER*(*) PROMPT
C-----------------------------------------------------------------------
C     Loop until user gets it right.
         ERROR = -1
         DO WHILE (ERROR.NE.0)
C
C           Write prompt.
               WRITE (6,1000) PROMPT
C
C           Accept response.
               READ (5,1010,IOSTAT=ERROR) INPUT_I2
C
C           Verify value.
               IF ((INPUT_I2.LT.MIN_VALUE) .OR.
     *            (INPUT_I2.GT.MAX_VALUE)) THEN
                   ERROR = 1
                   WRITE (6,1020) MIN_VALUE, MAX_VALUE
               ELSE IF (ERROR.NE.0) THEN
                  CALL PRINT_ERR (0)
                  END IF
C
             END DO
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X, A<LEN(PROMPT)>, '  ', $)
 1005 FORMAT (1X, A<REST>, $)
 1007 FORMAT (1X, A35, $)
 1010 FORMAT (I6)
 1020 FORMAT (' ** Input error. Range is', 2I6)
      END
      FUNCTION INPUT_CHAR (PROMPT)
C-----------------------------------------------------------------------
C   This function will print a prompt at a given line on the CRT and
C   accept a CHARACTER*64 input.
C   INPUTS
C      PROMPT      C*? The prompt message.
C   OUTPUT
C      INPUT_CHAR  C*64 The response typed in by the user.
C-----------------------------------------------------------------------
      CHARACTER*64 INPUT_CHAR
      CHARACTER*(*) PROMPT
C-----------------------------------------------------------------------
C
C     Write prompt.
         WRITE (6, 1000) PROMPT
C
C     Accept response.
         READ (5, 1010) INPUT_CHAR
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (1X, A<LEN(PROMPT)>, '  ', $)
 1010 FORMAT (A64)
      END
      FUNCTION ITRIM (STRING, LEN)
C-----------------------------------------------------------------------
C  This routine will return the length of a string if all trailing
C  blanks and nulls are dropped.
C-----------------------------------------------------------------------
      INTEGER   LEN, ITRIM
      CHARACTER*(*) STRING
      CHARACTER*1 NULL
C-----------------------------------------------------------------------
      NULL = CHAR(0)
      ITRIM = LEN
      DO WHILE ( ((STRING(ITRIM:ITRIM).EQ.' ') .OR.
     *  (STRING(ITRIM:ITRIM).EQ.NULL)) .AND.
     *  (ITRIM.GT.0) )
        ITRIM = ITRIM - 1
        END DO
      RETURN
      END
      SUBROUTINE LCOPY (BLK, INPUT, OUTPUT, TAPE_NAME)
C-----------------------------------------------------------------------
C  LCOPY will find a list of disk files in a text file and copy these
C  disk source files (80 byte records) to tape.  An end of file will
C  be written on tape after each selected file. A double end of file
C  will be written on the tape at the end of all files.
C   INPUTS:
C     BLK       C(*)   A large working buffer.
C     INPUT     I      Fortran logical unit no. of an open input file.
C     OUTPUT    I      Fortran logical unit no. of an open output file.
C     TAPE_NAME C*64   Device name of the mounted tape.
C-----------------------------------------------------------------------
      INTEGER   INPUT, OUTPUT, IERR, NBYT, INPUT_I2, CRT, IFIND
      CHARACTER*64 TAPE_NAME, LIST_FILE, INPUT_CHAR
      CHARACTER*132 DISK_FILE0, DISK_FILE
      CHARACTER*(*) BLK
      LOGICAL   EOL, OK
      DATA CRT /6/, FLLUN /15/
C-----------------------------------------------------------------------
C
      WRITE (CRT, '(//)')
C     Prompt for text file with list of disk file names.
        OK = .FALSE.
        DO WHILE (.NOT.OK)
          LIST_FILE = INPUT_CHAR (
     *      'ENTER NAME OF FILE CONTAINING LIST. <CR>=EXIT')
          IF (LIST_FILE.EQ.' ') THEN
            EOL = .TRUE.
            OK = .TRUE.
          ELSE
C           Open the list file.
              OPEN (UNIT=FLLUN, FILE=LIST_FILE, STATUS='OLD',
     *          ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *          READONLY, IOSTAT=IERR,
     *          RECORDTYPE='VARIABLE', RECL=132, BUFFERCOUNT=2)
              IF (IERR.NE.0) THEN
                CALL PRINT_ERR (0)
              ELSE
                EOL = .FALSE.
                OK = .TRUE.
                END IF
              END IF
          END DO
C     Loop until end of file on list file is found.
        DO WHILE (.NOT.EOL)
          READ (FLLUN, '(A132)', IOSTAT=IERR) DISK_FILE0
          IF (IERR.NE.0) THEN
             CALL PRINT_ERR (0)
             EOL = .TRUE.
             CLOSE (UNIT=FLLUN)
          ELSE
C           Erase any version number.
              IFIND = INDEX (DISK_FILE0, ';') - 1
              IF (IFIND.GE.0) THEN
                DISK_FILE = DISK_FILE0(1:IFIND)
              ELSE
                DISK_FILE = DISK_FILE0
                END IF
C           See if we need to add default extension.
              IFIND = INDEX (DISK_FILE, '.')
              IF (IFIND.LE.0) THEN
C               Add '.FOR' to end of DISK_FILE.
                  IFIND = INDEX (DISK_FILE, ' ') - 1
                  DISK_FILE = DISK_FILE(1:IFIND) // '.FOR'
                END IF
C           Open the disk file.
              OPEN (UNIT=INPUT, FILE=DISK_FILE, STATUS='OLD',
     *          ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *          READONLY, IOSTAT=IERR,
     *         RECORDTYPE='VARIABLE', RECL=MAXBLK, BUFFERCOUNT=2)
            IF (IERR.NE.0) THEN
              CALL PRINT_ERR (0)
            ELSE
C             Open the tape file.
                OPEN (UNIT=OUTPUT, FILE=TAPE_NAME, STATUS='OLD',
     *            ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *            IOSTAT=IERR,
     *            RECORDTYPE='VARIABLE', RECL=80, BUFFERCOUNT=2)
                IF (IERR.NE.0) THEN
                   CALL PRINT_ERR (0)
                ELSE
C                 Copy disk to tape.
                    IERR = 0
                    DO WHILE (IERR.EQ.0)
                      READ (INPUT, '(Q, A)', IOSTAT=IERR)
     *                  NBYT, BLK(1:NBYT)
                      IF (IERR.EQ.0) THEN
C                       Pad with blanks if necessary.
                          IF (NBYT.LT.80) BLK(NBYT+1:80) = ' '
                        WRITE (OUTPUT, '(A)', IOSTAT=IERR) BLK(1:80)
                        IF (IERR.NE.0) CALL PRINT_ERR (0)
                        END IF
                      END DO
                  CLOSE (UNIT=INPUT)
C                 Close the tape file, implicitly writting an EOF.
                    CLOSE (UNIT=OUTPUT)
                  WRITE (CRT, '('' Finished '', A60)') DISK_FILE(1:60)
                  END IF
              END IF
            END IF
          END DO
C     See if user wants EOT mark.
        OPTION = INPUT_I2 (' 1 = WRITE END OF TAPE MARK, 0 = NO',0,1)
        IF (OPTION.EQ.1) THEN
          WRITE (CRT, '(1X,''Writing end of tape'')' )
          OPEN (UNIT=OUTPUT, FILE=TAPE_NAME, STATUS='OLD',
     *      ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *      IOSTAT=IERR,
     *      RECORDTYPE='VARIABLE', RECL=80, BUFFERCOUNT=2)
          CLOSE (UNIT=OUTPUT)
          END IF
      RETURN
      END
      SUBROUTINE PRINT_ERR (IERR)
C-----------------------------------------------------------------------
C  This routine will find and print the last error status, or print
C  a given system service error status.
C  INPUT:
C    IERR   I   0 means find last FORTRAN error, otherwise decode this
C           system service error.
C-----------------------------------------------------------------------
      INTEGER   IERR, FORERR, RMSERR
      INTEGER   MSGLEN
      CHARACTER*64 MESSAGE
C-----------------------------------------------------------------------
C
C     Get error number.
        RMSERR = IERR
        IF (RMSERR.EQ.0) CALL ERRSNS (FORERR, RMSERR)
C
C     Get error message.
        MESSAGE = ' '
        MSGLEN = 64
        CALL SYS$GETMSG (%VAL(RMSERR), MSGLEN, MESSAGE, %VAL(1), )
C     Print error.
        WRITE (6,1000) MESSAGE
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT(1X, A64)
      END

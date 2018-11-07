LOCAL INCLUDE 'MK3TX.INC'
C                                       Local include for MK3TX
C
C                                       Maximum number of text files to
C                                       index (should be a multiple of
C                                       5):
      INTEGER    MAXTFL
      PARAMETER (MAXTFL = 100)
C                                       NUMTFL is the number of text
C                                       files on the tape (0 <= NUMTFL
C                                       <= MAXTFL); FNAME(1:NUMTFL)
C                                       holds the names of the text
C                                       files and FNUM(I) holds the
C                                       file number of the file with
C                                       name FNAME(I) (1 <= I <= NUMTL).
      INTEGER   NUMTFL, FNUM(MAXTFL)
      CHARACTER FNAME(MAXTFL)*6
C                                       CURFIL is the number of the
C                                       current file on the tape.
      INTEGER   CURFIL
C                                       LOGREC is the logical record
C                                       size for the archive tape in
C                                       8-bit bytes, BLKFAC is the
C                                       blocking factor (number of
C                                       logical records per tape block),
C                                       TBUFSI is the size of the tape
C                                       buffer in words and TBUFF is the
C                                       tape buffer.  TBIND is the index
C                                       of the next logical record to be
C                                       read from TBUFF and FDVEC is the
C                                       file descriptor vector used by
C                                       TAPIO (see the header comments
C                                       for TAPIO for details).
      INTEGER   LOGREC, BLKFAC, TBUFSI
      PARAMETER (LOGREC = 256)
      PARAMETER (BLKFAC = 16)
      PARAMETER (TBUFSI = LOGREC * BLKFAC / 4)
      INTEGER   TBUFF(TBUFSI), TBIND, FDVEC(50)
C                                       TTY contains the LUN and FTAB
C                                       index for the terminal:
      INTEGER   TTY(2)
C                                       TAPEOP is true iff the tape is
C                                       opened, TERMOP is true iff the
C                                       terminal is opened.
      LOGICAL   TAPEOP, TERMOP
C
      COMMON /M3TPAR/ NUMTFL, FNUM, CURFIL, TBUFF, TBIND, FDVEC, TAPEOP,
     *   TERMOP, TTY
      COMMON /M3TCHR/ FNAME
C                                       End local include MK3TX
LOCAL END
      PROGRAM MK3TX
C-----------------------------------------------------------------------
C! Extract text files from a Haystack MkIII archive tape
C# VLB Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   Interactively extract text files from a Haystack MkIII VLBI archive
C   ("A" or "B") tape.
C
C   Inputs from AIPS
C   ================
C
C   Adverb       Program name(s)          Description
C   INTAPE       XINTAP    R              Tape drive to read from
C                INTAPE    I
C-----------------------------------------------------------------------
      INTEGER   IERR, SCRTCH(256)
C                                       Program name.
      CHARACTER PRGM*6
      PARAMETER (PRGM = 'MK3TX ')
      INCLUDE 'MK3TX.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Initialize AIPS commons and read
C                                       input adverbs:
      CALL M3TINI (PRGM, IERR)
      IF (IERR.EQ.0) THEN
C                                       Read index file from tape:
         CALL M3TIND (IERR)
         IF (IERR.EQ.0) THEN
C                                       Extract files from tape:
            CALL M3TRD (IERR)
            END IF
         END IF
C                                       Close down and restart AIPS:
      IF (TAPEOP) CALL TAPIO ('CLOS', FDVEC, TBUFF, TBIND, IERR)
      IF (TERMOP) CALL ZCLOSE (TTY(1), TTY(2), IERR)
C
      CALL DIE (IERR, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE M3TINI (PRGM, IRET)
C-----------------------------------------------------------------------
C   Initialize AIPS commons and read adverbs.  Also checks that tape
C   buffer is large enough to handle a tape block initializes the file
C   descriptor vector for TAPIO, opens the tape and positions the read
C   head at the start of the first file.
C
C   Inputs:
C     PRGM        C*6            Program name
C
C   Outputs:
C     IRET        I              Return code: 0 -> success
C                                             1 -> error reading adverbs
C                                             2 -> user error
C                                             3 -> tape buffer too small
C                                             4 -> tape error
C                                             5 -> terminal error
C
C   Inputs in common:
C     NTAPED      I              Number of tape drives available
C
C   Outputs in common:
C     NSCR        I              Number of scratch files open (0)
C     NCFILE      I              Number of catalogued files open (0)
C     RQUICK      L              .FALSE. (do not release POPS until
C                                finished)
C     FDVEC       I(50)          File descriptor vector for TAPIO
C     CURFIL      I              Current file on tape (1)
C     TTY         I(2)           LUN and FTAB index for terminal
C     TAPEOP      L              True iff tape is opened
C     TERMOP      L              True iff terminal is opened
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      INTEGER   IERR, INTAPE, SCRTCH(256), I
      REAL      XINTAP
      CHARACTER BUFF*80
      INCLUDE 'MK3TX.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Initialize device
C                                       characteristics common:
      CALL ZDCHIN (.TRUE., SCRTCH)
C                                       Initialize files common:
      NSCR = 0
      NCFILE = 0
      CALL FILL (10, 0, IBAD)
C
      CALL FILL (50, 0, FDVEC)
      TAPEOP = .FALSE.
      TERMOP = .FALSE.
      TTY(1) = 5
C                                       Get input adverbs and set RQUICK
C                                       to .FALSE. to indicate that AIPS
C                                       has not been restarted:
      CALL GTPARM (PRGM, 1, RQUICK, XINTAP, SCRTCH, IERR)
      RQUICK = .FALSE.
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1000)
         CALL MSGWRT (8)
         GO TO 999
         END IF
      INTAPE = NINT (XINTAP)
C                                       Check tape buffer size (should
C                                       be large enough if NBITWD >=
C                                       32):
      IF (TBUFSI.LT.(LOGREC * BLKFAC * 8 / NBITWD)) THEN
         IRET = 3
         WRITE (MSGTXT, 1001) TBUFSI * NBITWD / 8
         CALL MSGWRT (8)
         WRITE (MSGTXT, 1002)
         CALL MSGWRT (8)
         GO TO 999
         END IF

C                                       Set default value of INTAPE if
C                                       required:
      IF (INTAPE.EQ.0) THEN
         INTAPE = 1
         WRITE (MSGTXT, 1003) INTAPE
         CALL MSGWRT (4)
         END IF
C                                       Check for invalid INTAPE:
      IF ((INTAPE.LT.1) .OR. (INTAPE.GT.NTAPED)) THEN
         IRET = 2
         WRITE (MSGTXT, 1004) INTAPE
         CALL MSGWRT (8)
         WRITE (MSGTXT, 1005) NTAPED
         GO TO 999
         END IF
C                                       Initialize FDVEC:
C                                         tape LUN
      FDVEC(1) = 129 - INTAPE
C                                         logical record size
      FDVEC(2) = LOGREC
C                                         buffer size
      FDVEC(3) = TBUFSI * NBITWD / 8
C                                         tape number
      FDVEC(5) = INTAPE
C                                         blocking factor
      FDVEC(6) = BLKFAC
C                                       Open tape and set position to
C                                       beginning of tape:
      CALL TAPIO ('OPRD', FDVEC, TBUFF, TBIND, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT, 1006) INTAPE
         CALL MSGWRT (8)
         WRITE (MSGTXT, 1007) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      TAPEOP = .TRUE.
      I = 1
      CALL ZTAPE ('REWI', FDVEC(1), FDVEC(40), I, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.5)) THEN
         IRET = 4
         WRITE (MSGTXT, 1008)
         CALL MSGWRT (8)
         WRITE (MSGTXT, 1009) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Establish the definition of
C                                       CURFIL:
      CURFIL = 1
C                                       Open terminal:
      CALL ZOPEN (TTY(1), TTY(2), 1, BUFF, .FALSE., .TRUE., .TRUE.,
     *   IERR)
      IF (IERR.NE.0) THEN
         IRET = 5
         WRITE (MSGTXT, 1010) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      TERMOP = .TRUE.
C
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('M3TINI: ERROR', I2, ' READING INPUTS')
 1001 FORMAT ('TAPE BUFFER SIZE ', I4, ' BYTES IS TOO SMALL')
 1002 FORMAT ('SEE YOUR AIPS MANAGER')
 1003 FORMAT ('Reading from tape drive ', I2)
 1004 FORMAT ('INVALID TAPE DRIVE NUMBER ', I2)
 1005 FORMAT ('USE VALUES OF INTAPE BETWEEN 1 AND ', I2, ' INCLUSIVE')
 1006 FORMAT ('CANNOT OPEN TAPE ', I2, ' FOR READING')
 1007 FORMAT ('M3TINI: TAPIO ERROR', I2)
 1008 FORMAT ('ERROR POSITIONING TAPE')
 1009 FORMAT ('M3TINI: ZTAPE ERROR', I2)
 1010 FORMAT ('M3TINI: ZOPEN ERROR', I2, ' OPENING TERMINAL')
      END
      SUBROUTINE M3TIND (IRET)
C-----------------------------------------------------------------------
C   Read tape file index.  Performs some crude format checking.
C   Outputs:
C      IRET     I        Return status: 0 -> success
C                           1 -> tape error
C                           2 -> bad tape
C                           3 -> buffer overflow
C                           4 -> no text files
C   Outputs in common:
C      NUMTFL   I        Number of text files found
C      FNAME    C(*)*6   A list of text file names
C      FNUM     I(*)     A list of text file locations
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IERR, IFILE, IPOS, I
      CHARACTER TITLE*32, TNAME*6
      INCLUDE 'MK3TX.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Clear file names:
      DO 10 I = 1, MAXTFL
         FNAME(I) = '      '
   10    CONTINUE
C                                       Read index file header:
      CALL TAPIO  ('READ', FDVEC, TBUFF, TBIND, IERR)
      IF (IRET.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Extract title:
      CALL ZC8CL (32, 81, TBUFF(TBIND), TITLE)
C
      WRITE (MSGTXT, 1001)
      CALL MSGWRT (4)
      WRITE (MSGTXT, 1002) TITLE
      CALL MSGWRT (4)
C                                       Reinitialize I/O and skip over
C                                       header (this is necessary
C                                       because the short header block
C                                       confuses TAPIO):
      CALL TAPIO ('BAKF', FDVEC, TBUFF, TBIND, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      I = 1
      CALL ZTAPE ('ADVR', FDVEC(1), FDVEC(40), I, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1003) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Read in first data record:
      CALL TAPIO ('READ', FDVEC, TBUFF, TBIND, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.10)) THEN
         IRET = 1
         WRITE (MSGTXT, 1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       The tape index is organised as
C                                       16-byte cards.  The first 6
C                                       bytes hold the file name and
C                                       most text file names begin with
C                                       a '#' while log files begin with
C                                       a '['. IPOS is the character
C                                       position of the current card and
C                                       IFILE is the file position of
C                                       the file described in that card.
C                                       Files 1 and 2 are index files.
      NUMTFL = 0
      IPOS = 1
      IFILE = 3
      IERR = 0
 100  IF (IERR.NE.4) THEN
         CALL ZC8CL (6, IPOS, TBUFF(TBIND), TNAME)
         IF ((TNAME(1:1).EQ.'#') .OR. (TNAME(1:1).EQ.'[')) THEN
            NUMTFL = NUMTFL + 1
            IF (NUMTFL.GT.MAXTFL) THEN
               IRET = 3
               WRITE (MSGTXT, 1100) MAXTFL
               CALL MSGWRT (8)
               WRITE (MSGTXT, 1101)
               CALL MSGWRT (8)
               GO TO 999
               END IF
            FNAME(NUMTFL) = TNAME
            FNUM(NUMTFL)  = IFILE
            END IF
         IFILE = IFILE + 1
         IPOS = IPOS + 16
         IF (IPOS.GT.LOGREC) THEN
            CALL TAPIO ('READ', FDVEC, TBUFF, TBIND, IERR)
            IF ((IERR.NE.0) .AND. (IERR.NE.4) .AND. (IERR.NE.10)) THEN
               IRET = 1
               WRITE (MSGTXT, 1000) IERR
               CALL MSGWRT (8)
               GO TO 999
               END IF
            IPOS = 1
            END IF
         GO TO 100
         END IF
C                                       Check that some files were
C                                       found:
      IF (NUMTFL.EQ.0) THEN
         IRET = 4
         WRITE (MSGTXT, 1102)
         CALL MSGWRT (6)
         GO TO 999
         END IF
      CURFIL = CURFIL + 1
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('M3TIND: ERROR ', I1, ' READING TAPE')
 1001 FORMAT ('Found index file')
 1002 FORMAT ('Archive title: ', A32)
 1003 FORMAT ('M3TIND: ERROR ', I1, ' ADVANCING TAPE')
 1100 FORMAT ('MAXIMUM NUMBER OF TEXT FILES (', I2, ') EXCEEDED')
 1101 FORMAT ('SEE YOUR AIPS MANAGER')
 1102 FORMAT ('THERE DO NOT APPEAR TO BE ANY TEXT FILES ON THIS TAPE')
      END
      SUBROUTINE M3TRD (IRET)
C-----------------------------------------------------------------------
C   Read text files from a MkIII archive tape.  Displays a menu of text
C   files and allows the user to select a file until the user terminates
C   by requesting file number zero.
C   Outputs:
C     IRET     I   Return code: 0 -> success
C                               1 -> I/O error
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   FILNUM, IERR
      INTEGER   TXMENU
      EXTERNAL  TXMENU
C-----------------------------------------------------------------------
C                                       Display menu and get number of
C                                       file to extract:
      FILNUM = TXMENU (IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         GO TO 999
         END IF
C                                       Extract requested file and
C                                       repeat menu selection until user
C                                       requests termination.
 10   IF (FILNUM.NE.0) THEN
         CALL EXTRAC (FILNUM, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            GO TO 999
            END IF
         FILNUM = TXMENU (IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            GO TO 999
            END IF
         GO TO 10
         END IF
C
      IRET = 0
C
 999  RETURN
      END
      INTEGER FUNCTION TXMENU (IRET)
C-----------------------------------------------------------------------
C   Display a menu of the files listed in FNAME and allow the user to
C   select a file number (corresponding to the index of the file in
C   FNAME) or 0 to exit.  The menu and prompt is repeated until the user
C   gives a valid response.
C   Output:
C      IRET     I        Return status: 0 -> success
C                                       1 -> I/O error
C   Return value:
C      TXMENU   I        Index of file selected or zero if termination
C                        requested
C   Inputs in common:
C      NUMTFL   I        Number of entries in FNAME
C      FNAME    C(*)*6   List of file names
C      TTY      I(2)     LUN and FTAB index for terminal
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, J, IERR
      CHARACTER LINBUF*72
      INTEGER   NCOL, NROW
      PARAMETER (NCOL = 4)
C
      INCLUDE 'MK3TX.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Calculate number of rows in
C                                       menu:
      IF (MOD(NUMTFL, NCOL).EQ.0) THEN
         NROW = NUMTFL / NCOL
      ELSE
         NROW = NUMTFL / NCOL + 1
         END IF
C                                       Display file menu and prompt for
C                                       file number until the user gives
C                                       a valid response (between 0 and
C                                       NUMTFL inclusive):
      I = -1
 10   IF ((I.LT.0) .OR. (I.GT.NUMTFL)) THEN
         LINBUF = ' '
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 1, LINBUF, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT, 1010) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 1, LINBUF, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT, 1010) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         LINBUF = 'Text files on tape:'
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 20, LINBUF, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT, 1010) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         LINBUF = ' '
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 1, LINBUF, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT, 1010) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         DO 20 I = 1, NROW
            WRITE (LINBUF, 1020) (J, FNAME(J), J=I, I+(NCOL-1)*NROW,
     *         NROW)
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, LINBUF, IERR)
            IF (IERR.NE.0) THEN
               IRET = 1
               WRITE (MSGTXT, 1010) IERR
               CALL MSGWRT (8)
               GO TO 999
               END IF
 20         CONTINUE
         LINBUF = ' '
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 1, LINBUF, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT, 1010) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         LINBUF = 'Enter number of file to extract (or 0 to exit): '
         CALL INQINT (TTY, LINBUF, 1, I, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT, 1021) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         LINBUF = ' '
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 1, LINBUF, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT, 1010) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         GO TO 10
         END IF
C
      IRET = 0
C
 999  TXMENU = I
      RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TXMENU: ERROR ', I2, ' WRITING TO TERMINAL')
 1020 FORMAT (1X, I3, ' -- ', A6, 3(3X, I3, ' -- ', A6))
 1021 FORMAT ('TXMENU: ERROR ', I2, ' FROM INQINT')
      END
      SUBROUTINE EXTRAC (FILNUM, IRET)
C-----------------------------------------------------------------------
C   Extract file number FILNUM from tape.  Position the tape at the
C   beginning of the file, request an output file name, open the output
C   file and copy the file from the tape to the output file, converting
C   to local text format.
C
C   Inputs:
C     FILNUM      I      Index of file in FNAME and FNUM
C
C   Outputs:
C     IRET        I      Return status: 0 -> success
C                                       1 -> I/O error
C
C   Inputs in common:
C     CURFIL      I      Current tape position
C     FNAME       I(*)   List of file names
C     FNUM        I(*)   Tape position of files in FNAME
C-----------------------------------------------------------------------
      INTEGER   FILNUM, IRET
C
      INTEGER   LINMAX
      PARAMETER (LINMAX = 256)
      INTEGER   CHARRD, IERR, ILINE, LINLEN, NFIL, POS, TXTLUN, TXTIND,
     *   I
      CHARACTER ANSWER, LINBUF*(LINMAX), OUTFIL*48, PROMPT*78, TNAME*6
      INCLUDE 'MK3TX.INC'
      INCLUDE 'INCS:DMSG.INC'
      PARAMETER (TXTLUN = 10)
C-----------------------------------------------------------------------
C                                       Position tape at beginning of
C                                       file:
      IERR = 0
      NFIL = FNUM(FILNUM) - CURFIL
      IF (NFIL.GT.0) THEN
         CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), NFIL, IERR)
      ELSE
         I = ABS (NFIL - 1)
         CALL ZTAPE ('BAKF', FDVEC(1), FDVEC(40), I, IERR)
         END IF
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1000)
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CURFIL = FNUM(FILNUM)
C                                       Read header and verify name:
      CALL TAPIO ('READ', FDVEC, TBUFF, TBIND, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.10)) THEN
         IRET = 1
         WRITE (MSGTXT, 1001) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IERR = 0
      CALL ZC8CL (6, 1, TBUFF(TBIND), TNAME)
      IF (TNAME.EQ.FNAME(FILNUM)) THEN
         WRITE (MSGTXT, 1002) FNAME(FILNUM)
         CALL MSGWRT (4)
      ELSE
         PROMPT = 'File header does not match requested file name.'
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 80, PROMPT, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT, 1003) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         PROMPT = 'Extract it anyway (Y/N)?'
         CALL INQSTR (TTY, PROMPT, 48, ANSWER, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT, 1003) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         IF ((ANSWER.NE.'Y') .AND. (ANSWER.NE.'y')) THEN
            WRITE (MSGTXT, 1004)
            CALL MSGWRT (4)
            GO TO 999
            END IF
         END IF
C                                       Get output file name and open
C                                       output file:
      PROMPT = 'Enter output file name:'
      CALL INQSTR (TTY, PROMPT, 48, OUTFIL, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1003) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL ZTXOPN ('WRIT', TXTLUN, TXTIND, OUTFIL, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         GO TO 999
         END IF
C                                       Reinitialize I/O and skip over
C                                       header:
      CALL TAPIO ('BAKF', FDVEC, TBUFF, TBIND, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1001) IERR
         CALL MSGWRT (8)
         GO TO 990
         END IF
      I = 1
      CALL ZTAPE ('ADVR', FDVEC(1), FDVEC(40), I, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1000) IERR
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Read lines from HP file. Each
C                                       line begins and ends with a
C                                       2-byte record length (in 16-bit
C                                       words). In some cases the high-
C                                       order bit of this quantity is
C                                       set.  Examination has shown that
C                                       in such cases masking out the
C                                       high-order bit gives the length
C                                       of the line in 16-bit words
C                                       minus 1.  The line length in
C                                       16-bit words can therefore be
C                                       recovered by adding 32769.
C                                       Small negative numbers denote
C                                       the end of the file.  The break
C                                       between large and small negative
C                                       numbers is arbitrarily assumed
C                                       to be -64.
C
C                                       ILINE is the number of  lines
C                                       read, LINLEN is the length of
C                                       the current line, CHARRD is
C                                       the number of characters read
C                                       from the current line and POS is
C                                       the current character position
C                                       in the input buffer.
      POS = 1
      ILINE = 0
      CHARRD = 0
      CALL TAPIO ('READ', FDVEC, TBUFF, TBIND, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.10)) THEN
         IRET = 1
         WRITE (MSGTXT, 1005) IERR
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Get length of first line in
C                                       characters:
      CALL ZI16IL (1, (POS/2 + 1), TBUFF(TBIND), LINLEN)
      IF (LINLEN.LT.-64) LINLEN = LINLEN + 32769
      LINLEN = 2 * LINLEN
      POS = POS + 2
C                                       Read lines until end of file:
 10   IF (LINLEN.GE.0) THEN
         IF (LINLEN.GT.LINMAX) THEN
            IRET = 3
            WRITE (MSGTXT, 1006) LINLEN, LINMAX
            CALL MSGWRT (8)
            GO TO 990
            END IF
C                                       Read 256-byte records as
C                                       necessary until line is
C                                       complete:
 20      IF (CHARRD.NE.LINLEN) THEN
            IF ((LINLEN - CHARRD) .LE. (LOGREC - POS + 1)) THEN
                CALL ZC8CL (LINLEN - CHARRD, POS, TBUFF(TBIND),
     *             LINBUF(CHARRD+1:))
                POS = POS + LINLEN - CHARRD
                CHARRD = LINLEN
             ELSE
                CALL ZC8CL (LOGREC - POS + 1, POS, TBUFF(TBIND),
     *             LINBUF(CHARRD+1:))
                CHARRD = CHARRD + LOGREC - POS + 1
                POS = LOGREC + 1
                END IF
             IF (POS.GT.LOGREC) THEN
                CALL TAPIO ('READ', FDVEC, TBUFF, TBIND, IERR)
                IF ((IERR.NE.0) .AND. (IERR.NE.10)) THEN
                   IRET = 1
                   WRITE (MSGTXT, 1001) IERR
                   CALL MSGWRT (8)
                   END IF
                POS = POS - LOGREC
                END IF
             GO TO 20
             END IF
C                                       Write out complete line:
          CALL ZTXIO ('WRIT', TXTLUN, TXTIND, LINBUF(1:LINLEN), IERR)
          IF (IERR.NE.0) THEN
             IRET = 1
             WRITE (MSGTXT, 1020) IERR
             CALL MSGWRT (8)
             GO TO 990
             END IF
          CHARRD = 0
          ILINE = ILINE + 1
C                                       Skip trailing count:
          POS = POS + 2
          IF (POS.GT.LOGREC) THEN
             CALL TAPIO ('READ', FDVEC, TBUFF, TBIND, IERR)
             IF ((IERR.NE.0) .AND. (IERR.NE.10)) THEN
                IRET = 1
                WRITE (MSGTXT, 1001) IERR
                CALL MSGWRT (8)
                GO TO 990
                END IF
             POS = POS - LOGREC
             END IF
C                                       Get next line length:
          CALL ZI16IL (1, (POS/2 + 1), TBUFF(TBIND), LINLEN)
          IF (LINLEN.LT.-64) LINLEN = LINLEN + 32769
          LINLEN = 2 * LINLEN
          POS = POS + 2
          IF (POS.GT.LOGREC) THEN
             CALL TAPIO ('READ', FDVEC, TBUFF, TBIND, IERR)
             IF ((IERR.NE.0) .AND. (IERR.NE.10)) THEN
                IRET = 1
                WRITE (MSGTXT, 1001) IERR
                CALL MSGWRT (8)
                GO TO 990
                END IF
             POS = POS - LOGREC
             END IF
          GO TO 10
          END IF
C                                       Summarize activity:
      WRITE (MSGTXT, 1021) ILINE, FNAME(FILNUM)
      CALL MSGWRT (4)
      IRET = 0
C                                       Close output file:
 990  CALL ZTXCLS (TXTLUN, TXTIND, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EXTRAC: ERROR ', I2, ' POSITIONING TAPE')
 1001 FORMAT ('EXTRAC: ERROR ', I2, ' READING TAPE')
 1002 FORMAT ('Found ', A6)
 1003 FORMAT ('EXTRAC: INQSTR ERROR ',I2)
 1004 FORMAT ('Skipping file...')
 1005 FORMAT ('EXTRAC: ERROR ', I2, 'READING TAPE')
 1006 FORMAT ('LINE FOUND WITH ', I5, ' CHARACTERS (>', I5, ')')
 1020 FORMAT ('EXTRAC: ERROR ', I2, ' WRITING OUTPUT FILE')
 1021 FORMAT ('Extracted ', I4, ' lines from ', A6)
      END

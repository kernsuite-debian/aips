      PROGRAM IMPFIT
C-----------------------------------------------------------------------
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
C  Read an AIPS source tape written in compresses ascii FITS format via
C  the program EXPFIT.  IMPFIT requires three operating-system-specific
C  subroutines:
C
C     ZTAPIO.....performs tape opening, reading and closing
C
C     ZCRDIR.....creates directories as required
C
C     ZFLOPN.....creates and opens a Fortran-77 text file
C
C   The UNIX versions of these Z routines are appended to this file.
C   ZTAPIO and ZCRDIR are written in C and hence are appended as
C   comments to this Fortran file.
C
C   Since the Fortran code of IMPFIT has only been tested under UNIX, it
C   is not certain to be operating system independent at this time
C   although it is believed to be in compliance with the ANSI Fortran 77
C   standard.
C-----------------------------------------------------------------------
      INTEGER   CMD, FILCNT, IP, IERR, LD, LO, NFILES
      LOGICAL   LDIR, COPY, CRT, TPOPN, EOFLG
      CHARACTER TPNAM*24, ANSWR*5, CC*1, B(36)*80, S*256
      INTEGER   FD, IR, IC, IO, NBYTE
      COMMON /CTRLC/ B, S, TPNAM
      COMMON /CTRLI/ FD, IR, IC, IO, EOFLG
      DATA LD, LO, FILCNT /3,4,1/
      DATA TPOPN /.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize
      CMD = 0
      FD = -1
      IR = 999
      IC = 999
      IO = 0
      IERR = 0
      EOFLG = .FALSE.
C                                       Get input from terminal
 5    WRITE (6,'('' ENTER THE TAPE NAME'')')
      READ (5, '(A)') TPNAM
C                                       Do more: ask operation
 6    WRITE (6,'('' DO YOU WANT TO MAKE AN INDEX OF THE TAPE? (Y/N)'')')
      READ (5, '(A1)') ANSWR
      IF (ANSWR.EQ.'Q') GO TO 999
      LDIR = (ANSWR.EQ.'Y')
      WRITE (6, '('' DO YOU WANT TO COPY THE FILES TO DISK? (Y/N)'')')
      READ (5, '(A1)') ANSWR
      COPY = (ANSWR.EQ.'Y')
      WRITE (6,'('' DO YOU WANT FILENAMES LISTED ON THE CRT? (Y/N)'')')
      READ (5,'(A1)') ANSWR
      CRT = (ANSWR.EQ.'Y')
      WRITE (6, '('' ENTER THE NUMBER OF FILES TO PROCESS. (I3)'')')
      READ (5, '(I3)') NFILES
      WRITE (6,'('' TYPE   GO, RESTART, OR QUIT'')')
      READ (5,'(A)') ANSWR
      IF (ANSWR.EQ.'GO') GO TO 8
      IF (ANSWR.EQ.'QUIT') GO TO 999
      GO TO 5
C                                       DO it
C                                       Set terminal carriage control
C                                       character
 8    IF (CRT) THEN
         CC = '-'
      ELSE
         CC = ' '
         END IF
C                                       Open tape file
      IF (.NOT.TPOPN) CALL ZTAPIO ('OPEN', 24, TPNAM, FD, 2880, NBYTE,
     *   B, IERR)
      IF ((FD.LT.0) .OR. (IERR.NE.0)) GO TO 900
      TPOPN = .TRUE.
C                                       Open tape index output file
      IF (LDIR) THEN
         OPEN (UNIT=LD, FILE='IMPFIT.INX', STATUS='NEW', ERR=920)
         REWIND (LD)
         WRITE (6,*) 'TAPE INDEX FILE = IMPFIT.INX'
         END IF
C                                       Process the tape
 10   CONTINUE
         CALL GNXT (CMD, IP, IERR)
         CMD = 0
         IF (IERR.EQ.0) THEN
            IF (COPY) THEN
               IF (B(IR)(IC:IC) .EQ. '!') THEN
                  CALL SPCIAL()
               ELSE
                  CALL PNXT (0, B(IR)(IC:IC), 1)
                  END IF
               END IF
         ELSE IF (IERR.EQ.1) THEN
            CALL HDR (LDIR, COPY, CRT, CMD)
         ELSE IF (IERR.EQ.2) THEN
            CLOSE (LO)
            FILCNT = FILCNT + 1
            IF (FILCNT.GT.NFILES) GO TO 850
            IF (LDIR) WRITE (LD,'(''-FILE NUMBER '',I4)') FILCNT
         ELSE IF (IERR.EQ.3) THEN
            GO TO 800
         ELSE
            GO TO 950
            END IF
         GO TO 10
C                                       End of volume
 800  CALL ZTAPIO ('CLOS', 24, TPNAM, FD, 2880, NBYTE, B, IERR)
      WRITE (6, '('' END OF TAPE REACHED - PROGRAM ENDING'')')
      GO TO 999
C                                       Requested number of files
C                                       processed
 850  FILCNT = FILCNT - 1
      WRITE (6, '(1X,I4, '' FILES PROCESSED'')') FILCNT
      FILCNT = 1
      GO TO 6
C                                       Tape open error
 900  WRITE (6, '('' TAPE OPEN FAILED - PROGRAM ENDING'')')
      GO TO 999
C                                       Index file open error
 920  WRITE (6, '('' UNABLE TO OPEN LISTING FILE '')')
      GO TO 5
C                                       Tape i/o error
 950  WRITE (6, '('' TAPE I/O ERROR - PROGRAM ENDING'')')
C
 999  STOP
      END
      SUBROUTINE GDNA (STR, NBEG, NEND, LENTH, IERR)
C-----------------------------------------------------------------------
C  Find the end of the next part of the pathname contained in the string
C  "STR" assuming that NBEG is the character position of the first "/".
C  "NEND" will be the character position for the last valid character.
C       IERR:  0 - Valid name. Is not the last one.
C              1 - Valid name. Nothing follows.
C              2 - Error.
C-----------------------------------------------------------------------
      INTEGER NBEG, NEND, IERR, LENTH, ND1, ND2, ND, N
      CHARACTER STR*(*)
C-----------------------------------------------------------------------
C                                       Find the first delimiter (blank
C                                       or apostrophe)
      ND1 = INDEX (STR(NBEG+1:80), ' ')
      ND2 = INDEX (STR(NBEG+1:80), '''')
      ND = MIN (ND1, ND2)
      IF (ND.EQ.0) ND = ND1 + ND2
C
      N = INDEX (STR(NBEG+1:80), '/')
      IF ((N.GT.0) .AND. (N.LT.ND)) THEN
         NEND = NBEG + N - 1
         IERR = 0
      ELSE
         NEND = NBEG + ND - 1
         IERR = 1
         END IF
C
      LENTH = NEND - NBEG + 1
C
 999  RETURN
      END
      SUBROUTINE GNXT (CMD, IP, IERR)
C-----------------------------------------------------------------------
C   Set the column (IC) and the row (IR) of the next character in the
C   tape buffer, staying always in the data area (columns 9-80) except
C   for "control" lines, in which case IP points to column 1 of that
C   line. Use CMD=1 to go immeditaely to the next line (like after a
C   command if need be, read another tape block into the buffer).
C   Inputs:
C      CMD      INTEGER  0 get next char; 1 get next char in next line;
C                        2 read until EOF.
C      B        CHAR     Tape char buffer (in commons).
C   Outputs:
C      IP       INTEGER  Position (1:2880) of next useful character in
C                        the tape buffer (not used).
C      IC       INTEGER  In commons - Column number in current line.
C      IR       INTEGER  In commons - Record number of current line.
C      IERR     INTEGER  0 - char is good; 1 - first char of a control
C                        line; 2 - EOF; 3 - EOV; 4 - i/o error
C-----------------------------------------------------------------------
      INTEGER CMD, IP, IERR
      INTEGER FD, IR, IC, IO
      LOGICAL EOFLG
      CHARACTER B(36)*80, S*256, TPNAM*24
      COMMON /CTRLC/ B, S, TPNAM
      COMMON /CTRLI/ FD, IR, IC, IO, EOFLG
C-----------------------------------------------------------------------
      IF ((CMD.EQ.0) .AND. (IC.LT.80)) THEN
         IC = IC + 1
         IP = (IR - 1)*80 + IC
         IERR = 0
      ELSE IF (((CMD.EQ.0) .AND. (IC.GE.80)) .OR. (CMD.EQ.1)) THEN
         IC = 1
         IF (IR.LT.36) THEN
            IR = IR + 1
         ELSE
            IR = 1
            IP = 1
            CALL RDTP (IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IP = (IR - 1)*80 + IC
         IF (B(IR)(IC:IC).EQ.' ') THEN
            IERR = 0
            IP = IP + 8
            IC = 9
         ELSE
            IERR = 1
            END IF
      ELSE IF (CMD.EQ.2) THEN
 10      CONTINUE
            CALL RDTP (IERR)
            IF (IERR.EQ.0) GO TO 10
         GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  IERR = IERR + 1
      IC = 999
      IR = 999
C
 999  RETURN
      END
      SUBROUTINE HDR (LDIR, COPY, CRT, CMD)
C-----------------------------------------------------------------------
C   Process the FITS header record pointed to by B(IR)(IC:IC+79).
C   LDIR = .TRUE. to cause listing of directory names and filenames to
C   be written to the output file UNIT 3. Files are dumped to disk when
C   COPY=.TRUE. . File names and directory names are written to the CRT
C   when CRT = .TRUE. "CMD" = 2 when an "END" header record is found so
C   that the remainder of the file may be ignored. The current directory
C   is used.
C-----------------------------------------------------------------------
      INTEGER LO, LD, CMD
      LOGICAL LDIR, COPY, CRT
      CHARACTER FNA*80, DNA*80, DNA0*80
      INTEGER FD, IR, IC, IO, NB, ITRIMS, MB, IERR, IERR2, LN, ME, LDNA
      LOGICAL EOFLG
      CHARACTER B(36)*80, S*256, TPNAM*24
      COMMON /CTRLC/ B, S, TPNAM
      COMMON /CTRLI/ FD, IR, IC, IO, EOFLG
      DATA LD, LO /3,4/
C-----------------------------------------------------------------------
      CMD = 0
      IF (B(IR)(1:8).EQ.'FILENAME') THEN
         CALL ITRIM (B(IR), ITRIMS)
         IF (CRT) WRITE (6, '(1X,A)') B(IR)(1:ITRIMS)
         IF (LDIR) WRITE (LD, '(1X,A)') B(IR)(1:ITRIMS)
         IF (COPY) THEN
            FNA = DNA(1:NB-1) // '/' // B(IR)(12:ITRIMS-1)
            CALL ZFLOPN (LO, FNA, IERR)
            IF (IERR.NE.0) GO TO 950
            REWIND (LO)
            IO = 0
            END IF
      ELSE IF (B(IR)(1:8).EQ.'DIRECTOR') THEN
         CALL ITRIM (B(IR), ITRIMS)
         IF (CRT) WRITE (6, '(''0'',A)') B(IR)(1:ITRIMS)
         IF (LDIR) WRITE (LD, '(''0'',A)') B(IR)(1:ITRIMS)
         DNA = ' '
         DNA0 = ' '
         DNA0 = B(IR)(12:80)
C                                       Construct the directory name
         IF (COPY) THEN
            MB = 1
            NB = 1
            MB = NB
            IERR = 0
 20         IF (IERR.NE.0) GO TO 30
               CALL GDNA (DNA0, MB, ME, LN, IERR)
               DNA(NB:NB+LN-1) = DNA0(MB:ME)
               IF ((IERR.EQ.0) .OR. (IERR.EQ.1)) NB = NB + LN
               IF (IERR.EQ.0) MB = ME + 1
               CALL ITRIM (DNA, LDNA)
               CALL ZCRDIR (LDNA, DNA, IERR2)
               IF (IERR2.EQ.1) WRITE (6,*) DNA,' ALREADY EXISTS BUT ',
     *            'IS NOT A DIRECTORY'
               IF (IERR2.EQ.2) WRITE (6,*) 'FAILED TO CREATE ',DNA
               GO TO 20
 30         CONTINUE
            END IF
      ELSE IF (B(IR)(1:8).EQ.'END     ') THEN
         CMD = 2
         END IF
      GO TO 990
C                                       Open error
 950  WRITE (6,'('' OPEN ERROR FOR'', A)') FNA
C                                       Point end of card
 990  IC = 80
C
 999  RETURN
      END
      SUBROUTINE ITRIM (STRING, ITRIMS)
C-----------------------------------------------------------------------
C  Function to determine length of a string (i.e., it trims trailing
C  blanks).
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
      INTEGER ITRIMS
C
C-----------------------------------------------------------------------
      ITRIMS = LEN (STRING)
 10   CONTINUE
         ITRIMS = ITRIMS - 1
         IF (ITRIMS.LT.1) GO TO 999
         IF (STRING(ITRIMS:ITRIMS).EQ.' ') GO TO 10
C
 999  RETURN
      END
      SUBROUTINE PNXT (CMD, C, N)
C-----------------------------------------------------------------------
C   If CMD = 0 transfer N copies of the char C to the output buffer.
C   Write the buffer when already full or when CMD = 1.
C   Inputs:
C      CMD     INTEGER  0 - transfer 1 char to output buffer.
C                       1 - write out the buffer.
C      C       CHAR*1   the character to be transferred.
C      N       INTEGER  repitition count for char C.
C   Outputs:
C      S       CHAR*256  the output buffer
C-----------------------------------------------------------------------
      INTEGER CMD, LO, N
      CHARACTER C*1
      INTEGER FD, IR, IC, IO, I
      LOGICAL EOFLG
      CHARACTER B(36)*80, S*256, TPNAM*24
      COMMON /CTRLC/ B, S, TPNAM
      COMMON /CTRLI/ FD, IR, IC, IO, EOFLG
C-----------------------------------------------------------------------
      LO = 4
      IF (CMD.EQ.0) THEN
         DO 10 I = 1,N
            IF (IO.LT.256) THEN
               IO = IO + 1
               S(IO:IO) = C
            ELSE
               WRITE (LO,'(A)') S(1:IO)
               IO = 1
               S = ' '
               S(IO:IO) = C
               END IF
 10         CONTINUE
      ELSE
         WRITE (LO,'(A)') S(1:IO)
         S = ' '
         IO = 0
         END IF
C
 999  RETURN
      END
      SUBROUTINE RDTP (IERR)
C-----------------------------------------------------------------------
C   Read an AIPS FITS tape into the 2880 char array in commons.
C   Inputs:
C      FD     the integer file descriptor in commons.
C      EOFLG  end of file logical flag.
C   Outputs:
C      B      the CHAR*2880 array in commons is the buffer.
C      EOFLG  the end of file flag in commons(coordinates EOF/EOV).
C      IERR   the integer return code
C             0  - no problems
C             1  - first end of file mark for the current file.
C             2  - end of volume. (two consecutive EOF's).
C             3  - not 2880 blocks or some other i/o error.
C-----------------------------------------------------------------------
      INTEGER IERR, NBYTE
      INTEGER FD, IR, IC, IO
      LOGICAL EOFLG
      CHARACTER B(36)*80, S*256, TPNAM*24
      COMMON /CTRLC/ B, S, TPNAM
      COMMON /CTRLI/ FD, IR, IC, IO, EOFLG
C-----------------------------------------------------------------------
      CALL ZTAPIO ('READ', 24, TPNAM, FD, 2880, NBYTE, B, IERR)
      IERR = 0
      IF (NBYTE.GT.0) THEN
         EOFLG = .FALSE.
         IF (NBYTE.NE.2880) IERR = 3
      ELSE
         IERR = 1
         IF (EOFLG) IERR = 2
         EOFLG = .TRUE.
         END IF
C
 999  RETURN
      END
      SUBROUTINE SPCIAL ()
C-----------------------------------------------------------------------
C   The escape char "!" has been read. Read the next character and take
C   the action indicated by it.
C      !!         =  a single exclamation point.
C      !,         =  tab (DEC 8-col convention).
C      !;         =  newline.
C      !+         =  form feed  ( = newline for now).
C      !.         =  end of information for current file.
C      !c - !f    =  3 to 26 blanks.
C      !A - !Z    =  27 to 52 blanks
C-----------------------------------------------------------------------
      INTEGER N, LO, IP
      INTEGER FD, IR, IC, IO, IERR, NR
      LOGICAL EOFLG
      CHARACTER B(36)*80, S*256, TPNAM*24
      COMMON /CTRLC/ B, S, TPNAM
      COMMON /CTRLI/ FD, IR, IC, IO, EOFLG
      DATA LO /4/
C-----------------------------------------------------------------------
C                                       get next character
      CALL GNXT (0, IP, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       what to do
      IF (B(IR)(IC:IC) .EQ. ';') THEN
         CALL PNXT (1, ' ', 1)
      ELSE IF (B(IR)(IC:IC) .EQ. '.') THEN
         IF (IO.GT.0) CALL PNXT (1, ' ', 1)
         CLOSE (LO)
         IC = 999
      ELSE IF (B(IR)(IC:IC) .EQ. '+') THEN
         CALL PNXT (1, ' ', 1)
      ELSE IF (B(IR)(IC:IC) .EQ. ',') THEN
         NR = MOD(IO, 8)
         N = 8 - NR
         CALL PNXT (0, ' ', N)
      ELSE IF (B(IR)(IC:IC) .EQ. '!') THEN
         CALL PNXT (0, '!', 1)
      ELSE
         CALL NBLNK (B(IR)(IC:IC), N)
         CALL PNXT (0, ' ', N)
         END IF
      GO TO 999
C                                       error exit
 900  WRITE (6,'('' SPCIAL: ERROR FROM GNXT ='', I6)') IERR
C
 999  RETURN
      END
      SUBROUTINE NBLNK (X, NBLNKS)
C-----------------------------------------------------------------------
C   Decode the number of blanks to be inserted.
C-----------------------------------------------------------------------
      INTEGER NX, NC, NZ, NUA, NUZ, NBLNKS
      CHARACTER X
C-----------------------------------------------------------------------
      NX = ICHAR(X)
      NC = ICHAR('c')
      NZ = ICHAR('z')
      NUA = ICHAR('A')
      NUZ = ICHAR('Z')
C
      IF ((NC.LE.NX) .AND. (NX.LE.NZ)) THEN
         NBLNKS = NX -NC + 3
      ELSE IF ((NUA.LE.NX) .AND. (NX.LE.NUZ)) THEN
         NBLNKS = NX - NUA + 27
      ELSE
         NBLNKS = 1
         END IF
C
 999  RETURN
      END
C***********************************************************************
C               UNIX Z ROUTINES
C***********************************************************************
      SUBROUTINE ZFLOPN (LO, FNA, IERR)
C-----------------------------------------------------------------------
C   ZFLOPN creates a text file of name FNA (includes full directory path
C   as well as the file name and extension) and opens it with UNIT = LO.
C   Note: this is a Z routine because FNA is expressed in UNIX notation
C   and may be inappropriate for other systems.
C   UNIX version
C-----------------------------------------------------------------------
      INTEGER LO, IERR
      CHARACTER FNA*80
C-----------------------------------------------------------------------
      IERR = 0
C                                       open it
C                                       no name conversion needed
      OPEN (UNIT=LO, FILE=FNA, STATUS='NEW', ERR=950)
      GO TO 999
C                                       Error
 950  IERR = 1
C
 999  RETURN
      END
C***********************************************************************
C
C THE SOURCE CODE FOR THE UNIX C VERSIONS OF ZTAPIO & ZCRDIR FOLLOWS:
C
C-----------------------------------------------------------------------
C #include <stdio.h>
C #include <sys/types.h>
C #include <sys/stat.h>
C #define MAXL 80
C
C zcrdir_(len, name, ierr)
C /*------------------------------------------------------------------*/
C /*! create the specified disk directory                             */
C /*# IO-basic                                                        */
C /*   This software is the subject of a User agreement and is        */
C /*   confidential in nature. It shall not be sold or otherwise made */
C /*   available or disclosed to third parties.                       */
C /*------------------------------------------------------------------*/
C /*  Create the directory specified in the character string "name".  */
C /*  Inputs:                                                         */
C /*     len    I       string length of name (at least to last char) */
C /*     name   C*(*)   directory name                                */
C /*  Output:                                                         */
C /*     ierr   I       Error code:                                   */
C /*                       0  directory already existed or creation ok*/
C /*                       1  if exists but is not a directory        */
C /*                       2  if creation failed                      */
C /*------------------------------------------------------------------*/
C int *ierr, *len ;
C char name[] ;
C {
C    char st[100], nam[MAXL];
C    int i, mlen, ret;
C    struct stat stbuf;
C
C    mlen = (len > MAXL) ? MAXL : len;
C    for (i=0; i < mlen; i++) {
C       if (name[i] != ' ')
C          nam[i] = name[i];
C       else
C          nam[i] = '\0';
C       }
C
C    ret = stat(nam, &stbuf);
C    if (ret == 0) {
C       if (stbuf.st_mode & 0040000)
C          *ierr = 0;
C       else
C          *ierr = 1;
C       }
C    else {
C       sprintf(st,"mkdir %s",nam);
C       *ierr = system(st);
C       if (*ierr != 0)
C          *ierr = 2;
C       }
C
C    return;
C }
C-----------------------------------------------------------------------
C ztapio_(oper, ln, name, fd, bytreq, bytread, buf, syserr)
C --------------------------------------------------------------------*/
C ! tape operations for IMPFIT (compressed FITS transport tape)       */
C # Tape                                                              */
C   This software is the subject of a User agreement and is           */
C   confidential in nature. It shall not be sold or otherwise made    */
C   available or disclosed to third parties.                          */
C --------------------------------------------------------------------*/
C   tape operations for IMPFIT (compressed FITS transport tape)       */
C   Inputs:                                                           */
C      oper     C*4    'OPEN', 'READ', 'CLOS'                         */
C      name     C*(*)  physical file name                             */
C      bytreq   I      bytes to be read                               */
C      ln       I      length of name                                 */
C   In/out:                                                           */
C      fd       I(*)   file descriptor (set on OPEN, else used)       */
C   Output:                                                           */
C      bytread  I      Bytes read on READ                             */
C      buf      I(*)   Data buffer read                               */
C      syserr   I      System error code                              */
C   Generic UNIX version.                                             */
C --------------------------------------------------------------------*/
C int *ln, *fd, *bytreq, *bytread, *syserr;
C char oper[], name[], buf[];
C --------------------------------------------------------------------*/
C {
C    char nam[65], op[5];
C    int i;
C    extern int errno ;
C --------------------------------------------------------------------*/
C                                         /* get operation */
C    for (i=0; (i < 4) && (oper[i] != ' '); i++)
C       op[i] = oper[i];
C    op[i] = '\0';
C
C    errno = 0 ;
C    if (strcmp (op, "READ") == 0) {
C       *bytread = read(*fd,buf,*bytreq);
C       }
C
C    else if (strcmp (op, "OPEN") == 0) {
C       for (i=0; (i < *ln) && (name[i] != ' ') ; i++)
C          nam[i] = name[i];
C       nam[i] = '\0';
C       *fd = open (nam, 0);
C       }
C
C    else if (strcmp (op, "CLOS") == 0) {
C       close (*fd);
C       }
C
C    *syserr = errno ;
C
C    return;
C }
C-----------------------------------------------------------------------
C***********************************************************************
C            Samples for other systems should go here too
C***********************************************************************

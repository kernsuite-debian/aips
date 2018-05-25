      SUBROUTINE ZBKLD2 (OP, IERR)
C-----------------------------------------------------------------------
C! does BACKUP operation: load images from tape to directory
C# Z Tape
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
C   Host-dependent routine to process input tape for task BAKLD.
C   The input tape is presumed to have been produced by task BAKTP
C   executing on the same host/OS combination. Data format is the
C   hosts 'backup' utility (BACKUP on VMS, 'tar' on Unix).
C   Inputs:
C      OP    C*4   'SKIP' skips over a saveset.
C                  'PRNT' moves over a saveset, listing directory info.
C                  'LOAD' loads a saveset.
C   Output:
C      IERR  I     Error return
C   Unix version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      INTEGER   ITRIM, JERR, LZDA0N, XLEN, ITEMP, CLEN
      LOGICAL   EOI
      CHARACTER COMMND*40, FNAME*50, GDA0N*4, ZDA0N*50, TEXT*128,
     *   TEMP*60, LLO*40, TY*2, DV*1, SN*3, PQ*1, BAKLDN*7,
     *   VQ*1, TEMP2*60, CCNO*3, CUSER*3, HTEMP*1, BACKSL*2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBKL.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       trick to get >= 1 \ thru compile
      BACKSL = '\\'
      IF ((OP.NE.'PRNT') .AND. (OP.NE.'SKIP') .AND. (OP.NE.'LOAD')) THEN
         WRITE (MSGTXT,1000) OP
         IERR = 99
         GO TO 990
         END IF
      IF (OP.EQ.'LOAD') THEN
         IVOL = KVOL4 + 0.01
         IF (IVOL.LE.0) THEN
            IVOL = 1
            WRITE (MSGTXT,1001) IVOL
            END IF
         END IF
C                                       translate DA0v
      CALL ZEHEX (IVOL, 1, HTEMP)
      GDA0N = 'DA0' // HTEMP
      XLEN = LEN (ZDA0N)
      CALL ZTRLOG (4, GDA0N, XLEN, ZDA0N, LZDA0N, JERR)
C                                       generate command file
      OPEN (ZLUN2, FILE=ZBKNAM, STATUS='OLD', ERR=900)
      REWIND ZLUN2
      CALL ZEHEX (NPOPS, 1, HTEMP)
      BAKLDN = '/BAKLD' // HTEMP
      TEMP = ZDA0N(1:LZDA0N) // BAKLDN // '.DIR'
      ITEMP = ITRIM (TEMP)
C                                       make directory on LOAD
      IF (OP.EQ.'LOAD') THEN
         TEXT = 'mkdir ' // TEMP
         JERR = ITEMP + 6
         WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
         END IF
C                                       change to directory
      TEXT = 'cd ' // TEMP
      JERR = ITEMP + 3
      WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
C                                       Wait till the Exabyte is ready
      TEXT = 'MTEX -f ' // ZMT0N(1:LZMT0N) // ' wait'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
C                                       The 'dd' command itself
      TEXT = 'dd if=' // ZMT0N(1:LZMT0N) // ' ibs=60b obs=1b 2>' //
     *   ZDA00(1:LZDA00) // BAKLDN // '.DDE' // ' ' // BACKSL(1:1)
      JERR = ITRIM (TEXT)
      WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
      IF (OP.NE.'LOAD') THEN
         TEMP = BAKTXT
      ELSE
         TEMP = ' '
         END IF
      TEXT = ' | tar xvbf 1 - ' // TEMP(1:ITRIM(TEMP)) // ' 2>' //
     *   ZDA00(1:LZDA00) // BAKLDN // '.TARE'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
C-----------------------------------------------------------------------
C     NOTE#1: we assume that "dd" will copy to the tapemark at the end
C        of the saveset, and will leave tape positioned BEFORE the mark.
C        Therefore, we will skip a tapemark if we actually
C        got a file from the tar.
C     NOTE#2: if empty file, and dd hits tapemark, it leaves tape
C        positioned AFTER the mark, and so in this case we back over
C        the mark to leave tape positioned at EOI. It is certainly
C        conceivable that not all Unixes will display the same behavior
C        on these subtle points!
C-----------------------------------------------------------------------
      LLO = ZDA00(1:LZDA00) // BAKLDN // '.LLO'
      TEXT = 'if test -f ' // BAKTXT(1:ITRIM(BAKTXT))
      JERR = ITRIM (TEXT)
      WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
      WRITE (ZLUN2,1010,ERR=910) 'then'
C                                       The Exabyte is a very
C                                       mysterious beast!
      TEXT = '   MTEX -f ' // ZMT0N(1:LZMT0N) // ' bof'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
      IF (OP.EQ.'LOAD') THEN
         TEXT = '   ls -1 ?????????.???' // BACKSL(1:1) // '; 1>' //
     *      LLO(1:ITRIM(LLO))
         JERR = ITRIM (TEXT)
         WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
         END IF
C      WRITE (ZLUN2,1010,ERR=910) 'else'
C                                       The Exabyte is a very
C                                       mysterious beast!
C      TEXT = '   echo "File backspace circumvented."'
C                TEXT = '   mt -f ' // ZMT0N(1:LZMT0N) // ' bsf'
C      JERR = ITRIM (TEXT)
C      WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
      WRITE (ZLUN2,1010,ERR=910) 'fi'
      CLOSE (ZLUN2)
C                                       execute the command file
      COMMND = ZBKNAM
      CLEN = ITRIM (COMMND)
      CALL ZSHCMD (CLEN, COMMND, 8, 'TASK_OUT', IERR)
C                                       read special catalog file:
      FNAME = ZDA0N(1:LZDA0N) // BAKLDN // '.DIR/' // BAKTXT
      OPEN (ZLUN1, FILE=FNAME, STATUS='OLD', ERR=20)
      REWIND ZLUN1
      READ  (ZLUN1,1010,ERR=20) BAKREC
      READ  (ZLUN1,1010,ERR=20) BAKVER
      CLOSE (ZLUN1)
      EOI = .FALSE.
      GO TO 30
C                                       Fortran error
 20   EOI = .TRUE.
      GO TO 100
C                                       log the file read:
 30   WRITE (MSGTXT,1030) OP, BAKREC
      CALL MSGWRT (6)
C                                       LOAD only
      IF (OP.EQ.'LOAD') THEN
C                                       Begin file rename stuff:
C                                       execute mach-indep code:
         CALL BAKLDS ('CREA', IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Rename data files:
         OPEN (UNIT=ZLUN1, FILE=LLO, STATUS='OLD',ERR=920)
         REWIND ZLUN1
         OPEN (UNIT=ZLUN2, FILE=ZBKNAM, STATUS='UNKNOWN',ERR=900)
         REWIND ZLUN2
         CALL ZEHEX (CNO, 3, CCNO)
         CALL ZEHEX (USER, 3, CUSER)
C                                       delete old files if any
         TEXT = 'rm ' // ZDA0N(1:LZDA0N) // '/???' // CCNO // '????' //
     *      CUSER // '?'
         JERR = ITRIM (TEXT)
         WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
C                                       move to BAKLD directory
         TEXT = 'cd ' // ZDA0N(1:LZDA0N) // BAKLDN // '.DIR'
         JERR = ITRIM (TEXT)
         WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
C                                       read file names loop
 50      READ (ZLUN1,1010,END=60,ERR=930) TEMP
            READ (TEMP,1050) TY, DV, SN, PQ, VQ
            IF ((PQ.NE.'.') .OR. (VQ.NE.';')) THEN
               MSGTXT = 'ZBKLD2: BAD FILE NAME ''' // TEMP(1:14) // ''''
               IERR = 2
               GO TO 990
               END IF
            TEMP2 = TEMP(1:12)
C                                       escape the ';' in the filename:
            TEMP2(13:13) = BACKSL(1:1)
            TEMP2(14:15) = TEMP(13:14)
            TEMP = TEMP2
            TEXT = 'ln ' // TEMP(1:15) // ' ../' // TY // DV // CCNO //
     *         SN // '.' // CUSER // BACKSL(1:1) // ';'
            JERR = ITRIM (TEXT)
            WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
            GO TO 50
 60      CLOSE (UNIT=ZLUN1)
         TEXT = 'rm -r ' // ZDA0N(1:LZDA0N) // BAKLDN // '.DIR'
         JERR = ITRIM (TEXT)
         WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
         CLOSE (UNIT=ZLUN2)
         COMMND = ZBKNAM
         CLEN = ITRIM (COMMND)
         CALL ZSHCMD (CLEN, COMMND, 8, 'TASK_OUT', IERR)
C                                       do some more mach-indep code:
         CALL BAKLDS ('UPDT', IERR)
         GO TO 999
         END IF
C-----------------------------------------------------------------------
C                                       cleanup private directory:
 100  IF (IVOL.LE.0) THEN
         IF (EOI) GO TO 110
         COMMND = 'rm ' // ZDA0N(1:LZDA0N) // BAKLDN // '.DIR/*'
      ELSE
         COMMND = 'rm -r ' // ZDA0N(1:LZDA0N) // BAKLDN // '.DIR'
         END IF
      IF ((.NOT.EOI) .OR. (IVOL.GT.0)) THEN
         CLEN = ITRIM (COMMND)
         CALL ZSHCMD (CLEN, COMMND, 8, 'TASK_OUT', IERR)
         END IF
C                                       baby catalog didn't exist:
 110  IF (.NOT.EOI) GO TO 999
         IF (OP.NE.'SKIP') THEN
            MSGTXT = 'Not a BAKTP saveset - is tape positioned'
     *         // ' correctly?'
            IERR = 999
         ELSE
            MSGTXT = 'Skipped a file which was not a BAKTP saveset'
            END IF
         GO TO 990
C                                       Error: open command file
 900  MSGTXT = 'ZBKLD2: UNABLE TO OPEN ' // ZBKNAM
      IERR = 1
      GO TO 990
C                                       Error: write command file
 910  MSGTXT = 'ZBKLD2: UNABLE TO WRITE ' // ZBKNAM
      IERR = 1
      CALL MSGWRT (8)
      MSGTXT = TEXT
      GO TO 990
C                                       Error: open command file
 920  MSGTXT = 'ZBKLD2: UNABLE TO OPEN ' // LLO
      IERR = 1
      GO TO 990
C                                       Error: read command file
 930  MSGTXT = 'ZBKLD2: UNABLE TO READ ' // LLO
      IERR = 1
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZBKLD2: BAD OPCODE = ',A4,'; FATAL ERROR.')
 1001 FORMAT ('File(s) will be loaded to default OUTDISK; using',I2)
 1010 FORMAT (A)
 1030 FORMAT (A4,1X,A58)
 1050 FORMAT (A2,A1,3X,A3,A1,3X,A1)
 1110 FORMAT ('Saw no BAKTP saveset; POSSIBLY at End-of-Information.')
      END

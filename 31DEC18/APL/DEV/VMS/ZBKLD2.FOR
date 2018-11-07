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
C   VMS version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   IERR
C
      INTEGER   ITRIM, JERR, LZDA0N, XLEN, ITEMP, IL
      LOGICAL   T, EOI
      CHARACTER COMMND*80, FNAME*60, GDA0N*4, ZDA0N*50, TEXT*128,
     *   TEMP*132, BAKERR*60, LLO*60, TY*2, DV*1, SN*2, PQ*1, CCNO*3,
     *   VQ*2, TEMP2*132, DATVER*1, DBLOG2*7, HTEMP*1, CUSER*3
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBKL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      IF ((OP.NE.'PRNT') .AND. (OP.NE.'SKIP') .AND. (OP.NE.'LOAD')) THEN
         WRITE (MSGTXT,1000) OP
         IERR = 99
         GO TO 990
         END IF
      IVOL = 0
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
C                                       generate command file:
      OPEN (ZLUN2, FILE=ZBKNAM, STATUS='OLD', ERR=900)
      REWIND ZLUN2
      TEMP = ZDA0N(1:LZDA0N-1) // '.' // TKNAME // ']'
      ITEMP = ITRIM (TEMP)
C                                       make directory on LOAD
      IF (OP.EQ.'LOAD') THEN
         TEXT = '$ CREATE/DIRECTORY/PROT=(O:RWED,G:RWED) ' //
     *      TEMP(1:ITEMP)
         JERR = ITRIM(TEXT)
         WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
         END IF
C                                       set default to our dir
      TEXT = '$ SET DEFAULT ' // TEMP(1:ITEMP)
      JERR = ITRIM (TEXT)
      WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
C                                       create syserror file name
      BAKERR = ZDA00(1:LZDA00) // TKNAME // '.BAKERR'
      ITEMP = ITRIM (BAKERR)
      TEXT = '$ ASSIGN/USER ' // BAKERR(1:ITEMP) // ' SYS$ERROR'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
C                                       Backup  command
      DBLOG2 = DBLOG
      IF (OP.EQ.'LOAD') DBLOG2 = '/LOG'
      TEXT = '$ BACKUP/BLOCK=30720' // DBLOG // ' -'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
      IF (OP.NE.'LOAD') THEN
         TEMP = BAKTXT
      ELSE
         TEMP = '*.*'
         END IF
      TEXT = ' '
      ITEMP = ITRIM (TEMP)
      TEXT(9:) = ZMT0N(1:LZMT0N) // 'BAKAIP.BCK/SELECT=(' //
     *   TEMP(1:ITEMP) // ') []'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
C
      LLO = ZDA00(1:LZDA00) // TKNAME // '.LLO'
      IF (OP.EQ.'LOAD') THEN
         TEMP = '%%%%%%%%%.%%%;1'
         TEXT = '$ IF F$SEARCH("' // TEMP(1:15) // '").NES."" THEN -'
         JERR = ITRIM (TEXT)
         WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
         TEXT = '$ DIRECTORY/NOHEAD/NOTRAIL/OUTPUT=' //
     *      LLO(1:ITRIM(LLO)) // ' ' // TEMP(1:14)
         JERR = ITRIM (TEXT)
         WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
         END IF
      CLOSE (ZLUN2)
C                                       execute the command file
      COMMND = '@' // ZBKNAM
      TEMP = ZDA00(1:LZDA00) // TKNAME // '.BAKOUT'
      IL = ITRIM (COMMND)
      ITEMP = ITRIM (TEMP)
      CALL ZSHCMD (IL, COMMND, ITEMP, TEMP, IERR)
C                                       read special catalog file:
      FNAME = ZDA0N(1:(LZDA0N-1)) // '.' // TKNAME // ']' // BAKTXT
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
         OPEN (UNIT=ZLUN1, FILE=BAKERR, STATUS='OLD', ERR=37)
         REWIND ZLUN1
 35      READ (ZLUN1,1010,END=36) TEMP
            WRITE (MSGTXT,1010) TEMP(1:64)
            GO TO 35
 36      CLOSE (UNIT=ZLUN1, STATUS='DELETE')
C                                       must check version eventually:
C                                       Execute mach-indep code:
 37      CALL BAKLDS ('CREA', IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Rename data files:
         OPEN (UNIT=ZLUN1, FILE=LLO, STATUS='OLD', ERR=920)
         REWIND ZLUN1
         OPEN (UNIT=ZLUN2, FILE=ZBKNAM, STATUS='OLD', ERR=900)
         REWIND ZLUN2
C                                       should delete only current ver:
         DATVER = '%'
         CALL ZEHEX (CNO, 3, CCNO)
         CALL ZEHEX (USER, 3, CUSER)
         TEXT = '$ DELETE/NOLOG' // DBLOG // ZDA0N(1:LZDA0N) // '%%' //
     *      DATVER // CCNO // '%%%.' // CUSER // ';1'
         JERR = ITRIM (TEXT)
         WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
 50      READ (ZLUN1,1010,END=60,ERR=930) TEMP
            TEMP2 = TEMP((INDEX(TEMP,']')+1):)
            READ (TEMP2,1050) TY, DV, SN, PQ, VQ
            IF ((PQ.NE.'.') .OR. (VQ.NE.';1')) THEN
               MSGTXT = 'ZBKLD2: BAD FILE NAME ''' // TEMP(1:14) // ''''
               IERR = 2
               GO TO 990
               END IF
            ITEMP = ITRIM (TEMP)
            TEXT = '$ RENAME' //  DBLOG2 // TEMP(:ITEMP) // ' -'
            JERR = ITRIM (TEXT)
            WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
            TEXT = ' '
            TEXT(13:) = ZDA0N(1:LZDA0N) // TY // DV // CCNO // SN //
     *         '.' // CUSER // ';1'
            JERR = ITRIM (TEXT)
            WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
            GO TO 50
 60      CLOSE (UNIT=ZLUN1, STATUS='DELETE')
         TEXT = '$ DELETE/NOLOG' // DBLOG // ZDA0N(1:(LZDA0N-1))
     *      // '.' // TKNAME // ']*.*;*'
         JERR = ITRIM (TEXT)
         WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
         TEXT = '$ DELETE/NOLOG' // DBLOG // ZDA0N(1:LZDA0N)
     *      // TKNAME // '.DIR;1'
         JERR = ITRIM (TEXT)
         WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
         CLOSE (UNIT=ZLUN2)
         COMMND = '@' // ZBKNAM
         IL = ITRIM (COMMND)
         CALL ZSHCMD (IL, COMMND, 8, 'TASK_OUT', IERR)
C                                       do some mach-indep code:
         CALL BAKLDS ('UPDT', IERR)
         GO TO 999
         END IF
C-----------------------------------------------------------------------
C                                       cleanup private directory:
 100  IF ((.NOT.EOI) .OR. (IVOL.GT.0)) THEN
         OPEN (UNIT=ZLUN2, FILE=ZBKNAM, STATUS='OLD')
         REWIND ZLUN2
         IF (.NOT.EOI) THEN
            TEXT = '$ DELETE/NOLOG' // DBLOG // ZDA0N(1:(LZDA0N-1))
     *         // '.' // TKNAME // ']*.*;*'
            JERR = ITRIM (TEXT)
            WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
            END IF
         IF (IVOL.GT.0) THEN
            TEXT = '$ DELETE/NOLOG' // DBLOG // ZDA0N(1:LZDA0N)
     *         // TKNAME // '.DIR;1'
            JERR = ITRIM (TEXT)
            WRITE (ZLUN2,1010,ERR=910) TEXT(1:JERR)
            TEXT = '$ DELETE/LOG ' // ZDA00(1:LZDA00) // TKNAME //
     *         '.BAKOUT;*,.BAKERR;*'
            END IF
         CLOSE (UNIT=ZLUN2)
         COMMND = '@' // ZBKNAM
         IL = ITRIM (COMMND)
         CALL ZSHCMD (IL, COMMND, 8, 'TASK_OUT', IERR)
         END IF
C                                       baby catalog didn't exist:
      IF (.NOT.EOI) GO TO 999
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
 1050 FORMAT (A2,A1,3X,A2,A1,3X,A2)
 1110 FORMAT ('Saw no BAKTP saveset; PRESUMABLY at End-of-Information.')
      END

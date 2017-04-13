      SUBROUTINE ZBKTP1 (ZLUN, ZBKNAM, BAKTXT, NTAPE, ZMT0N, LZMT0N,
     *   IERR)
C-----------------------------------------------------------------------
C! initialize BACKUP to tape operation for BAKTP
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
C   Initialize the host environment in preparation for ZBKTP2.  If
C   OPCODE = 'INIT', initializes the tape too.
C   (used only by task BAKTP)
C   Inputs:
C      NTAPE    I       Tape drive number
C      BAKTXT   C*(*)   lowest file name for file listing (not used
C                       here, only in VMS now)
C   Outputs:
C      ZLUN     I       LUN to use
C      ZBKNAM   C*(*)   Command file name
C      ZMT0N    C*(*)   Tape name (translation of MT0n) n=NTAPE-1
C      LZMT0N   I       Actual length used in ZMT0N
C      IERR     I       Error code: 0 => okay
C   VMS version.
C-----------------------------------------------------------------------
      INTEGER   ZLUN, NTAPE, LZMT0N, IERR
      CHARACTER ZBKNAM*(*), BAKTXT*(*), ZMT0N*(*)
C
      INTEGER   JERR, ITEMP, ITRIM, IL
      CHARACTER COMMND*40, CUSER*3, CTEMP*1, TEXT*128, FNAME*50
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBTP.INC'
C-----------------------------------------------------------------------
C                                       ZLUN is OS-dependent:
      ZLUN = 79
C                                       create private command file:
      CALL ZEHEX (NPOPS, 1, CTEMP)
      CALL ZEHEX (USER, 3, CUSER)
      ZBKNAM = 'DA00:BAKTP' // CTEMP // '.' // CUSER // ';1'
      IERR = 0
      OPEN (ZLUN, FILE=ZBKNAM, STATUS='UNKNOWN', ERR=900,
     *   CARRIAGECONTROL='LIST')
      REWIND ZLUN
      TEXT = '$ SET DEFAULT DA00:'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1000,ERR=900) TEXT(1:JERR)
      TEXT = '$ CREATE/DIRECTORY [.BAKTP' // CTEMP // ']'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1000,ERR=900) TEXT(1:JERR)
      TEXT = '$ SET DEFAULT [.BAKTP' // CTEMP // ']'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1000,ERR=900) TEXT(1:JERR)
      TEXT = '$ A = "''''F$PARSE(""ANY"",,,""DEVICE"")''"'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1000,ERR=900) TEXT(1:JERR)
      TEXT = '$ B = "''''F$PARSE(""ANY"",,,""DIRECTORY"")''"'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1000,ERR=900) TEXT(1:JERR)
      TEXT = '$ ASSIGN/JOB "''''A''''''B''"' // ' ZBAKTP' // CTEMP
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1000,ERR=900) TEXT(1:JERR)
      CLOSE (ZLUN)
      COMMND = '@' // ZBKNAM
      IL = ITRIM (COMMND)
      CALL ZSHCMD (IL, COMMND, 8, 'TASK_OUT', IERR)
C                                       INITialize the BACKUP tape:
C                                       (write label+init-saveset)
      IF (OP.EQ.'INIT') THEN
C                                       generate special catalog file:
         FNAME = 'Z' // ZBKNAM(6:11) // ':' // BAKTXT
         OPEN (ZLUN, FILE=FNAME, STATUS='UNKNOWN', ERR=910,
     *      CARRIAGECONTROL='LIST')
         REWIND ZLUN
         TEXT = 'INIT for AIPS task BAKTP (skipped by task BAKLD)'
         JERR = ITRIM (TEXT)
         WRITE (ZLUN,1000,ERR=910) TEXT(1:JERR)
         CLOSE (ZLUN)
C                                       generate command file:
         OPEN (ZLUN, FILE=ZBKNAM, STATUS='OLD', ERR=910)
         REWIND ZLUN
         TEXT = '$ BACKUP/BLOCK=30720 -'
         JERR = ITRIM (TEXT)
         WRITE (ZLUN,1000,ERR=910) TEXT(1:JERR)
         TEXT = ' '
         JERR = ITRIM (FNAME)
         TEXT(9:) = FNAME(:JERR) // ' -'
         JERR = ITRIM (TEXT)
         WRITE (ZLUN,1000,ERR=910) TEXT(1:JERR)
         ITEMP = NTAPE
         CALL ZEHEX (ITEMP, 1, CTEMP)
         TEXT(9:) = 'AMT0' // CTEMP //
     *      ':BAKINI.BCK/REWIND/LABEL=(BAKAIP)'
         JERR = ITRIM (TEXT)
         WRITE (ZLUN,1000,ERR=910) TEXT(1:JERR)
         CLOSE (ZLUN)
         COMMND = '@' // ZBKNAM
         IL = ITRIM (COMMND)
         CALL ZSHCMD (IL, COMMND, 8, 'TASK_OUT', IERR)
         WRITE (MSGTXT,1010)
         CALL MSGWRT (5)
         END IF
      GO TO 999
C                                       can't create, fatal error:
 900  MSGTXT = 'ZBKTP1: CANNOT CREATE/WRITE ' // ZBKNAM
      GO TO 990
 910  MSGTXT = 'ZBKTP1: CANNOT CREATE/WRITE ' // FNAME
C
 990  CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A)
 1010 FORMAT ('Tape initialized for VMS-BACKUP BAKTP use (VSN=BAKAIP).')
      END

      SUBROUTINE ZBKLD1 (IERR)
C-----------------------------------------------------------------------
C! initialize environment for BAKLD
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
C   Routine to initialize the host environment in preparation for
C   execution of ZBKLD2 under task BAKLD.
C   Output:
C      IERR   I   error code : 0 => okay
C                    1 => can't create listing subdirectory
C                    2 => input error - translates fail
C   VMS version.
C   NOTE: the private subdirectory on DA00 is only used by ZBKLD2 when
C   it is doing OP=PRNT/SKIP; it avoids dynamic creation of
C   subdirectories just for the little BAKTP catalog files.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   JERR, IT, ITRIM, ITEMP, XLEN, IL
      CHARACTER COMMND*40, HTEMP*1, CUSER*3, CTEMP*5, TEXT*128
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBKL.INC'
C-----------------------------------------------------------------------
      DBLOG = '/NOLOG'
C                                       ZLUNn may be host-dependent:
      ZLUN1 = 78
      ZLUN2 = 79
C                                       translate logical DA00:
      XLEN = LEN (ZDA00)
      CALL ZTRLOG (4, 'DA00', XLEN, ZDA00, LZDA00, JERR)
C                                       translate logical AMT0n:
      IF (JERR.EQ.0) THEN
         ITEMP = NTAPE
         CALL ZEHEX (ITEMP, 1, HTEMP)
         CTEMP = 'AMT0' // HTEMP
         XLEN = LEN (ZMT0N)
         CALL ZTRLOG (5, CTEMP, XLEN, ZMT0N, LZMT0N, JERR)
         END IF
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1000) CTEMP, JERR
         IERR = 2
         GO TO 990
         END IF
C                                       full task name
      CALL ZEHEX (NPOPS, 1, HTEMP)
      CALL ZEHEX (USER, 3, CUSER)
      IT = ITRIM (TSKNAM)
      TKNAME = TSKNAM(1:IT) // HTEMP
      IT = IT + 1
C                                       create private command file:
      ZBKNAM = ZDA00(1:LZDA00) // TKNAME(1:IT) // '.' // CUSER
      IERR = 0
      OPEN (ZLUN2, FILE=ZBKNAM, STATUS='NEW', ERR=900, RECL=132,
     *   ACCESS='SEQUENTIAL', FORM='FORMATTED',
     *   CARRIAGECONTROL='LIST')
C                                       create private subdirectory:
      REWIND ZLUN2
      TEXT = '$ CREATE/DIRECTORY/PROT=(O:RWED,G:RWED) ' //
     *   ZDA00(1:(LZDA00-1)) // '.' // TKNAME(1:IT) // ']'
      ITEMP = ITRIM (TEXT)
      WRITE (ZLUN2,1040,ERR=900) TEXT(1:ITEMP)
      CLOSE (ZLUN2)
C                                       now execute it (DCL):
      COMMND = '@' // ZBKNAM
      IL = ITRIM (COMMND)
      CALL ZSHCMD (IL, COMMND, 8, 'TASK_OUT', IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1045) IERR
         CALL MSGWRT (8)
         MSGTXT = 'ZBKLD1: ' // COMMND
         GO TO 990
         END IF
C                                       Must close tape under Unix.
C                                       Apparently VMS can stay open
      GO TO 999
C                                       can't create, fatal error:
 900  MSGTXT = 'ZBKLD1: CANNOT CREATE/WRITE ' // ZBKNAM
      IERR = 1
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZBKLD1: CANNOT TRANSLATE ''',A,''' IERR ',I5)
 1040 FORMAT (A)
 1045 FORMAT ('ZBKLD1: ZSHCMD ERROR',I5,' COMMAND =')
      END

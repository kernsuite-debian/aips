      SUBROUTINE ZBKTP2 (ZLUN, ZBKNAM, BAKTXT, BAKREC, IVOL, ICNO,
     *   NTAPE, ZMT0N, LZMT0N, IERR)
C-----------------------------------------------------------------------
C! write a cataloged file plus extensions to BACKUP tape in BAKTP
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
C   Host-dependent subroutine to write a file and its extensions to
C   tape using the hosts "backup" utility.
C   Inputs:
C      ZLUN     I       LUN for command file
C      ZBKNAM   C*(*)   Name of command file
C      BAKTXT   C*(*)   lowest file name for file listing
C      BAKREC   C*60    ?
C      IVOL     I       Disk number
C      ICNO     I       Catalog number
C      NTAPE    I       Tape drive number
C      ZMT0N    C*(*)   Tape name (translation of MT0n) n=NTAPE-1
C      LZMT0N   I       Actual length used in ZMT0N
C   Output:
C      IERR     I       Error code: 0 => okay, else ZSHCMD
C   VMS version.
C-----------------------------------------------------------------------
      INTEGER   ZLUN, IVOL, ICNO, NTAPE, LZMT0N, IERR
      CHARACTER ZBKNAM*(*), BAKTXT*(*), BAKREC*(*), ZMT0N*(*)
C
      CHARACTER TYPINO*2, MNAME*48, CTEMP*1, CUSER*3, CCNO*3, TEXT*128,
     *   COMMND*80, FNAME*50, DATVER*1
      INTEGER   NP, ITRIM, JERR, IL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBTP.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       generate special catalog file:
      FNAME = 'Z' // ZBKNAM(6:11) // ':' // BAKTXT
      OPEN (ZLUN, FILE=FNAME, STATUS='UNKNOWN', CARRIAGECONTROL='LIST',
     *   ERR = 900)
      REWIND ZLUN
      WRITE (ZLUN,1010) BAKREC
      WRITE (ZLUN,1010) SYSNAM
      CLOSE (ZLUN)
C                                       get data-version code:
      CALL H2CHR (2, KHPTYO, CATH(KHPTY),TYPINO)
      CALL ZPHFIL (TYPINO, IVOL, ICNO, 1, MNAME, IERR)
      NP = INDEX (MNAME, TYPINO) + 2
      DATVER = MNAME(NP:NP)
C                                       generate command file:
      OPEN (ZLUN, FILE=ZBKNAM, STATUS='OLD', ERR=910)
      REWIND ZLUN
      TEXT = '$ BACKUP/LOG/IGNORE=INTERLOCK/BLOCK=30720 -'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=920) TEXT(1:JERR)
      CALL ZEHEX (IVOL, 1, CTEMP)
      CALL ZEHEX (ICNO, 3, CCNO)
      CALL ZEHEX (USER, 3, CUSER)
      TEXT = ' '
      JERR = ITRIM (FNAME)
      TEXT(9:) = FNAME(:JERR) // ',DA0' // CTEMP // ':%%' // DATVER //
     *   CCNO // '%%%.' // CUSER // ' -'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=920) TEXT(1:JERR)
      NP = NTAPE
      CALL ZEHEX (NP, 1, CTEMP)
      TEXT(9:) = 'AMT0' // CTEMP // ':BAKAIP.BCK/LABEL=(BAKAIP)'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=920) TEXT(1:JERR)
      CLOSE (ZLUN)
      COMMND = '@' // ZBKNAM
      IL = ITRIM (COMMND)
      CALL ZSHCMD (IL, COMMND, 8, 'TASK_OUT', IERR)
      GO TO 999
C                                       Errors - can't open catalog
 900  MSGTXT = 'ZBKTP2: CAN''T OPEN ' // FNAME
      IERR = 1
      GO TO 990
C                                       Errors - can't open command
 910  MSGTXT = 'ZBKTP2: CAN''T OPEN ' // ZBKNAM
      IERR = 1
      GO TO 990
C                                       Errors - can't write command
 920  MSGTXT = 'ZBKTP2: CAN''T WRITE ' // ZBKNAM
      IERR = 1
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (A)
      END

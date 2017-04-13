      SUBROUTINE ZBKTP3 (ZLUN, ZBKNAM, IERR)
C-----------------------------------------------------------------------
C! clean up host environment at end of BAKTP
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
C   Clean up host environment after BAKTP execution.
C   Inputs:
C      ZLUN     I       LUN for command file
C      ZBKNAM   C*(*)   Name of command file
C   Output:
C      IERR     I       Error code: 0 => okay, else ZSHCMD
C   VMS version
C-----------------------------------------------------------------------
      INTEGER   ZLUN, IERR
      CHARACTER ZBKNAM*(*)
C
      INTEGER   ITRIM, JERR, IL
      CHARACTER COMMND*40, TEXT*128, CPOPS*1
      INCLUDE 'INCS:DBTP.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OPEN (ZLUN, FILE=ZBKNAM, STATUS='UNKNOWN', ERR=900,
     *   CARRIAGECONTROL='LIST')
C                                       generate command file:
      REWIND ZLUN
C                                       delete our subdirectory:
      CALL ZEHEX (NPOPS, 1, CPOPS)
      TEXT = '$ DELETE/NOLOG ZBAKTP' // CPOPS // ':*.*;*'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=910) TEXT(1:JERR)
      TEXT = '$ SET DEFAULT DA00:'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=910) TEXT(1:JERR)
      TEXT = '$ SET PROT=(O:RWED) BAKTP' // CPOPS //'.DIR'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=910) TEXT(1:JERR)
      TEXT = '$ DELETE/NOLOG BAKTP' // CPOPS // '.DIR;*'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=910) TEXT(1:JERR)
C                                       delete our logical symbol:
      TEXT = '$ DEASSIGN/JOB ZBAKTP' // CPOPS
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=910) TEXT(1:JERR)
C                                       delete THIS(!) command file:
      TEXT = '$ DELETE/NOLOG ' // ZBKNAM
      JERR = ITRIM (TEXT)
      WRITE (ZLUN,1010,ERR=910) TEXT(1:JERR)
      CLOSE (ZLUN)
      COMMND = '@' // ZBKNAM
      IL = ITRIM (COMMND)
      CALL ZSHCMD (IL, COMMND, 8, 'TASK_OUT', IERR)
C                                       Close tape
      CALL ZTPCLS (TLUN, TIND, JERR)
      IF (IERR.EQ.0) IERR = JERR
      TIND = 0
      GO TO 999
C                                       can't open, fatal error:
 900  MSGTXT = 'ZBKTP3: CAN''T OPEN ' // ZBKNAM
      GO TO 990
 910  MSGTXT = 'ZBKTP3: CAN''T WRITE ' // ZBKNAM
C
 990  CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (A)
      END

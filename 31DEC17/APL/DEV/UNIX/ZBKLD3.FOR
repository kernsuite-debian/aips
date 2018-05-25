      SUBROUTINE ZBKLD3 (IERR)
C-----------------------------------------------------------------------
C! clean up system things for BAKLD ending
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
C   Routine to clean up host environment after executing task BAKLD.
C   Output:
C      IERR   I   Error code: 0 => okay
C                    1 => can't open command file
C   Generic UNIX version.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   ITEMP, ITRIM, CLEN
      CHARACTER COMMND*40, HTEMP*1, TEXT*128
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBKL.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OPEN (ZLUN2, FILE=ZBKNAM, STATUS='UNKNOWN', ERR=900)
C                                       generate command file:
      REWIND ZLUN2
C                                       delete our DA00 subdirectory
C                                       plus dd/tar/ls scratches
      CALL ZEHEX (NPOPS, 1, HTEMP)
      TEXT = 'rm -r ' // ZDA00(1:LZDA00) // '/BAKLD' // HTEMP // '.*'
      ITEMP = ITRIM (TEXT)
      WRITE (ZLUN2,1000,ERR=900) TEXT(1:ITEMP)
      CLOSE (ZLUN2)
      COMMND = ZBKNAM
      CLEN = ITRIM (COMMND)
      CALL ZSHCMD (CLEN, COMMND, 8, 'TASK_OUT', IERR)
C                                       delete command file
      IF (IERR.EQ.0) THEN
         COMMND = 'rm ' // ZBKNAM
         CLEN = ITRIM (COMMND)
         CALL ZSHCMD (CLEN, COMMND, 8, 'TASK_OUT', IERR)
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         MSGTXT = 'ZBKLD3: ' // COMMND
         GO TO 990
         END IF
      GO TO 999
C                                       can't open, fatal error:
 900  MSGTXT = 'ZBKLD3: CANNOT OPEN ' // ZBKNAM
      IERR = 1
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A)
 1010 FORMAT ('ZBKLD3: ZSHCMD ERROR',I7,'  COMMAND=')
      END

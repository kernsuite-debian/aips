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
C   VMS version
C   It is conceivable that VMS would not want to delete the command file
C   while it is still being executed by "DCL".  In such a case, a
C   separate execution of ZSHCMD could be used.  Because we use a
C   command file, ZBKLD3 could do other commands too.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   ITRIM, ITEMP, JERR, IL
      CHARACTER COMMND*40, TEMP*60, HTEMP*1, TEXT*128
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DBKL.INC'
C-----------------------------------------------------------------------
      IERR = 0
      OPEN (ZLUN2, FILE=ZBKNAM, STATUS='OLD', ERR=900)
C                                       generate command file:
      REWIND ZLUN2
C                                       delete our DA00 subdirectory
C                                       plus BAKLD scratches;
C                                       also delete THIS command file!
      CALL ZEHEX (NPOPS, 1, HTEMP)
      TEMP = ZDA00(1:(LZDA00-1)) // '.' // TKNAME // ']*.*;*'
      ITEMP = ITRIM (TEMP)
      TEXT = '$ IF F$SEARCH("' // TEMP(:ITEMP) // '").NES."" THEN -'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN2,1000,ERR=900) TEXT(1:JERR)
      TEXT = '$   DELETE' // DBLOG // TEMP(1:ITEMP)
      JERR = ITRIM (TEXT)
      WRITE (ZLUN2,1000,ERR=900) TEXT(1:JERR)
      TEXT = '$ DELETE' // DBLOG // ZDA00(1:LZDA00) // TKNAME //
     *   '.*;*'
      JERR = ITRIM (TEXT)
      WRITE (ZLUN2,1000,ERR=900) TEXT(1:JERR)
      CLOSE (ZLUN2)
      COMMND = '@' // ZBKNAM
      IL = ITRIM (COMMND)
      CALL ZSHCMD (IL, COMMND, 8, 'TASK_OUT', IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         MSGTXT = 'ZBKLD3: ' // COMMND
         GO TO 990
         END IF
      GO TO 999
C                                       can't open, fatal error:
 900  MSGTXT = 'ZBKLD3: CANNOT OPEN/WRITE ' // ZBKNAM
      IERR = 1
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A)
 1010 FORMAT ('ZBKLD3: ZSHCMD ERROR',I7,'  COMMAND=')
      END

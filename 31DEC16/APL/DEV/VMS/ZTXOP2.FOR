      SUBROUTINE ZTXOP2 (OPCODE, LUN, FIND, OUTFIL, APPEND, IERR)
C-----------------------------------------------------------------------
C! translate the file name and open a text file
C# Z2 Text
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
C   Translate file name and do actual text file open.
C   Inputs:
C      OPCODE   C*4    Operation code 'READ' or 'WRIT'
C      LUN      I      Logical unit number
C      FIND     I      Index in FTAB for LUN
C      OUTFIL   C*48   Physical file name
C      APPEND   L      If true append new text to end of old file.
C                         (OPCODE='WRIT' only).
C   Outputs:
C      IERR     I      Error return code: 0 => no error
C                         4 => trouble translating logical
C                         5 => file doesn't exist
C                         6 => open error
C   VMS version.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, OUTFIL*48
      INTEGER   LUN, FIND, IERR
      LOGICAL   APPEND
C
      INTEGER   I, FLEN, IOSVAL, ITRIM
      LOGICAL   EXISTS
      CHARACTER FILNAM*128, LSTAT*3
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Translate logical device name.
      CALL ZFULLN (OUTFIL, ' ', ' ', FILNAM, IERR)
      IF (IERR.NE.0) THEN
         IERR = 4
         GO TO 999
         END IF
      FLEN = ITRIM (FILNAM)
      WRITE (MSGTXT,1000)
      CALL MSGWRT (2)
      WRITE (MSGTXT,1001) FILNAM(1:MIN (56, FLEN))
      CALL MSGWRT (2)
      IF (FLEN.GT.56) THEN
         WRITE (MSGTXT,1001) FILNAM(57:FLEN)
         CALL MSGWRT (2)
         END IF
C                                       See if file exists
      INQUIRE (FILE=FILNAM, EXIST=EXISTS)
      LSTAT = 'NEW'
      IF (EXISTS) LSTAT = 'OLD'
C                                       File should exist (READ)
      IF ((OPCODE.EQ.'READ') .AND. (.NOT.EXISTS)) THEN
         IERR = 5
         WRITE (MSGTXT,1010)
C                                       File should not exist (WRIT)
      ELSE IF ((OPCODE.EQ.'WRIT') .AND. (EXISTS) .AND. (.NOT.APPEND))
     *   THEN
         IERR = 5
         WRITE (MSGTXT,1011)
      ELSE
C                                       To read (only)
         IF (OPCODE.EQ.'READ') THEN
            OPEN (UNIT=LUN, FILE=FILNAM(1:FLEN), STATUS='OLD',
     *         IOSTAT=IOSVAL, READONLY)
            IF (IOSVAL.EQ.0) REWIND (LUN)
C                                       To write at end
         ELSE IF ((EXISTS) .AND. (APPEND)) THEN
            OPEN (UNIT=LUN, FILE=FILNAM(1:FLEN), STATUS=LSTAT,
     *         CARRIAGECONTROL='LIST', IOSTAT=IOSVAL, ACCESS='APPEND')
C                                       To write from beginning
         ELSE
            OPEN (UNIT=LUN, FILE=FILNAM(1:FLEN), STATUS=LSTAT,
     *         CARRIAGECONTROL='LIST', IOSTAT=IOSVAL)
            IF (IOSVAL.EQ.0) REWIND (LUN)
            END IF
         IF (IOSVAL.NE.0) THEN
            IERR = 6
            WRITE (MSGTXT,1020) IOSVAL
            END IF
         END IF
C                                       Error
      IF (IERR.NE.0) CALL MSGWRT (6)
      IF (IERR.EQ.6) THEN
         I = FIND + NMOFF
         FTAB(I+FCBERR) = IOSVAL
         CALL ZERROR ('ZTXOP2', IOSVAL, FILNAM, -999, .FALSE.)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTXOP2: using translated file name =')
 1001 FORMAT ('ZTXOP2: ',A)
 1010 FORMAT ('ZTXOP2: FILE TO BE READ DOES NOT EXIST')
 1011 FORMAT ('ZTXOP2: FILE TO BE WRITTEN ALREADY EXISTS')
 1020 FORMAT ('ZTXOP2: FORTRAN OPEN ERROR = ',I6)
      END

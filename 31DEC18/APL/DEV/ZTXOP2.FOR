      SUBROUTINE ZTXOP2 (OPCODE, LUN, FIND, OUTFIL, APPEND, IERR)
C-----------------------------------------------------------------------
C! translate the file name and open a text file
C# Z2 Text
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2015, 2018
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
C   Translate file name and do actual text file open.
C   Inputs:
C      OPCODE   C*4    Operation code 'READ', 'QRED', 'WRIT', or 'QWRT'
C                         'REWR'. 'QREW'
C      LUN      I      Logical unit number
C      FIND     I      Index in FTAB for LUN
C      OUTFIL   C*(*)  Physical file name - fully qualified already or
C                      with a colon as logical:name
C      APPEND   L      If true append new text to end of old file.
C                         (OPCODE='WRIT' or 'QWRT').
C   Outputs:
C      IERR     I      Error return code: 0 => no error
C                         4 => trouble translating logical
C                         5 => file doesn't exist (READ)
C                         5 => file exists (WRIT no APPEND)
C                         6 => open error
C   Generic version - uses ZFULLN, Fortran INQUIRE and simple OPEN,
C   does APPEND by reading to the EOF.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, OUTFIL*(*)
      INTEGER   LUN, FIND, IERR
      LOGICAL   APPEND
C
      INTEGER   I, FLEN, IOSVAL, ITRIM, J
      LOGICAL   EXISTS, REDING, WRTING
      CHARACTER FILNAM*256, TEMP*80, LSTAT*3
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Translate logical device name.
      I = INDEX (OUTFIL, ':')
      IF (I.GT.0) THEN
         CALL ZFULLN (OUTFIL, ' ', ' ', FILNAM, IERR)
         IF (IERR.NE.0) THEN
            IERR = 4
            GO TO 999
            END IF
      ELSE
         FILNAM = OUTFIL
         END IF
      FLEN = ITRIM (FILNAM)
C                                       Init logicals for read/write
      REDING = (OPCODE.EQ.'READ') .OR. (OPCODE.EQ.'QRED')
      WRTING = (OPCODE.EQ.'WRIT') .OR. (OPCODE.EQ.'QWRT')
C                                       If not doing a quiet operation
      IF (OPCODE(:1).NE.'Q') THEN
         MSGTXT = 'ZTXOP2: using translated file name ='
         CALL MSGWRT (2)
         WRITE (MSGTXT,1001) FILNAM(1:MIN (56, FLEN))
         CALL MSGWRT (2)
         IF (FLEN.GT.56) THEN
            WRITE (MSGTXT,1001) FILNAM(57:FLEN)
            CALL MSGWRT (2)
            END IF
C                                       End if not a quiet operation
         END IF
C                                       See if file exists
      INQUIRE (FILE=FILNAM, EXIST=EXISTS)
      LSTAT = 'NEW'
      IF (EXISTS) LSTAT = 'OLD'
C                                       File should exist (READ)
      IF (REDING .AND. (.NOT.EXISTS)) THEN
         IERR = 5
         MSGTXT = 'ZTXOP2: FILE TO BE READ DOES NOT EXIST'
C                                       File should not exist (WRIT)
C                                       REWR file exists or not ok
      ELSE IF (WRTING .AND. (EXISTS) .AND. (.NOT.APPEND))
     *   THEN
         IERR = 5
         MSGTXT = 'ZTXOP2: FILE TO BE WRITTEN ALREADY EXISTS'
C                                       Else, OK: Open file.
      ELSE
         OPEN (UNIT=LUN, FILE=FILNAM(1:FLEN), STATUS=LSTAT,
     *      IOSTAT=IOSVAL)
         IF (IOSVAL.NE.0) THEN
            IERR = 6
            WRITE (MSGTXT,1020) IOSVAL
C                                       Rewind.
         ELSE
            REWIND (LUN)
C                                       Append to old file
            IF (WRTING .AND. EXISTS .AND. APPEND) THEN
               I = 0
C                                       Read to EOF
 50            READ (LUN,1050,IOSTAT=IOSVAL) TEMP
               IF (IOSVAL.EQ.0) THEN
                  I = I + 1
                  GO TO 50
                  END IF
C                                       IO error if not EOF
               IF (IOSVAL.NE.-1) THEN
                  IERR = 6
                  WRITE (MSGTXT,1051) IOSVAL
                  CLOSE (UNIT=LUN)
                  GO TO 900
                  END IF
C                                       close, rewind
               CLOSE (UNIT=LUN)
               OPEN (UNIT=LUN, FILE=FILNAM(1:FLEN), STATUS=LSTAT,
     *            IOSTAT=IOSVAL)
               REWIND (LUN)
               DO 60 J = 1,I
                  READ (LUN,1050,IOSTAT=IOSVAL) TEMP
C                                       IO error if not EOF
                  IF (IOSVAL.NE.0) THEN
                     IERR = 6
                     WRITE (MSGTXT,1051) IOSVAL
                     CLOSE (UNIT=LUN)
                     GO TO 900
                     END IF
 60               CONTINUE
               END IF
            END IF
         END IF
C                                       Error
 900  IF (IERR.NE.0) CALL MSGWRT (6)
      IF (IERR.EQ.6) THEN
         I = FIND + NMOFF
         FTAB(I+FCBERR) = IOSVAL
         CALL ZERROR ('ZTXOP2', IOSVAL, FILNAM, -999, .FALSE.)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('ZTXOP2: ',A)
 1020 FORMAT ('ZTXOP2: FORTRAN OPEN ERROR = ',I6)
 1050 FORMAT (A)
 1051 FORMAT ('ZTXOP2: FORTRAN ERROR',I5,' WHILE SKIPPING TO END OF',
     *   ' FILE')
      END

      SUBROUTINE ZEXPND (LUN, IVOL, PNAME, NREC, IERR)
C-----------------------------------------------------------------------
C! expand an open disk file --- either map or non-map now allowed
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   Increase the size of a disk file --- it must be open
C   Inputs:
C      LUN     I      LUN of file open file
C      IVOL    I      Disk volume containing file, 1,2,3,...
C      PNAME   C*48   Physical file name
C   In/Out:
C      NREC    I      # 256-integer records requested/received
C   Output:
C      IERR    I      Error return code: 0 => no error
C                        1 => input error
C                        2 => expansion error
C                        3 => ZEXIST error
C                       -1 => some, but less than all of request met
C                        4 => no disk added to file
C   Generic version - uses ZEXPN2.
C-----------------------------------------------------------------------
      INTEGER   LUN, IVOL, NREC, IERR
      CHARACTER PNAME*48
C
      INTEGER   FIND, FCBOFF, IMAP, ISIZE, LSIZE, JERR, LREC, KREC
      LOGICAL   MAP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
      IERR = 1
C                                       Valid number of records.
      IF (NREC.GT.0) GO TO 10
         WRITE (MSGTXT,1000) NREC
         GO TO 990
C                                       Valid volume?
 10   IF ((IVOL.GT.0) .OR. (IVOL.LE.NVOL)) GO TO 20
         WRITE (MSGTXT,1010) IVOL
         GO TO 990
C                                       Locate file in FTAB.
 20   MAP = .FALSE.
      CALL LSERCH ('SRCH', LUN, FIND, MAP, IERR)
      IF (IERR.EQ.0) GO TO 30
         MAP = .TRUE.
         CALL LSERCH ('SRCH', LUN, FIND, MAP, IERR)
         IF (IERR.EQ.0) GO TO 30
            WRITE (MSGTXT,1020) PNAME(1:24), LUN
            GO TO 990
C                                       Expand file.
 30   IF (MAP) THEN
         FCBOFF = FIND + MOFF
         IMAP = 1
      ELSE
         FCBOFF = FIND + NMOFF
         IMAP = 0
         END IF
      CALL ZEXIST (IVOL, PNAME, ISIZE, JERR)
      IF (JERR.NE.0) THEN
         IERR = 3
         GO TO 999
         END IF
      LREC = NREC
      CALL ZEXPN2 (FTAB(FCBOFF), IMAP, NREC, IERR)
      IF (IERR.EQ.0) THEN
         CALL ZEXIST (IVOL, PNAME, LSIZE, JERR)
         IF (JERR.NE.0) THEN
            IERR = 3
            GO TO 999
            END IF
         KREC = NREC
         NREC = LSIZE - ISIZE
         IF ((NREC.LT.KREC) .OR. (NREC.LT.LREC)) THEN
            IERR = -1
            IF (NREC.LE.0) IERR = 4
            WRITE (MSGTXT,1025) LREC, KREC, NREC
            CALL MSGWRT (6)
            END IF
C                                       Error
      ELSE
         IERR = 2
         WRITE (MSGTXT,1030) PNAME(1:24), NREC
         CALL MSGWRT (7)
         CALL ZERROR ('ZEXPN2', FTAB(FCBOFF+FCBERR), PNAME,
     *      FTAB(FCBOFF), MAP)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (7)
      NREC = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZEXPND: INVALID RECORD REQUEST = ',I6)
 1010 FORMAT ('ZEXPND: INVALID VOLUME REQUEST = ',I6)
 1020 FORMAT ('ZEXPND: FILE ',A24,' NOT OPEN IN FTAB.  LUN = ',I2)
 1025 FORMAT ('ZEXPND: REQUEST, CLAIMED, ACTUAL',3I7,' BLOCKS')
 1030 FORMAT ('ZEXPND: EXPANSION FAILED ON FILE ',A24,' NREC = ',I6)
      END

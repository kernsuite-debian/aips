      SUBROUTINE HICREA (HLUN, KVOL, CNO, CATBLK, BUFFER, IERR)
C-----------------------------------------------------------------------
C! open a history file, creating one if needed
C# History
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
C   HICREA creates and opens a history file.  If there is already a
C   history file, no creation is done and the existing file is opened
C   for adding output.
C   Inputs:
C      HLUN    I         lun to use
C      KVOL    I         disk number
C      CNO     I         catalog location
C      CATBLK  I(256)    map header
C   Output:
C      BUFFER  I(256)    HI work buffer (contains first rec)
C      IERR    I         error code - 0 => ok
C                                     others
C-----------------------------------------------------------------------
      INTEGER   HLUN, KVOL, CNO, CATBLK(256), BUFFER(256), IERR
C
      CHARACTER HNAME*48, CTEMP*2, CHTM12*12, CHTM6*6
      INTEGER   VER, HPTR, IER, I, IDUM, ISIZE, LSIZE
      LOGICAL   T, F, OLD
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                        does HI already exist
      CALL FNDEXT ('HI', CATBLK, I)
C                                        Already an HI warning
      IF (I.GT.0) THEN
         MSGTXT = 'HICREA: WARNING - ADDING TO EXISTING HI FILE'
         CALL MSGWRT (6)
         OLD = T
C                                        NO - add extension
      ELSE
         CALL MADDEX ('HI', KVOL, CNO, CATBLK, BUFFER, T, 'UPDT', VER,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
C                                        create >= 40 log recs
         ISIZE = 39 / NHILPR + 1
         OLD = F
         CALL ZPHFIL ('HI', KVOL, CNO, VER, HNAME, IERR)
         CALL ZCREAT (KVOL, HNAME, ISIZE, F, LSIZE, IERR)
         IF (IERR.NE.0) THEN
            IF (IERR.NE.1) THEN
               WRITE (MSGTXT,1010) IERR
               IF (IERR.EQ.5) WRITE (MSGTXT,1011) KVOL
               GO TO 900
C                                       warning replace old file
            ELSE
               MSGTXT = 'HICREA: WARNING - REPLACING OLD HI PHYSICAL'
     *            // ' FILE'
               CALL MSGWRT (6)
               OLD = T
               END IF
            END IF
         END IF
C                                        open it
 30   CALL HIOPEN (HLUN, KVOL, CNO, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR
         GO TO 900
C                                        reset things for new files
 40   IF (.NOT.OLD) THEN
         CALL HILOCT ('SRCH', HLUN, HPTR, IER)
         HITAB(HPTR+2) = 0
         HITAB(HPTR+3) = NHILPR * LSIZE
         BUFFER(1) = HITAB(HPTR+2)
         BUFFER(2) = HITAB(HPTR+3)
         END IF
      BUFFER(3) = HPTR
      GO TO 999
C                                        errors
 900  CALL MSGWRT (7)
      IF (.NOT.OLD) THEN
         CALL DELEXT ('HI', KVOL, CNO, 'WRIT', CATBLK, BUFFER, VER, IER)
         CALL ZDESTR (KVOL, HNAME, IER)
         CHTM12 = ' '
         CHTM6 = ' '
         CTEMP = ' '
         CALL CATDIR ('CSTA', KVOL, CNO, CHTM12, CHTM6, IDUM, CTEMP,
     *      IDUM, 'WRIT', BUFFER, IER)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('HICREA: ZCREAT ERROR',I7)
 1011 FORMAT ('HICREA: PROHIBITED FROM CREATING FILE ON DISK',I4)
 1030 FORMAT ('HICREA: CAN''T OPEN HI FILE - ERROR',I7)
      END

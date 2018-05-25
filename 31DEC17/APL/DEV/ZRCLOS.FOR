      SUBROUTINE ZRCLOS (LUN, FIND, IRET)
C-----------------------------------------------------------------------
C! close a connection to a server
C# IO-remote Z
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
C   Close a connection to a server.
C
C   Inputs:
C      LUN         I          Logical Unit Number.
C      FIND        I          Index of the FTAB entry for the opened
C                             channel (redundant).
C
C   Output:
C      IRET        I          Error return code:
C                                0 => channel closed
C                                1 => invalid LUN
C                                2 => LUN not open
C                                3 => system error
C                               99 => internal error
C
C   Generic version.
C-----------------------------------------------------------------------
      INTEGER LUN, FIND, IRET

      INTEGER FCBOFF
      INTEGER ITYP, INUM
      INTEGER IERR

      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DMSG.INC'

      INTEGER    MAXLUN
      PARAMETER (MAXLUN = 50)
C                                       Maximum valid LUN.
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check LUN range:
      IF ((LUN.LT.1) .OR. (LUN.GT.MAXLUN)) THEN
         IRET = 1
         WRITE (MSGTXT, 1000) LUN, MAXLUN
         CALL MSGWRT (6)
         END IF

      IF (IRET .EQ. 0) THEN
C                                       Check device type:
         IF ((DEVTAB(LUN).EQ.1) .OR. (DEVTAB(LUN).EQ.3)) THEN
            IRET = 1
            WRITE (MSGTXT, 1001) LUN, DEVTAB(LUN)
            CALL MSGWRT (6)
            END IF
         END IF

      IF (IRET .EQ. 0) THEN
C                                       Locate LUN entry in FTAB:
         CALL LSERCH ('SRCH', LUN, FIND, .FALSE., IERR)
         IF (IERR .EQ. 1) THEN
            IRET = 2
            WRITE (MSGTXT, 1002) LUN
            CALL MSGWRT (6)
         ELSE IF (IERR .NE. 0) THEN
            IRET = 99
            WRITE (MSGTXT, 1003) IERR
            CALL MSGWRT (6)
         ELSE
            FCBOFF = FIND + NMOFF
            END IF
         END IF

      IF (IRET. EQ. 0) THEN
C                                       Find the location of the channel
C                                       name (for error messages):
         CALL FSERCH (FIND, ITYP, INUM, IERR)
         IF (IERR .NE. 0) THEN
            IRET = 99
            WRITE (MSGTXT, 1004) IERR
            CALL MSGWRT (6)
            END IF
         END IF

      IF (IRET .EQ. 0) THEN
C                                       Close connection:
         CALL ZRCLS2 (FTAB(FCBOFF), IERR)
         IF (IERR .NE. 0) THEN
            IRET = 3
            WRITE (MSGTXT, 1005)
            CALL MSGWRT (6)
            CALL ZERROR ('ZRCLOS', FTAB(FCBOFF+FCBERR), NONNAM(INUM),
     *         FTAB(FCBOFF), .TRUE.)
            END IF
         END IF

      IF (IRET .EQ. 0) THEN
C                                       Destroy common file table entry:
         CALL LSERCH ('CLOS', LUN, FIND, .FALSE., IERR)
         IF (IERR .NE. 0) THEN
            IRET = 99
            WRITE (MSGTXT, 1006) IERR
            CALL MSGWRT (6)
            END IF
         END IF

      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZRCLOS: LUN ', I4, ' IS OUT OF RANGE (1 TO ', I2, ')')
 1001 FORMAT ('ZRCLOS: LUN ', I2, ' HAS WRONG TYPE (', I1, ')')
 1002 FORMAT ('ZRCLOS: LUN ', I2, ' IS NOT OPEN')
 1003 FORMAT ('ZRCLOS: LSERCH (SEARCH) RETURNS ERROR ', I2)
 1004 FORMAT ('ZRCLOS: FSERCH RETURNS ERROR ', I2)
 1005 FORMAT ('ZRCLOS: COULD NOT CLOSE CONNECTION')
 1006 FORMAT ('ZRCLOS: LSERCH (CLOSE) RETURNS ERROR ', I2)
      END

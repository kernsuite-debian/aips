      SUBROUTINE ZRXDWR (LUN, FIND, CODE, IRET)
C-----------------------------------------------------------------------
C! write expedited data to a server
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
C   Write one byte of expedited data to a server.
C
C   Inputs:
C      LUN          I         Logical unit number of connection.
C      FIND         I         Index of connection entry in FTAB
C                             (redundant).
C      CODE         I         Byte to be written.
C
C   Outputs:
C      IRET         I         Error code:
C                                0 => byte sent successfully
C                                1 => bad inputs
C                                2 => LUN not open
C                                3 => system error
C                                4 => partner closed connection
C                               99 => other error.
C
C   Generic version.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, CODE, IRET

      INTEGER   FCBOFF, ITYP, INUM, IERR

      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DMSG.INC'

      INTEGER    MAXLUN
      PARAMETER (MAXLUN = 50)
C-----------------------------------------------------------------------
      IRET = 0

C                                       Check inputs:
      IF ((CODE.LT.0) .OR. (CODE.GT.255)) THEN
         IRET = 1
         WRITE (MSGTXT, 1000)
         CALL MSGWRT (6)
         END IF
      IF (IRET .EQ. 0) THEN
         IF ((LUN.LT.1) .OR. (LUN.GT.MAXLUN)) THEN
            IRET = 1
            WRITE (MSGTXT, 1001) LUN, MAXLUN
            CALL MSGWRT (6)
            END IF
         END IF
      IF (IRET .EQ. 0) THEN
         IF ((DEVTAB(LUN).EQ.1) .OR. (DEVTAB(LUN).EQ.3)) THEN
            IRET = 1
            WRITE (MSGTXT, 1002) LUN, DEVTAB(LUN)
            CALL MSGWRT (6)
            END  IF
         END IF

      IF (IRET .EQ. 0) THEN
C                                       Find location of file control
C                                       block:
         CALL LSERCH ('SRCH', LUN, FIND, .FALSE., IERR)
         IF (IERR .EQ. 1) THEN
            IRET = 2
            WRITE (MSGTXT, 1003) LUN
            CALL MSGWRT (6)
         ELSE IF (IERR .NE. 0) THEN
            IRET = 99
            WRITE (MSGTXT, 1004) IERR
            CALL MSGWRT (6)
         ELSE
            FCBOFF = FIND + NMOFF
            END IF
         END IF

      IF (IRET .EQ. 0) THEN
C                                       Find the location of the channel
C                                       name (NONNAM(INUM)) for error
C                                       messages:
         CALL FSERCH (FIND, ITYP, INUM, IERR)
         IF (IERR .NE. 0) THEN
            IRET = 99
            WRITE (MSGTXT, 1005) IERR
            CALL MSGWRT (6)
            END IF
         END IF

      IF (IRET .EQ. 0) THEN
C                                       Write expedited data:
         CALL ZRXDW2 (FTAB(FCBOFF), CODE, IERR)
         IF (IERR .EQ. 2) THEN
            IRET = 4
            WRITE (MSGTXT, 1006)
            CALL MSGWRT (6)
         ELSE IF (IERR .NE. 0) THEN
            IRET = 3
            WRITE (MSGTXT, 1007)
            CALL MSGWRT (6)
            CALL ZERROR ('ZRXDWR', FTAB(FCBOFF+FCBERR), NONNAM(INUM),
     *         FTAB(FCBOFF), .TRUE.)
            END IF
         END IF

      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZRXDWR: CODE MUST BE IN RANGE 0 TO 255')
 1001 FORMAT ('ZRXDWR: LUN ', I4, ' OUT OF RANGE (1 TO ', I2, ')')
 1002 FORMAT ('ZRXDWR: LUN ', I2, ' HAS WRONG TYPE (', I1, ')')
 1003 FORMAT ('ZRXDWR: LUN ', I2, ' IS NOT OPEN')
 1004 FORMAT ('ZRXDWR: LSERCH (SEARCH) RETURNS ERROR ', I2)
 1005 FORMAT ('ZRXDWR: FSERCH RETURNS ERROR ', I2)
 1006 FORMAT ('ZRXDWR: PARTNER UNEXPECTED CLOSED CONNECTION')
 1007 FORMAT ('ZRXDWR: FAILED TO WRITE EXPEDITED DATA')
      END

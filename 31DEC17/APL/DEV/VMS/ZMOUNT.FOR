      SUBROUTINE ZMOUNT (MOUNT, IDRIVE, IDENS, MACHIN, RDRIVE, LMSG,
     *   SYSERR, IERR)
C-----------------------------------------------------------------------
C! mount or dismount magnetic tape device
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
C   Issue software mount or dismount for a given tape drive.
C   Inputs:
C      MOUNT    L       .TRUE. means mount, .FALSE. means dismount
C      IDRIVE   I       Tape drive number
C      IDENS    I       Density at which to mount tape (800, 1600, 6250)
C      MACHIN   C*(*)   Name of remote computer (iff TAPEn translates
C                       to REMOTE)
C      RDRIVE   I       Number of tape drive on remote machine (0->1)
C   Output:
C      LMSG     C*80    Message text: error or success (already
C                       reported)
C      SYSERR   I       System-level error code
C      IERR     I       Error return code: 0 => no error
C                          1 => error
C   VMS version - calls ZMOUN2 to do the real work.  It does not support
C   REMOTE tape drives.
C-----------------------------------------------------------------------
      LOGICAL   MOUNT
      INTEGER   IDRIVE, IDENS, RDRIVE, SYSERR, IERR
      CHARACTER MACHIN*(*),  LMSG*80
C
      INTEGER   IMOUNT, I, J, JTRIM
      CHARACTER HVAL*4, TAPLOG*8, XLATED*128
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      SYSERR = 0
C                                       Valid drive?
      IF ((IDRIVE.LE.0) .OR. (IDRIVE.GT.NTAPED)) THEN
         IERR = 1
         WRITE (MSGTXT,1000) IDRIVE
         GO TO 995
         END IF
C                                       Valid density for MOUNT?
      IF (MOUNT) THEN
         IMOUNT = 1
         IF ((IDENS.NE.800) .AND. (IDENS.NE.1600) .AND. (IDENS.NE.6250))
     *      THEN
            IERR = 1
            WRITE (MSGTXT,1010) IDENS
            GO TO 995
            END IF
         CALL ZEHEX (IDRIVE, 4, HVAL)
         TAPLOG = 'TAPE' // HVAL(4:4)
         I = 5
         CALL ZTRLOG (I, TAPLOG, 128, XLATED, J, IERR)
         IF (IERR.EQ.0) THEN
            IF (XLATED.EQ.'REMOTE') THEN
               IERR = 8
               MSGTXT = 'ZMOUNT: REMOTE TAPES NOT SUPPORT IN VMS'
               GO TO 995
               END IF
            END IF
         TPNAME(IDRIVE) = 'LOCAL'
C                                        Dismounts:
      ELSE
         IMOUNT = 0
         TPNAME(IDRIVE) = ' '
         END IF
C                                        Local tape dismount/mount
      MSGTXT = ' '
      CALL ZMOUN2 (IMOUNT, IDRIVE, IDENS, NTAPED, NPOPS, NLUSER,
     *   LMSG, SYSERR, IERR)
      J = JTRIM (LMSG)
      IF ((LMSG.NE.' ') .AND. (LMSG.NE.TSKNAM)) THEN
         IF ((IERR.NE.0) .OR. (SYSERR.NE.0)) THEN
            MSGTXT = LMSG
            CALL MSGWRT (8)
         ELSE
            MSGTXT = LMSG(:J) // ' on local host'
            CALL MSGWRT (3)
            END IF
         END IF
      IF (SYSERR.NE.0) CALL ZERROR ('ZMOUN2', SYSERR, ' ', -999,
     *   .FALSE.)
      GO TO 999
C
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZMOUNT: INVALID INTAPE = ',I6)
 1010 FORMAT ('ZMOUNT: INVALID DENSITY = ',I6)
      END

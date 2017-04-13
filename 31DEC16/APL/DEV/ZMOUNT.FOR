      SUBROUTINE ZMOUNT (MOUNT, IDRIVE, IDENS, MACHIN, RDRIVE, LMSG,
     *   SYSERR, IERR)
C-----------------------------------------------------------------------
C! mount or dismount magnetic tape device
C# Z Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2002, 2004
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
C   Generic version - calls ZMOUN2  or ZMOUNR to do the real work.  It
C   chooses ZMOUNR if the translation of TAPEn (n = IDRIVE) is "REMOTE".
C-----------------------------------------------------------------------
      LOGICAL   MOUNT
      INTEGER   IDRIVE, IDENS, RDRIVE, SYSERR, IERR
      CHARACTER MACHIN*(*), LMSG*80
C
      INTEGER   IMOUNT, I, J, JTRIM, JERR
      LOGICAL   REMOTE
      CHARACTER HVAL*4, TAPLOG*8, XLATED*256, LTPNAM*24
      HOLLERITH HMSG(20), HXLAT(64), HTAPL(2)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      SYSERR = 0
      LMSG = ' '
C                                       Valid drive?
      IF ((IDRIVE.LE.0) .OR. (IDRIVE.GT.NTAPED)) THEN
         IERR = 1
         WRITE (MSGTXT,1000) IDRIVE
         GO TO 995
         END IF
C                                       Valid density for MOUNT?
      IF (MOUNT) THEN
         IMOUNT = 1
         IF ((IDENS.NE.800) .AND. (IDENS.NE.1600) .AND. (IDENS.NE.6250)
     *      .AND. (IDENS.NE.22500)) THEN
            IERR = 1
            WRITE (MSGTXT,1010) IDENS
            GO TO 995
            END IF
         IF (TPNAME(IDRIVE).NE.' ') THEN
            MSGTXT = 'ZMOUNT: TAPE IS ALREADY MOUNTED'
            IERR = 2
            GO TO 995
            END IF
         CALL ZEHEX (IDRIVE, 4, HVAL)
         TAPLOG = 'TAPE' // HVAL(4:4)
         I = 5
         CALL ZTRLOG (I, TAPLOG, 256, XLATED, J, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ZMOUNT: UNABLE TO TRANSLATE LOGICAL NAME ' //
     *         TAPLOG
            GO TO 995
            END IF
         REMOTE = XLATED.EQ.'REMOTE'
C                                        Remote tape
         IF (REMOTE) THEN
            LTPNAM = MACHIN
            J = RDRIVE
            IF (J.LE.0) J = 1
            CALL ZEHEX (J, 4, HVAL)
            J = JTRIM (MACHIN)
            IF (J.LE.0) THEN
               MSGTXT = 'A REMOTE COMPUTER MUST BE SPECIFIED'
               IERR = 2
               GO TO 995
               END IF
            CALL CHLORU ('DOWN', J, LTPNAM)
            XLATED = 'aipsmt' // HVAL(4:4) // ':' // LTPNAM(:J)
C                                        Local tape
         ELSE
            LTPNAM = 'LOCAL'
            END IF
C                                        Create logical name
         TAPLOG(:4) = 'AMT0'
         J = JTRIM (XLATED)
         CALL CHR2H (8, TAPLOG, 1, HTAPL)
         CALL CHR2H (J, XLATED, 1, HXLAT)
         CALL ZCRLOG (I, HTAPL, J, HXLAT, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'UNABLE TO ASSIGN LOGICAL ' // TAPLOG(:I) // ' TO '
     *         // XLATED(:J)
            GO TO 995
            END IF
C                                        Remote tape mount
         MSGTXT = ' '
         IF (REMOTE) THEN
            CALL ZMOUNR (IMOUNT, IDRIVE, RDRIVE, IDENS, NPOPS, NLUSER,
     *         LMSG, SYSERR, IERR)
            J = JTRIM (LMSG)
            IF (LMSG.NE.' ') THEN
               IF ((IERR.NE.0) .OR. (SYSERR.NE.0)) THEN
                  MSGTXT = LMSG
                  CALL MSGWRT (8)
               ELSE
                  MSGTXT = LMSG(:J) // ' on host ' // LTPNAM
                  CALL MSGWRT (3)
                  END IF
               END IF
            IF (SYSERR.NE.0) THEN
               CALL ZERROR ('ZMOUNR', SYSERR, ' ', -999, .FALSE.)
               MSGTXT = LMSG
               END IF
C                                        Local tape mount
         ELSE
            LMSG = TSKNAM
            CALL ZMOUN2 (IMOUNT, IDRIVE, IDENS, NTAPED, NPOPS, NLUSER,
     *         HMSG, SYSERR, IERR)
            CALL H2CHR (80, 1, HMSG, LMSG)
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
            IF (SYSERR.NE.0) THEN
               CALL ZERROR ('ZMOUN2', SYSERR, ' ', -999, .FALSE.)
               MSGTXT = LMSG
               END IF
            END IF
         IF ((IERR.NE.0) .OR. (SYSERR.NE.0)) THEN
            IF ((SYSERR.EQ.0) .AND. (LMSG.EQ.' ')) THEN
               WRITE (MSGTXT,1020) IERR
               CALL MSGWRT (8)
               END IF
            XLATED = 'DISMOUNTED'
            I = JTRIM (TAPLOG)
            J = 10
            CALL CHR2H (8, TAPLOG, 1, HTAPL)
            CALL CHR2H (J, XLATED, 1, HXLAT)
            CALL ZCRLOG (I, HTAPL, J, HXLAT, IERR)
            IF (JERR.NE.0) THEN
               MSGTXT = 'UNABLE TO DEASSIGN LOGICAL ' // TAPLOG
               GO TO 995
               END IF
         ELSE
            TPNAME(IDRIVE) = LTPNAM
            END IF
C                                        Dismounts:
      ELSE
         IMOUNT = 0
         IF (TPNAME(IDRIVE).EQ.' ') THEN
            MSGTXT = 'ZMOUNT: TAPE IS NOT MOUNTED'
            IERR = 2
            GO TO 995
            END IF
         REMOTE = TPNAME(IDRIVE).NE.'LOCAL'
C                                        Remote tape dismount
         IF (REMOTE) THEN
            CALL ZMOUNR (IMOUNT, IDRIVE, RDRIVE, IDENS, NPOPS, NLUSER,
     *         LMSG, SYSERR, IERR)
            J = JTRIM (LMSG)
            IF (LMSG.NE.' ') THEN
               MSGTXT = LMSG
               CALL MSGWRT (8)
               END IF
            IF (SYSERR.NE.0) THEN
               CALL ZERROR ('ZMOUNR', SYSERR, ' ', -999, .FALSE.)
               MSGTXT = LMSG
               END IF
C                                        Local tape dismount
         ELSE
            LMSG = TSKNAM
            CALL ZMOUN2 (IMOUNT, IDRIVE, IDENS, NTAPED, NPOPS, NLUSER,
     *         HMSG, SYSERR, IERR)
            CALL H2CHR (80, 1, HMSG, LMSG)
            J = JTRIM (LMSG)
            IF (LMSG.EQ.TSKNAM) LMSG = ' '
            IF (LMSG.NE.' ') THEN
               MSGTXT = LMSG
               CALL MSGWRT (8)
               END IF
            IF (SYSERR.NE.0) THEN
               CALL ZERROR ('ZMOUN2', SYSERR, ' ', -999, .FALSE.)
               MSGTXT = LMSG
               END IF
            END IF
         IF ((IERR.NE.0) .AND. (SYSERR.EQ.0) .AND. (LMSG.EQ.' ')) THEN
            WRITE (MSGTXT,1020) IERR
            CALL MSGWRT (8)
            END IF
C                                       There is really little we can do
C                                       when dismounts fail, so clean up
         TPNAME(IDRIVE) = ' '
         CALL ZEHEX (IDRIVE, 4, HVAL)
         TAPLOG = 'AMT0' // HVAL(4:4)
         I = 5
         J = 10
         XLATED = 'DISMOUNTED'
         CALL CHR2H (8, TAPLOG, 1, HTAPL)
         CALL CHR2H (10, XLATED, 1, HXLAT)
         CALL ZCRLOG (I, HTAPL, J, HXLAT, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'UNABLE TO DEASSIGN LOGICAL ' // TAPLOG
            GO TO 995
            END IF
         IF (IERR.EQ.0) IERR = SYSERR
         END IF
      GO TO 999
C
 995  CALL MSGWRT (8)
      IF (IERR.EQ.0) IERR = SYSERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZMOUNT: INVALID INTAPE = ',I6)
 1010 FORMAT ('ZMOUNT: INVALID DENSITY = ',I6)
 1020 FORMAT ('ZMOUNT: ERROR',I6,' RETURNED BY ZMOUN2/ZMOUNR')
      END

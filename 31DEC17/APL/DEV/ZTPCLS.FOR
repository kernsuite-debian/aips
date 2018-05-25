      SUBROUTINE ZTPCLS (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! closes a tape device (real or pseudo-tape disk)
C# Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   Close the tape drive associated with LUN as well as its disk control
C   file removing any exclusive use state and clear the corresponding
C   FTAB entries.  ZTPCL2 actually closes the tape drive and ZDACLS is
C   called to close the disk control file.  Also closes sequential type
C   disk files via ZTPCLD.
C   Inputs:
C      LUN    I   Logical unit number = 30 + DriveNumber for tape
C      FIND   I   Index in FTAB to file control block for LUN
C   Output:
C      IERR   I   Error return code: 0 => no error
C                    1 => close error
C                    2 => non-zero LSERCH error
C                    3 => both 1 and 2
C                    4 => invalid LUN
C   Generic version.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   FCBOFF, IMAP, JERR, KERR, MTLUN, MTIND, I, J
      LOGICAL   T, F, TAPE
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA T, F /.TRUE., .FALSE./
      DATA IMAP /0/
C-----------------------------------------------------------------------
C                                       Check inputs.
      TAPE = (LUN.LT.129) .AND. (LUN.GE.129-NTAPED)
      IERR = 4
C                                       Proper device type?
      MTLUN = LUN - NTAPED
      IF ((TAPE) .AND. (DEVTAB(LUN).EQ.2)) GO TO 20
      IF ((.NOT.TAPE) .AND. (DEVTAB(LUN).EQ.0)) GO TO 20
         WRITE (MSGTXT,1010) LUN, DEVTAB(LUN)
         CALL MSGWRT (7)
         GO TO 999
C                                       Locate LUN in FTAB.
 20   CALL LSERCH ('SRCH', LUN, FIND, T, JERR)
      IF (JERR.EQ.0) GO TO 30
         IERR = 2
         WRITE (MSGTXT,1020) JERR, 'SRCH', LUN
         CALL MSGWRT (7)
         GO TO 999
C                                       Close tape drive.
 30   IERR = 0
C                                       Calculate file control block
C                                       offset in FTAB.
      FCBOFF = FIND + MOFF
C                                       Close tapes
      IF (TAPE) THEN
C                                       Remote tape
         IF (TPNAME(129-LUN).NE.'LOCAL') THEN
            CALL ZTPCLR (LUN, FIND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1030) IERR, 'REMOTE'
               CALL MSGWRT (7)
               END IF
C                                       Local tape
         ELSE
            CALL ZTPCL2 (FTAB(FCBOFF), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1030) IERR, 'LOCAL'
               CALL MSGWRT (7)
               CALL ZERROR ('ZTPCL2', FTAB(FCBOFF+FCBERR), ' ',
     *            FTAB(FCBOFF), T)
               END IF
            END IF
C                                       Clear FTAB entry for LUN.
         CALL LSERCH ('CLOS', LUN, FIND, T, JERR)
         IF (JERR.NE.0) THEN
            IERR = IERR + 2
            WRITE (MSGTXT,1020) JERR, 'CLOS', LUN
            CALL MSGWRT (7)
            END IF
C                                       Close tape control disk file.
 50      JERR = 0
         CALL LSERCH ('CLOS', MTLUN, MTIND, F, KERR)
         IF (KERR.NE.0) THEN
            JERR = 2
            WRITE (MSGTXT,1050) KERR, 'CLOS', MTLUN
            CALL MSGWRT (7)
            GO TO 900
            END IF
C                                       Close file.
 60      FCBOFF = MTIND + NMOFF
         CALL ZDACLS (FTAB(FCBOFF), IMAP, KERR)
         IF (KERR.NE.0) THEN
            JERR = JERR + 1
            WRITE (MSGTXT,1060) KERR
            CALL MSGWRT (7)
            CALL ZERROR ('ZDACLS', FTAB(FCBOFF+FCBERR), ' ',
     *         FTAB(FCBOFF), F)
            END IF
C                                       close sequential disk files
      ELSE
         IF (TPDNAM.NE.'LOCAL') THEN
            CALL ZTPCLR (LUN, FIND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1030) IERR, 'REMOTE'
               CALL MSGWRT (7)
               END IF
            TPDNAM = ' '
         ELSE
            CALL ZTPCLD (LUN, FTAB(FCBOFF), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) IERR
               CALL MSGWRT (7)
               CALL ZERROR ('ZTPCLD', FTAB(FCBOFF+FCBERR), ' ',
     *            FTAB(FCBOFF), T)
               END IF
            END IF
C                                       Clear FTAB entry for LUN.
         CALL LSERCH ('CLOS', LUN, FIND, T, JERR)
         IF (JERR.NE.0) THEN
            IERR = IERR + 2
            WRITE (MSGTXT,1020) JERR, 'CLOS', LUN
            CALL MSGWRT (7)
            END IF
         END IF
C                                       Maximize error return.
 900  IERR = MAX (JERR, IERR)
C                                       clear name
      CALL FSERCH (FIND, I, J, JERR)
      IF (JERR.EQ.0) THEN
         IF (I.EQ.1) DEVNAM(J) = ' '
         IF (I.EQ.2) NONNAM(J) = ' '
         IF (I.EQ.3) MAPNAM(J) = ' '
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ZTPCLS: IMPROPER DEVICE TYPE DEVTAB(',I3,') = ',I1,
     *   'FOR TAPE I/O')
 1020 FORMAT ('ZTPCLS: LSERCH ERROR = ',I3,' ON ',A4,' FOR LUN = ',I3)
 1030 FORMAT ('ZTPCLS: ERROR = ',I3,' CLOSING ',A,' TAPE DEVICE')
 1050 FORMAT ('ZTPCLS: LSERCH ERROR = ',I3,' ON ',A4,' FOR MTLUN = ',I3)
 1060 FORMAT ('ZTPCLS: ZDACLS RETURNS ERROR = ',I3)
 1100 FORMAT ('ZTPCLS: ZTPCLD RETURNS ERROR = ',I3)
      END

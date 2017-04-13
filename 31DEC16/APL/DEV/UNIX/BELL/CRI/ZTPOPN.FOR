      SUBROUTINE ZTPOPN (LUN, FIND, IVOL, PNAME, OPER, IERR)
C-----------------------------------------------------------------------
C! open tape or pseudo-tape device
C# Tape
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
C   Open a tape drive (as well as its corresponding disk control file)
C   for sequential, "map" (double buffered, asynchronous) I/O or open
C   a pseudo-tape sequential disk file.  Exclusive use and wait to open
C   are assumed.  Uses a 'TP' disk "lock" file for real tapes.
C   Inputs:
C      LUN     I       Logical unit number (30 < LUN <= 30 + NTAPED
C                         => tape, else disk)
C      IVOL    I       Tape drive or disk volume containing file
C      PNAME   C*48    tape disk physical file name
C      OPER    C*4     'READ' => read only or 'WRIT' => read/write
C   Output:
C      FIND    I       Index in FTAB to file control block for LUN
C      IERR    I       Error return code: 0 => no error
C                         1 => LUN already in use
C                         2 => file not found
C                         3 => volume not found
C                         4 => exclusive use denied
C                         5 => no room for LUN in FTAB
C                         6 => other open errors
C   UNICOS version.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IVOL, IERR
      CHARACTER PNAME*48, OPER*4
C
      INTEGER   JERR, DELMAX, DELCNT, DLMAX2, DLCNT2, MODE, MTLUN, I, J,
     *   MTIND, IMAP, IEXCL, FCBOFF
      REAL      TDELAY
      HOLLERITH ANAME(12)
      LOGICAL   T, F, TAPE
      CHARACTER MTNAM*48
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA T, F /.TRUE., .FALSE./
      DATA TDELAY, DELMAX, DLMAX2 /1.0, 30, 60/
      DATA IMAP /0/
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 6
C                                       Valid LUN?
      TAPE = (LUN.GT.30) .AND. (LUN.LE.30+NTAPED)
C                                       Proper device type?
      IF ((TAPE) .AND. (DEVTAB(LUN).EQ.2)) GO TO 20
      IF ((.NOT.TAPE) .AND. (DEVTAB(LUN).EQ.0)) GO TO 20
         WRITE (MSGTXT,1010) LUN, DEVTAB(LUN)
         GO TO 995
C                                       Valid opcode?
 20   IF ((OPER.EQ.'READ') .OR. (OPER.EQ.'WRIT')) GO TO 25
         WRITE (MSGTXT,1020) OPER
         GO TO 995
C                                       match tape LUN and IVOL
 25   IF ((.NOT.TAPE) .OR. (LUN.EQ.30+IVOL)) GO TO 30
         WRITE (MSGTXT,1025) IVOL, LUN
         GO TO 995
C                                       Real tapes:
C                                       Open tape control disk file.
 30   IF (.NOT.TAPE) GO TO 70
         CALL ZPHFIL ('TP', 1, 1, IVOL, MTNAM, IERR)
         MTLUN = LUN + 20
C                                       Allocate area in FTAB for MTLUN.
         CALL LSERCH ('OPEN', MTLUN, MTIND, F, JERR)
         IF (JERR.EQ.0) GO TO 40
C                                       MTLUN already in use.
            IF (JERR.EQ.2) THEN
               IERR = 1
               WRITE (MSGTXT,1031) MTLUN
C                                       No room in FTAB for MTLUN.
            ELSE IF (JERR.EQ.3) THEN
               IERR = 5
               WRITE (MSGTXT,1032) MTLUN
C                                       Other error
            ELSE
               WRITE (MSGTXT,1030) JERR, 'OPEN', MTLUN
               END IF
            GO TO 995
C                                       Okay, so far.
 40      IERR = 0
         DELCNT = 0
         DLCNT2 = 0
         FCBOFF = MTIND + NMOFF
         IEXCL = 1
C 50      CALL CHR2H (48, MTNAM, 1, ANAME)
 50      CALL ZDAOPN (FTAB(FCBOFF), MTNAM, IMAP, IEXCL, IERR)
         IF (IERR.EQ.0) GO TO 70
C                                       File not found
            IF (IERR.EQ.2) THEN
               WRITE (MSGTXT,1050) MTNAM(1:24)
               CALL MSGWRT (6)
C                                       Volume not found?
            ELSE IF (IERR.EQ.3) THEN
               WRITE (MSGTXT,1051) IVOL, MTNAM(1:24)
               CALL MSGWRT (6)
C                                       File busy?
            ELSE IF (IERR.EQ.4) THEN
C                                       Wait and try again every TDELAY
C                                       seconds up to DELMAX*DLMAX2
C                                       times.  Issue a message every
C                                       DLMAX2 wait.
               DELCNT = DELCNT + 1
               IF (DELCNT.LE.DELMAX) THEN
                  CALL ZDELAY (TDELAY, IERR)
               ELSE
                  DLCNT2 = DLCNT2 + 1
                  IF (DLCNT2.GT.DLMAX2) GO TO 53
                     DELCNT = 0
                     WRITE (MSGTXT,1052) MTNAM(1:24)
                     CALL MSGWRT (2)
                  END IF
               GO TO 50
C                                       No more patience
 53            WRITE (MSGTXT,1053) MTNAM(1:24)
               CALL MSGWRT (6)
C                                       Other open error.
 54         ELSE
               CALL ZERROR ('ZDAOPN', FTAB(FCBOFF+FCBERR), MTNAM,
     *            FTAB(FCBOFF), F)
               END IF
            GO TO 915
C                                       Allocate area in FTAB for LUN.
 70   CALL LSERCH ('OPEN', LUN, FIND, T, JERR)
      IF (JERR.EQ.0) GO TO 80
C                                       LUN already in use.
         IF (JERR.EQ.2) THEN
            IERR = 1
            WRITE (MSGTXT,1071) LUN
C                                       No room in FTAB for LUN.
         ELSE IF (JERR.EQ.3) THEN
            IERR = 5
            WRITE (MSGTXT,1072) MTLUN
         ELSE
            WRITE (MSGTXT,1070) JERR, 'OPEN', LUN
            END IF
         CALL MSGWRT (6)
         GO TO 910
C                                       Finally, open the tape drive.
 80   IERR = 0
      FCBOFF = FIND + MOFF
      MODE = 0
      IF (OPER.EQ.'WRIT') MODE = 2
C                                       Open real tape.
      IF (TAPE) THEN
         CALL ZTPOP2 (FTAB(FCBOFF), PNAME, MODE, IERR)
         IF (IERR.NE.0) THEN
C                                       Device not found?
            IF (IERR.EQ.2) THEN
               WRITE (MSGTXT,1080) PNAME(1:24)
               CALL MSGWRT (6)
C                                       No such logical?
            ELSE IF (IERR.EQ.3) THEN
               WRITE (MSGTXT,1081) PNAME(1:24)
               CALL MSGWRT (6)
C                                       Other open error.
            ELSE
               CALL ZERROR ('ZTPOP2', FTAB(FCBOFF+FCBERR), PNAME,
     *            FTAB(FCBOFF), T)
               END IF
            GO TO 905
            END IF
C                                       Pseudo-tape disk files.
      ELSE
         CALL ZTPOPD (LUN, FTAB(FCBOFF), PNAME, MODE, IERR)
         IF (IERR.NE.0) THEN
C                                       Device not found?
            IF (IERR.EQ.2) THEN
               WRITE (MSGTXT,1280) PNAME(1:24)
               CALL MSGWRT (6)
C                                       No such logical?
            ELSE IF (IERR.EQ.3) THEN
               WRITE (MSGTXT,1281) PNAME(1:24)
               CALL MSGWRT (6)
C                                       Other open error.
            ELSE
               CALL ZERROR ('ZTPOPD', FTAB(FCBOFF+FCBERR), PNAME,
     *            FTAB(FCBOFF), T)
               END IF
            GO TO 925
            END IF
         END IF
C                                       store file name
      CALL FSERCH (FIND, I, J, JERR)
      IF (JERR.EQ.0) THEN
         IF (I.EQ.1) DEVNAM(J) = PNAME
         IF (I.EQ.2) NONNAM(J) = PNAME
         IF (I.EQ.3) MAPNAM(J) = PNAME
         END IF
      GO TO 999
C                                       Error.  Close files ignoring
C                                       error returns and clear FTAB
C                                       entries for MTLUN and LUN.
 905  CALL LSERCH ('CLOS', LUN, FIND, T, JERR)
 910  CALL ZDACLS (FTAB(MTIND+NMOFF), IMAP, JERR)
 915  CALL LSERCH ('CLOS', MTLUN, MTIND, F, JERR)
      GO TO 999
 925  CALL LSERCH ('CLOS', LUN, FIND, T, JERR)
      GO TO 999
C
 995  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ZTPOPN: IMPROPER DEVICE TYPE DEVTAB(',I2,') = ',I1,
     *   'FOR TAPE I/O')
 1020 FORMAT ('ZTPOPN: INVALID OPERATION CODE = ',A4)
 1025 FORMAT ('ZTPOPN: TAPE NUMBER',I5,' DOESN''T MATCH LUN',I5)
 1030 FORMAT ('ZTPOPN: LSERCH ERROR ',I1,' FOR ',A4,' ON MTLUN = ',I2)
 1031 FORMAT ('ZTPOPN: MTLUN = ',I2,' ALREADY OPEN IN FTAB')
 1032 FORMAT ('ZTPOPN: NO ROOM IN FTAB FOR MTLUN = ',I2)
 1050 FORMAT ('ZTPOPN: TAPE CONTROL FILE = ',A24,' NOT FOUND')
 1051 FORMAT ('ZTPOPN: VOLUME = ',I2,' FOR FILE = ',A24,' NOT FOUND')
 1052 FORMAT ('ZTPOPN: STILL WAITING FOR FILE = ',A24)
 1053 FORMAT ('ZTPOPN: FILE = ',A24,' NOT AVAILABLE')
 1070 FORMAT ('ZTPOPN: LSERCH ERROR ',I1,' FOR ',A4,' ON LUN = ',I2)
 1071 FORMAT ('ZTPOPN: LUN = ',I2,' ALREADY OPEN IN FTAB')
 1072 FORMAT ('ZTPOPN: NO ROOM IN FTAB FOR LUN = ',I2)
 1080 FORMAT ('ZTPOPN: TAPE DEVICE = ',A24,' NOT FOUND')
 1081 FORMAT ('ZTPOPN: NO SUCH LOGICAL DEVICE = ',A24)
 1280 FORMAT ('ZTPOPN: DISK DEVICE = ',12A4,' NOT FOUND')
 1281 FORMAT ('ZTPOPN: NO SUCH LOGICAL DEVICE = ',A24)
      END

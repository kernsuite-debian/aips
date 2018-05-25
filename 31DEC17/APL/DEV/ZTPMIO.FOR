      SUBROUTINE ZTPMIO (OPER, LUN, FIND, NBYTES, BUFF, IBUFF, IERR)
C-----------------------------------------------------------------------
C! read/write tape devices with quick return IO methods
C# Z Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2001, 2004
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
C   Low level sequential access, large record, double buffered tape
C   device I/O.
C   Inputs:
C      OPER     C*4    Operation code 'READ' or 'WRIT'
C      LUN      I      Logical unit number == 129 - DriveNumber
C      FIND     I      Index in FTAB to file control block for LUN
C      NBYTES   I      Number of 8-bit bytes to transfer
C      BUFF     I(*)   I/O buffer
C      IBUFF    I      Buffer number to use (1 or 2)
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         1 => file not open
C                         2 => input error
C                         3 => I/O error
C                         4 => end of file (no messages)
C   Generic version: now if VMS uses one call seq for ZTPMID and
C   CHARACTER for OPER, else different call seq and HOLLERITH
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   LUN, FIND, NBYTES, BUFF(*), IBUFF, IERR
C
      INTEGER   FCBOFF, IEREOF
      LOGICAL   TAPE, T
      HOLLERITH HOPER
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA T /.TRUE./
      DATA IEREOF /4/
C-----------------------------------------------------------------------
C                                       Check inputs.
      TAPE = (LUN.LT.129) .AND. (LUN.GE.129-NTAPED)
      IERR = 2
C                                       Valid opcode?
      MSGTXT = ' '
      IF ((OPER.NE.'READ') .AND. (OPER.NE.'WRIT')) THEN
         WRITE (MSGTXT,1000) OPER
C                                       Valid # bytes requested?
      ELSE IF ((NBYTES.LT.0) .OR. (NBYTES.GE.32768)) THEN
         WRITE (MSGTXT,1020) NBYTES
C                                       Proper device type?
      ELSE IF (((.NOT.TAPE) .AND. (DEVTAB(LUN).EQ.2)) .OR.
     *   ((TAPE) .AND. (DEVTAB(LUN).EQ.0))) THEN
         WRITE (MSGTXT,1030) LUN, DEVTAB(LUN)
C                                       Proper buffer # specified?
      ELSE IF ((IBUFF.NE.1) .AND. (IBUFF.NE.2)) THEN
         WRITE (MSGTXT,1040) IBUFF
C                                       File open in FTAB?
      ELSE IF (FTAB(FIND).NE.LUN) THEN
         IERR = 1
         WRITE (MSGTXT,1050) LUN
         END IF
      IF (MSGTXT.NE.' ') GO TO 995
C                                       Calculate FTAB offset to file
C                                       control block for buffer #.
 60   FCBOFF = FIND + MOFF + (IBUFF - 1) * MFCB
      CALL CHR2H (4, OPER, 1, HOPER)
C                                       Real tape devices
      IF (TAPE) THEN
C                                       Remote tapes
         IF (TPNAME(129-LUN).NE.'LOCAL') THEN
            CALL ZTPMIR (OPER, LUN, FIND, NBYTES, BUFF, IBUFF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1060) OPER, LUN, IERR, 'REMOTE'
               CALL MSGWRT (7)
               END IF
C                                       Local tapes
         ELSE
            IF (SYSTYP.EQ.'VMS') THEN
               CALL ZTPMI2 (OPER, FTAB(FCBOFF), BUFF, NBYTES, IERR)
            ELSE
               CALL ZTPMI2 (HOPER, FTAB(FCBOFF), BUFF, NBYTES, IERR)
               END IF
C                                       Check for good start of I/O.
            IF ((IERR.NE.0) .AND. (IERR.NE.IEREOF)) THEN
               WRITE (MSGTXT,1060) OPER, LUN, NBYTES, 'TAPE'
               CALL MSGWRT (7)
               CALL ZERROR ('ZTPMI2', FTAB(FCBOFF+FCBERR), ' ',
     *            FTAB(FCBOFF), T)
               END IF
            END IF
C                                       Pseudo-tape disk files
      ELSE
C                                       Remote
         IF (TPDNAM.NE.'LOCAL') THEN
            CALL ZTPMIR (OPER, LUN, FIND, NBYTES, BUFF, IBUFF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1060) OPER, LUN, IERR, 'REMOTE'
               CALL MSGWRT (7)
               END IF
C                                       Local
         ELSE
            IF (SYSTYP.EQ.'VMS') THEN
               CALL ZTPMID (OPER, FIND, FTAB(FCBOFF), BUFF, NBYTES,
     *            IERR)
C                                       FTAB(FIND+5) = blkno
            ELSE
               CALL ZTPMID (HOPER, FTAB(FIND+5), FTAB(FCBOFF), BUFF,
     *            NBYTES, IERR)
               END IF
C                                       Check for good start of I/O.
            IF ((IERR.NE.0) .AND. (IERR.NE.IEREOF)) THEN
               WRITE (MSGTXT,1060) OPER, LUN, NBYTES, 'DISK'
               CALL MSGWRT (7)
               CALL ZERROR ('ZTPMID', FTAB(FCBOFF+FCBERR), ' ',
     *            FTAB(FCBOFF), T)
               END IF
            END IF
         END IF
      IF (IERR.EQ.3) THEN
         MSGTXT = 'This usually means PARITY ERROR'
      ELSE IF (IERR.EQ.5) THEN
         MSGTXT = 'This usually means unexpected BEGINNING-OF-TAPE'
      ELSE IF (IERR.EQ.6) THEN
         MSGTXT = 'This usually means you''re at END-OF-TAPE'
      ELSE
         GO TO 999
         END IF
C                                       Error
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTPMIO: INVALID OPERATION = ',A4)
 1020 FORMAT ('ZTPMIO: INVALID BYTE REQUEST = ',I6)
 1030 FORMAT ('ZTPMIO: IMPROPER DEVICE TYPE DEVTAB(',I3,') = ',I1,
     *   'FOR TAPE I/O')
 1040 FORMAT ('ZTPMIO: INVALID BUFFER NUMBER = ',I2)
 1050 FORMAT ('ZTPMIO: LUN = ',I3,' NOT OPEN IN FTAB')
 1060 FORMAT ('ZTPMIO: OPER = ',A4,' LUN = ',I3,' NBYTES = ',I6,' TO ',
     *   A)
      END

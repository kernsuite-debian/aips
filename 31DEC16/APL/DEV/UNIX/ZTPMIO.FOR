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
C                         if real tapes, other if disk
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
C   Generic UNIX version - different call seq to ZTPMID
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   LUN, FIND, NBYTES, BUFF(*), IBUFF, IERR
C
      INTEGER   FCBOFF, IEREOF
      LOGICAL   TAPE, T
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
      IF ((OPER.EQ.'READ') .OR. (OPER.EQ.'WRIT')) GO TO 10
         WRITE (MSGTXT,1000) OPER
         GO TO 995
C                                       Valid # bytes requested?
 10   IF ((NBYTES.GE.0) .AND. (NBYTES.LT.32768)) GO TO 30
         WRITE (MSGTXT,1020) NBYTES
         GO TO 995
C                                       Proper device type?
 30   IF ((TAPE) .AND. (DEVTAB(LUN).EQ.2)) GO TO 40
      IF ((.NOT.TAPE) .AND. (DEVTAB(LUN).EQ.0)) GO TO 40
         WRITE (MSGTXT,1030) LUN, DEVTAB(LUN)
         GO TO 995
C                                       Proper buffer # specified?
 40   IF ((IBUFF.EQ.1) .OR. (IBUFF.EQ.2)) GO TO 50
         WRITE (MSGTXT,1040) IBUFF
         GO TO 995
C                                       File open in FTAB?
 50   IF (FTAB(FIND).EQ.LUN) GO TO 60
         IERR = 1
         WRITE (MSGTXT,1050) LUN
         GO TO 995
C                                       Calculate FTAB offset to file
C                                       control block for buffer #.
 60   FCBOFF = FIND + MOFF + (IBUFF - 1) * MFCB
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
            CALL ZTPMI2 (OPER, FTAB(FCBOFF), BUFF, NBYTES, IERR)
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
C                                       FTAB(FIND+5) = blkno
            CALL ZTPMID (OPER, FTAB(FIND+5), FTAB(FCBOFF), BUFF, NBYTES,
     *         IERR)
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

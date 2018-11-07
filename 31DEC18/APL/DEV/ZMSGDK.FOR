      SUBROUTINE ZMSGDK (OPER, LUN, FIND, NREC, BUFF, IERR)
C-----------------------------------------------------------------------
C! disk IO to message file
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004, 2007, 2011, 2015
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
C   Transfer one 256-integer record to/from message disk file.  Like
C   ZFIO, but does not call MSGWRT to avoid recursion.
C   Inputs:
C      OPER     C*4     Operation code 'READ' or 'WRIT'
C      LUN      I       Logical unit number
C      FIND     I       Index in FTAB to file control block for LUN
C      NREC     I       Random access record number (1-relative)
C                       In units of 256-integer records.
C   In/out:
C      BUFF     I(256)   I/O buffer
C   Output:
C      IERR     I        Error return code: 0 => no error
C                           1 => file not open
C                           2 => input error
C                           3 => I/O error
C                           4 => end of file
C   Generic version - calls ZMSFI2
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   LUN, FIND, NREC, BUFF(256), IERR
C
      INTEGER   ERRLUN, FCBOFF, IEREOF, IBYTES
      LOGICAL   F
      HOLLERITH HOPER
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DSUMIO.INC'
      DATA F, ERRLUN, IEREOF /.FALSE., 12, 4/
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 2
C                                       Valid opcode?
      IF ((OPER.NE.'READ') .AND. (OPER.NE.'WRIT')) THEN
         WRITE (6,1000) OPER
C                                       Valid LUN.
      ELSE IF (LUN.NE.ERRLUN) THEN
         WRITE (6,1010) LUN
C                                       Valid record request?
      ELSE IF (NREC.LE.0) THEN
         WRITE (6,1020) NREC
C                                       File open in FTAB?
      ELSE IF (FTAB(FIND).NE.LUN) THEN
         IERR = 1
         WRITE (6,1030) LUN
      ELSE
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
C                                       Transfer record.
      IBYTES = 256 * (NBITWD / 8)
      IF (OPER.EQ.'READ') THEN
         NRCOUN(2) = NRCOUN(2) + 1
         NRBYTE(2) = NRBYTE(2) + IBYTES
      ELSE
         NWCOUN(2) = NWCOUN(2) + 1
         NWBYTE(2) = NWBYTE(2) + IBYTES
         END IF
      FCBOFF = FIND + NMOFF
      IF (SYSTYP.EQ.'VMS') THEN
         CALL ZMSFI2 (OPER, FTAB(FCBOFF), BUFF, NREC, IERR)
      ELSE
         CALL CHR2H (4, OPER, 1, HOPER)
         CALL ZMSFI2 (HOPER, FTAB(FCBOFF), BUFF, NREC, IERR)
         END IF
      IF ((IERR.NE.0) .AND. (IERR.NE.IEREOF)) THEN
         WRITE (6,1040) OPER, LUN, NREC
         CALL ZMSGER ('ZFI2', FTAB(FCBOFF+FCBERR), ' ', FTAB(FCBOFF), F)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (' ZMSGDK: INVALID OPERATION = ',A4)
 1010 FORMAT (' ZMSGDK: INVALID LUN = ',I2)
 1020 FORMAT (' ZMSGDK: INVALID RECORD REQUEST = ',I6)
 1030 FORMAT (' ZMSGDK: LUN = ',I2,' NOT OPEN IN FTAB')
 1040 FORMAT (' ZMSGDK: OPER = ',A4,' LUN = ',I2,' NREC = ',I8)
      END

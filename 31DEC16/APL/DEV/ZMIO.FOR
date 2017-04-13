      SUBROUTINE ZMIO (OPER, LUN, FIND, BLKNO, NBYTES, BUFF, IBUFF,
     *   IERR)
C-----------------------------------------------------------------------
C! random-access, quick return (double buffer) disk IO for large blocks
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2004, 2007-2008, 2011
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
C   Low level random access, large block, double buffered device I/O.
C   Inputs:
C      OPER     C*4   Operation code 'READ' or 'WRIT'
C      LUN      I     Logical unit number
C      FIND     I     Index in FTAB to file control block for LUN
C      BLKNO    I     Beginning virtual block number (1-relative).
C                     Block size is given by NBPS in /DCHCOM/.
C      NBYTES   I     Number of AIPS-bytes to transfer (an AIPS-byte is
C                     1/2 a local integer).
C      IBUFF    I     Buffer number to use (1 or 2)
C   In/out:
C      BUFF     I(*)  I/O buffer
C   Output:
C      IERR     I     Error return code: 0 => no error
C                        1 => file not open
C                        2 => input error
C                        3 => I/O error
C                        4 => end of file
C   Generic version - does SYSTYP='VMS' differently with double
C   buffering and CHARACTER to ZMI2 while UNIX does not actually do
C   double buffering since lower level routines are pure wait mode
C   routines anyway.
C   No longer performs I/O to TV devices (15MAR84), to tape devices
C   (15APR87), and to Tektronix devices (15JUL87) release.
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   LUN, FIND, BLKNO, NBYTES, BUFF(*), IBUFF, IERR
C
      INTEGER   ERRLUN, FCBOFF, IEREOF, IBYTES, BOFF
      LOGICAL   T
      HOLLERITH HOPER
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DSUMIO.INC'
      DATA T, ERRLUN, IEREOF /.TRUE., 12, 4/
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 2
C                                       Valid opcode?
      MSGTXT = ' '
      IF ((OPER.NE.'READ') .AND. (OPER.NE.'WRIT')) THEN
         WRITE (MSGTXT,1000) OPER
C                                       LUN in range?
      ELSE IF ((LUN.LE.0) .OR. (LUN.GT.128-NTAPED)) THEN
         WRITE (MSGTXT,1010) LUN
C                                       Does not handle message files.
      ELSE IF (LUN.EQ.ERRLUN) THEN
         WRITE (MSGTXT,1020) LUN
C                                       Does not handle TVs (as of
C                                       the 15MAR84 release).
      ELSE IF (DEVTAB(LUN).EQ.4) THEN
         WRITE (MSGTXT,1030)
C                                       Does not handle tapes (as of
C                                       the 15APR87 release).
      ELSE IF ((LUN.LT.129) .AND. (LUN.GE.129-NTAPED)) THEN
         WRITE (MSGTXT,1040)
C                                       Does not handle Tektronix I/O
C                                       (as of the 15JUL87 release).
      ELSE IF (LUN.EQ.7) THEN
         WRITE (MSGTXT,1050)
C                                       Valid block # requested?
      ELSE IF (BLKNO.LE.0) THEN
         WRITE (MSGTXT,1060) BLKNO
C                                       Valid # bytes requested?
C                                       128 Mwords for now
      ELSE IF ((NBYTES.LT.0) .OR. (NBYTES.GE.268435456)) THEN
         IBYTES = NBYTES * (NBITWD / 16)
         WRITE (MSGTXT,1070) IBYTES
C                                       Proper device type?
      ELSE IF (MOD (DEVTAB(LUN), 2).EQ.1) THEN
         WRITE (MSGTXT,1080) LUN, DEVTAB(LUN)
C                                       File open in FTAB?
      ELSE IF (FTAB(FIND).NE.LUN) THEN
         IERR = 1
         WRITE (MSGTXT,1090) LUN
C                                       Proper buffer # specified?
      ELSE IF ((IBUFF.NE.1) .AND. (IBUFF.NE.2)) THEN
         WRITE (MSGTXT,1100) IBUFF
         END IF
      IF (MSGTXT.NE.' ') GO TO 995
C                                       Calculate FTAB offset to file
C                                       control block for buffer #.
      IBYTES = NBYTES * (NBITWD / 16)
      IF (OPER.EQ.'READ') THEN
         NRCOUN(1) = NRCOUN(1) + 1
         NRBYTE(1) = NRBYTE(1) + IBYTES
      ELSE
         NWCOUN(1) = NWCOUN(1) + 1
         NWBYTE(1) = NWBYTE(1) + IBYTES
         END IF
      FCBOFF = FIND + MOFF
      IF (SYSTYP.EQ.'VMS') THEN
         BOFF = (IBUFF - 1) * MFCB
         CALL ZMI2 (OPER, FTAB(FCBOFF+BOFF), BUFF, BLKNO, NBYTES, IERR)
      ELSE
         CALL CHR2H (4, OPER, 1, HOPER)
         CALL ZMI2 (HOPER, FTAB(FCBOFF), BUFF, BLKNO, NBYTES, IERR)
         END IF
C                                       Do it.
C                                       Check for good start of I/O.
      IF ((IERR.NE.0) .AND. (IERR.NE.IEREOF)) THEN
         IBYTES = NBYTES * (NBITWD / 16)
         WRITE (MSGTXT,1900) OPER, LUN, BLKNO, IBYTES
         CALL MSGWRT (7)
         CALL ZERROR ('ZMI2', FTAB(FCBOFF+FCBERR), ' ', FTAB(FCBOFF), T)
         END IF
      GO TO 999
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZMIO: INVALID OPERATION = ',A4)
 1010 FORMAT ('ZMIO: LUN = ',I6,' OUT OF RANGE')
 1020 FORMAT ('ZMIO: ILLEGAL LUN = ',I2)
 1030 FORMAT ('ZMIO: DOES NOT PERFORM TV I/O AS OF 15MAR84')
 1040 FORMAT ('ZMIO: DOES NOT PERFORM TAPE I/O AS OF 15APR87')
 1050 FORMAT ('ZMIO: DOES NOT PERFORM TEKTRONIX I/O AS OF 15JUL87')
 1060 FORMAT ('ZMIO: INVALID BLOCK NUMBER = ',I12)
 1070 FORMAT ('ZMIO: INVALID BYTE REQUEST =',I10,' (8-BIT BYTES)')
 1080 FORMAT ('ZMIO: IMPROPER DEVICE TYPE DEVTAB(',I2,') = ',I2)
 1090 FORMAT ('ZMIO: LUN = ',I2,' NOT OPEN IN FTAB')
 1100 FORMAT ('ZMIO: INVALID BUFFER NUMBER = ',I2)
 1900 FORMAT ('ZMIO: OPER=',A4,' LUN=',I2,' BLKNO=',I12,' 8-BIT-BYTES=',
     *   I6)
      END

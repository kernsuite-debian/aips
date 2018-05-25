      SUBROUTINE ZTTBUF (BUF, BYTCNT, SYSERR, IERR)
C-----------------------------------------------------------------------
C! reads terminal input with no prompt or wait - simulates TV trackball
C# Z Terminal TV-IO
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
C   ZTTBUF dumps the contents (if any) of the input buffer into the
C   array BUF without issuing a prompt or waiting for input.  It issues
C   a call to ZQIOW with timeout set to zero.
C   Outputs:
C      BUF      C*(*)   Input buffer
C      BYTCNT   I       Number of bytes (characters) read
C      IOSB     I       System error code
C      IERR     I       Returned error code:  0 = OK
C                          2 = no access to terminal
C                          3 = IO error
C                          8 = channel not deassigned
C   This routine was written to simulate trackball buttons on
C   the keyboard.    Andy Lubenow   22 Nov 1983
C   VMS version
C-----------------------------------------------------------------------
      CHARACTER BUF*(*)
      INTEGER   BYTCNT, SYSERR, IERR
C
      INTEGER   ISTAT, TTCHAN, SYS$ASSIGN, SYS$DASSGN, SYS$QIOW,
     *   FUNC
      INTEGER*2 IOSB(4)
      HOLLERITH LBUF(20)
      INCLUDE '($IODEF)'
      INCLUDE '($SSDEF)'
C-----------------------------------------------------------------------
C                                       Open channel to terminal
      IERR = 2
      SYSERR = SYS$ASSIGN ('TT', TTCHAN,,)
      IF (SYSERR.NE.SS$_NORMAL) GO TO 999
C                                       Read from terminal
      FUNC = IOR (IO$_READVBLK, IO$M_TIMED)
      FUNC = IOR (FUNC, IO$M_NOECHO)
      SYSERR = SYS$QIOW ( , %VAL(TTCHAN), %VAL(FUNC), IOSB, , , LBUF,
     *   %VAL(80), %VAL(0), , , , )
      IF ((SYSERR.EQ.SS$_TIMEOUT) .OR. (SYSERR.EQ.SS$_NORMAL)) SYSERR =
     *   IOSB(1)
      IF ((SYSERR.EQ.SS$_TIMEOUT) .OR. (SYSERR.EQ.SS$_NORMAL)) THEN
         BYTCNT = IOSB(2)
         IERR = 0
      ELSE
         BYTCNT = 0
         IERR = 3
         END IF
      CALL H2CHR (BYTCNT, 1, LBUF, BUF)
C                                       deassign the TT
      SYSERR = SYS$DASSGN (%VAL(TTCHAN))
      IF ((SYSERR.NE.SS$_NORMAL) .AND. (IERR.EQ.0)) IERR = 8
C
 999  RETURN
      END

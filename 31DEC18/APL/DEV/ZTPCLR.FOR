      SUBROUTINE ZTPCLR (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! closes a remote tape device (real or pseudo-tape disk)
C# Tape Z2
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
C   Close the remote tape drive associated with LUN.
C   Inputs:
C      LUN    I   Logical unit number = 30 + DriveNumber for tape
C      FIND   I   Index in FTAB to file control block for LUN
C   Output:
C      IERR   I   Error return code: 0 => no error
C                    1 => close error
C                    2 => non-zero LSERCH error
C                    3 => both 1 and 2
C                    4 => invalid LUN
C   Generic version - calls ZVTPC2.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   IBUF(6), JERR, NBACK, NBACKE
      HOLLERITH HBUF(6)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      EQUIVALENCE (IBUF, HBUF)
      DATA NBACKE /0/
C-----------------------------------------------------------------------
C                                       send tape close
      CALL ZCLC8 (8, 'ZTPCLS  ', 1, HBUF)
      CALL ZILI32 (1, 1, 3, IBUF)
      CALL ZILI32 (1, 0, 4, IBUF)
      CALL ZILI32 (1, 0, 5, IBUF)
C                                       pack buffer: data
      CALL ZILI32 (1, LUN, 6, IBUF)
C
      NBACK = NBACKE
      CALL ZVTPX2 (FTAB(FIND+MOFF), 1, NBACK, IBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
      ELSE
         CALL ZI32IL (1, 5, IBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (7)
            END IF
         END IF
      IF ((IERR.EQ.0) .AND. (NBACK.NE.NBACKE)) THEN
         WRITE (MSGTXT,1015) NBACK, NBACKE
         CALL MSGWRT (6)
         END IF
C                                       Deassign socket
      CALL ZVTPC2 (FTAB(FIND+MOFF), JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1020) JERR
         CALL MSGWRT (7)
         END IF
      IF (IERR.EQ.0) IERR = JERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTPCLR: ZVTPX2 RETURNS ERROR CODE',I7)
 1010 FORMAT ('ZTPCLR: REMOTE TAPE SYSTEM RETURNS ERROR CODE',I7)
 1015 FORMAT ('ZTPCLR: WARNING - GOT BACK',I3,' WORDS, EXPECTED',I3)
 1020 FORMAT ('ZTPCLR: ZVTPC2 RETURNS ERROR',I4,' DEASSIGNING SOCKET')
      END

      SUBROUTINE ZTAPR (OP, LUN, FIND, COUNT, IERR)
C-----------------------------------------------------------------------
C! mount, dismount, position, write EOF, etc. for remote tapes
C# Z2 Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006
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
C   Performs standard tape manipulating functions - remote tapes
C   Inputs:
C       OP      C*4   Operation to be performed. 4 characters ASCII.
C                     'ADVF' = advance file marks
C                     'ADVR' = advance records
C                     'BAKF' = backspace file marks.
C                     'BAKR' = backspace records.
C                     'REWI' = rewind the tape on unit LUN
C                     'WEOF' = write end of file on unit LUN: writes 4
C                              EOFs, positions tape after first one
C                     'MEOF' = write 4 EOF marks on tape, position tape
C                              before the first one
C       LUN     I     logical unit number == 30 + DriveNumber
C       FIND    I     FTAB pointer.  Drive number for MOUNT/DISMOUNT.
C   In/out:
C       COUNT   I     In: Number of records or file marks to skip.
C                     Out: (AEOI) > 0 EOI after COUNT files
C                                 < 0 EOI after skipping -COUNT files
C   Outputs:
C       IERR  I       Error return: 0 => ok
C                        1 = File not open
C                        2 = Input specification error.
C                        3 = I/O error.
C                        4 = End Of File
C                        5 = Beginning Of Medium
C                        6 = End Of Medium
C   Generic version - calls ZVTPX2.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   LUN, FIND, COUNT, IERR
C
      HOLLERITH HBUF(10)
      INTEGER   IBUF(10), NBACK, NBACKE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      EQUIVALENCE (HBUF, IBUF)
      DATA NBACKE /1/
C-----------------------------------------------------------------------
C                                       pack buffer: header
      CALL ZCLC8 (8, 'ZTAPE   ', 1, HBUF)
      CALL ZILI32 (1, 3, 3, IBUF)
      CALL ZILI32 (1, 1, 4, IBUF)
      CALL ZILI32 (1, 0, 5, IBUF)
C                                       pack buffer: data
      CALL ZCLC8 (4, OP, 21, HBUF)
      CALL ZILI32 (1, LUN, 7, IBUF)
      CALL ZILI32 (1, COUNT, 8, IBUF)
      CALL ZILI32 (1, 0, 9, IBUF)
      CALL ZILI32 (1, 0, 10, IBUF)
C
      NBACK = NBACKE
      CALL ZVTPX2 (FTAB(FIND+MOFF), 3, NBACK, IBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
      ELSE
         CALL ZI32IL (1, 6, IBUF, COUNT)
         CALL ZI32IL (1, 5, IBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (7)
            END IF
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) OP, COUNT, LUN
         CALL MSGWRT (7)
         END IF
      IF ((IERR.EQ.0) .AND. (NBACK.NE.NBACKE)) THEN
         WRITE (MSGTXT,1025) NBACK, NBACKE
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTAPR: ZVTPX2 RETURNS ERROR CODE',I7)
 1010 FORMAT ('ZTAPR: REMOTE TAPE SYSTEM RETURNS ERROR CODE',I7)
 1020 FORMAT ('ZTAPR: ERROR WAS ON OP =''',A,''' COUNT=',I3,' LUN=',I3)
 1025 FORMAT ('ZTAPR: WARNING - GOT BACK',I3,' WORDS, EXPECTED',I3)
      END

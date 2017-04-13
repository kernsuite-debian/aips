      SUBROUTINE ZTPMIR (OPER, LUN, FIND, NBYTES, BUFF, IBUFF, IERR)
C-----------------------------------------------------------------------
C! read/write tape devices with quick return IO methods
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
C   Low level sequential access, large record, double buffered tape
C   device I/O.
C   Inputs:
C      OPER     C*4    Operation code 'READ' or 'WRIT'
C      LUN      I      Logical unit number == 30 + DriveNumber
C      FIND     I      Index in FTAB to file control block for LUN
C      NBYTES   I      Number of 8-bit bytes to transfer
C      IBUFF    I      Buffer number to use (1 or 2)
C   In/out:
C      BUFF     I(*)   I/O buffer - on out only those words actually
C                      read are changed, not all NBYTES.
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         1 => file not open
C                         2 => input error
C                         3 => I/O error
C                         4 => end of file (no messages)
C   Generic version - calls ZVTPX2
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   LUN, FIND, NBYTES, BUFF(*), IBUFF, IERR
C
      HOLLERITH HBUF(8197)
      INTEGER   IBUF(8197), NW, NR, I, IO, ND, FCBOFF, NBACK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      EQUIVALENCE (HBUF, IBUF)
C-----------------------------------------------------------------------
C                                       pack buffer: header
      CALL ZCLC8 (8, 'ZTPMIO  ', 1, HBUF)
      I = NBITWD / 8
      IF (OPER.EQ.'READ') THEN
         NW = 5
         NR = 4 + (NBYTES-1)/4
      ELSE
         NW = 6 + (NBYTES-1)/4
         NR = 3
         END IF
      NBACK = NR
      CALL ZILI32 (1, NW, 3, IBUF)
      CALL ZILI32 (1, NR, 4, IBUF)
      CALL ZILI32 (1, 0, 5, IBUF)
C                                       pack buffer: data
      CALL ZCLC8 (4, OPER, 21, HBUF)
      CALL ZILI32 (1, LUN, 7, IBUF)
      CALL ZILI32 (1, NBYTES, 8, IBUF)
      CALL ZILI32 (1, IBUFF, 9, IBUF)
      CALL ZILI32 (1, FTAB(FIND+5), 10, IBUF)
      IO = 39 / I + 2
      ND = (NBYTES - 1) / I + 1
      IF (OPER.EQ.'WRIT') CALL COPY (ND, BUFF, IBUF(IO))
C
      FCBOFF = FIND + MOFF + (IBUFF-1) * MFCB
      NBACK = NR
      CALL ZVTPX2 (FTAB(FCBOFF), NW, NBACK, IBUF, IERR)
      CALL ZI32IL (1, 6, IBUF, FTAB(FCBOFF+FCBERR))
      CALL ZI32IL (1, 7, IBUF, FTAB(FCBOFF+FCBXFR))
      CALL ZI32IL (1, 8, IBUF, FTAB(FIND+5))
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
      ELSE
         CALL ZI32IL (1, 5, IBUF, FTAB(FCBOFF+FCBSP4))
         IO = 31 / I + 2
         ND = (FTAB(FCBOFF+FCBXFR) - 1) / I + 1
         IF (OPER.EQ.'READ') CALL COPY (ND, IBUF(IO), BUFF)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTPMIR: ZVTPX2 RETURNS ERROR CODE',I7)
      END

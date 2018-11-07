      SUBROUTINE ZVTPRC (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! closes channel in server (real-tape) to client (virtual-tape)
C# Tape Z
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   Close the virtual tape device channel: from receiver point of view
C   Inputs:
C      LUN    I   Logical unit number
C      FIND   I   Index in FTAB to file control block for LUN
C   Output:
C      IERR   I   Error return code: 0 => no error
C                    1 => close error or flush error
C                    2 => file already closed in FTAB
C                    3 => both errors
C                    4 => erroneous LUN
C   Generic version - uses ZVTPC3.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   JERR
      LOGICAL   F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Locate LUN in FTAB.
 20   IERR = 0
      CALL LSERCH ('SRCH', LUN, FIND, F, JERR)
C                                       LUN already closed in FTAB.
      IF (JERR.NE.0) THEN
         IERR = 2
         WRITE (MSGTXT,1020) LUN
         GO TO 995
         END IF
C                                       Deassign socket
 30   CALL ZVTPC3 (FTAB(FIND+NMOFF), JERR)
      IF (JERR.NE.0) THEN
         IERR = IERR + 2
         WRITE (MSGTXT,1030) JERR
         CALL MSGWRT (7)
         END IF
C                                       Clear FTAB entry.
 40   CALL LSERCH ('CLOS', LUN, FIND, F, JERR)
C                                       LUN already closed in FTAB.
      IF (JERR.EQ.0) GO TO 999
         IERR = IERR + 2
         WRITE (MSGTXT,1040) LUN
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ZVTPRC: LUN = ',I2,' ALREADY CLOSED IN FTAB ON SEARCH')
 1030 FORMAT ('ZVTPRC/ZVTPC3 ERROR',I3,' DEASSIGNING THE TAPE SOCKET')
 1040 FORMAT ('ZVTPRC: LUN = ',I2,' ALREADY CLOSED IN FTAB ON CLOSE')
      END

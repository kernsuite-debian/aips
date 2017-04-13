      SUBROUTINE ZSSSCL (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! close the SUN-Screen-Server device
C# TV-IO
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   Close the SUNview AIPS TV device associated with LUN and clear its
C   FTAB entry.
C   Inputs:
C      LUN    I   Logical unit number
C      FIND   I   Index in FTAB to file control block for LUN
C   Output:
C      IERR   I   Error return code: 0 => no error
C                    1 => close error or flush error
C                    2 => file already closed in FTAB
C                    3 => both errors
C                    4 => erroneous LUN
C   SUN - SSS (SUNview Screen Server) version.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   JERR, TVDEVN
      LOGICAL   F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA F /.FALSE./
      DATA TVDEVN /4/
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 4
C                                       Valid LUN?
      IF (LUN.EQ.TVLUN) GO TO 10
         WRITE (MSGTXT,1000) LUN
         GO TO 995
C                                       Valid device type?
 10   IF (DEVTAB(LUN).EQ.TVDEVN) GO TO 20
         WRITE (MSGTXT,1010) LUN, DEVTAB(LUN)
         GO TO 995
C                                       Locate LUN in FTAB.
 20   IERR = 0
      CALL LSERCH ('SRCH', LUN, FIND, F, JERR)
C                                       LUN already closed in FTAB.
      IF (JERR.EQ.0) GO TO 30
         IERR = 2
         WRITE (MSGTXT,1020) LUN
         GO TO 995
C                                       Deassign socket
 30   CALL ZSSSC2 (FTAB(FIND+NMOFF), JERR)
      IF (JERR.NE.0) THEN
         IERR = IERR + 2
         WRITE (MSGTXT,1030)
         CALL MSGWRT (7)
         END IF
      TVBROK = 0
C                                       Clear FTAB entry.
 40   CALL LSERCH ('CLOS', LUN, FIND, F, JERR)
C                                       LUN already closed in FTAB.
      IF (JERR.EQ.0.) GO TO 999
         IERR = IERR + 2
         WRITE (MSGTXT,1040) LUN
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZSSSCL: INVALID LUN = ',I2)
 1010 FORMAT ('ZSSSCL: DEVTAB(',I2,') = ',I1,' = INVALID DEVICE TYPE')
 1020 FORMAT ('ZSSSCL: LUN = ',I2,' ALREADY CLOSED IN FTAB ON SEARCH')
 1030 FORMAT ('ZSSSCL: ZSSSC2 RETRUNS ERROR DEASSIGNING THE TV SOCKET')
 1040 FORMAT ('ZSSSCL: LUN = ',I2,' ALREADY CLOSED IN FTAB ON CLOSE')
      END

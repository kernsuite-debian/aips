      SUBROUTINE ZM70CL (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! close an IIS Model 70 TV device, flushing any buffer
C# IO-TV
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
C   Close the IIS model 70 TV device associated with LUN and clear its
C   FTAB entry.
C   Inputs:
C      LUN     I   Logical unit number
C      FIND    I   Index in FTAB to file control block for LUN
C   Output:
C      IERR    I   Error return code: 0 => no error
C                              1 => close error or flush error
C                              2 => file already closed in FTAB
C                              3 => both errors
C                              4 => erroneous LUN
C   Generic version.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   IDUM, JERR, TVLUN, TVDEVN, FCBOFF, I, J
      LOGICAL   F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA F /.FALSE./
      DATA TVLUN, TVDEVN /9, 4/
C-----------------------------------------------------------------------
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
      IF (JERR.EQ.0) GO TO 30
         IERR = 2
         WRITE (MSGTXT,1020) LUN
         GO TO 995
C                                       Flush buffers.
 30   CALL ZM70XF ('FLSH', IDUM, IDUM, IDUM, JERR)
      IF (JERR.EQ.0) GO TO 40
         IERR = 1
         WRITE (MSGTXT,1030) JERR
C                                       Close the device.
 40   FCBOFF = FIND + NMOFF
      CALL ZM70C2 (FTAB(FCBOFF), JERR)
      IF (JERR.EQ.0) GO TO 50
         IERR = 1
         WRITE (MSGTXT,1040) JERR
         CALL MSGWRT (7)
         CALL ZERROR ('ZM70C2', FTAB(FCBOFF+FCBERR), ' ', FTAB(FCBOFF),
     *      F)
C                                       Clear FTAB entry.
 50   CALL LSERCH ('CLOS', LUN, FIND, F, JERR)
      IF (JERR.EQ.0) THEN
         CALL FSERCH (FIND, I, J, JERR)
         IF (JERR.EQ.0) THEN
            IF (I.EQ.1) DEVNAM(J) = ' '
            IF (I.EQ.2) NONNAM(J) = ' '
            IF (I.EQ.3) MAPNAM(J) = ' '
            END IF
         GO TO 999
      ELSE
         IERR = IERR + 2
         WRITE (MSGTXT,1050) LUN
         END IF
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZM70CL: INVALID LUN = ',I4)
 1010 FORMAT ('ZM70CL: DEVTAB(',I2,') = ',I2,' = INVALID DEVICE TYPE')
 1020 FORMAT ('ZM70CL: LUN = ',I2,' ALREADY CLOSED IN FTAB ON SEARCH')
 1030 FORMAT ('ZM70CL: ZM70XF RETURNS ERROR = ',I6)
 1040 FORMAT ('ZM70CL: ZM70C2 RETURNS ERROR = ',I6)
 1050 FORMAT ('ZM70CL: LUN = ',I2,' ALREADY CLOSED IN FTAB ON CLOSE')
      END

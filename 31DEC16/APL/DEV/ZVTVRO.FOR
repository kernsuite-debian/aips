      SUBROUTINE ZVTVRO (LUN, IND, IERR)
C-----------------------------------------------------------------------
C! open scket in server (real-TV) to any client (virtual-TV)
C# TV-IO
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   ZVTVRO does whatever is needed to enable communication from the
C   current program (TVMON) on a machine that has a real TV display
C   attached to any remote machine needing the display.
C   This is a generic upper level Z routine.
C   Inputs:
C      LUN    I    An LUN to use (not TVLUN or TVLUN2)
C   Output:
C      IND    I    FTAB location opened
C      IERR   I    Error code: 0 => ok
C                     1 = LUN already in use
C                     2 = file not found
C                     3 = volume not found
C                     4 = excl requested but not available
C                     5 = no room for lun
C                     6 = other open errors
C-----------------------------------------------------------------------
      INTEGER   LUN, IND, IERR
C
      LOGICAL   F
      INTEGER   ERRLUN, ERRTER, IER
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA F /.FALSE./
      DATA ERRLUN, ERRTER /12, 6/
C-----------------------------------------------------------------------
      IERR = 6
      IF ((LUN.EQ.ERRLUN) .OR. (LUN.EQ.ERRTER)) GO TO 980
      IF ((LUN.LE.0) .OR. (LUN.GT.50)) GO TO 980
      IERR = 0
C                                       allocate table
      CALL LSERCH ('OPEN', LUN, IND, F, IER)
      IF (IER.EQ.0) GO TO 20
         IF (IER.NE.2) GO TO 10
            IERR = 1
            WRITE (MSGTXT,1000) LUN
            GO TO 990
 10      IF (IER.NE.3) GO TO 20
            IERR = 5
            WRITE (MSGTXT,1010) LUN
            GO TO 990
C                                       non-disk devices
 20   CALL ZVTVO3 (FTAB(IND+NMOFF), IER)
      IF (IER.EQ.0) GO TO 999
         IERR = 6
         WRITE (MSGTXT,1020) LUN
C                                       Errors
C                                       remove FTAB
 900  CALL LSERCH ('CLOS', LUN, IND, F, IER)
      IF (IERR.EQ.6) GO TO 990
C                                       illegal device
 970  WRITE (MSGTXT,1970) LUN
      GO TO 990
C                                       illegal lun
 980  WRITE (MSGTXT,1980) LUN
C                                       print msg
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZVTVRO: LUN',I4,' ALREADY IN USE')
 1010 FORMAT ('ZVTVRO: LUN TABLE FULL, CAN''T FIT LUN',I4)
 1020 FORMAT ('ZVTVRO: CHANNEL ASSIGN ERROR FOR REMOTE TV LUN ',I4)
 1970 FORMAT ('ZVTVRO: CALLED FOR NON-TV DEVICE WITH LUN',I4)
 1980 FORMAT ('ZVTVRO: LUN',I6,' ILLEGAL')
      END

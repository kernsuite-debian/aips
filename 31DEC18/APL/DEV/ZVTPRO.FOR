      SUBROUTINE ZVTPRO (SOCKET, LUN, THOSTS, THLEN, NTH, IND, IERR)
C-----------------------------------------------------------------------
C! open socket in server (real-tape) to any client (virtual-tape)
C# Tape Z
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2004
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
C   ZVTPRO does whatever is needed to enable communication from the
C   current program (TPMON) on a machine that has a real tape units
C   attached to any remote machine needing the service.
C   Inputs:
C      SOCKET   C*8    Socket name (aipsmtm:)
C      LUN      I      An LUN to use (not 25 nor 31-40 or so)
C      THOSTS   H(*)   Array of allowed host names, size THLEN * NTH
C      THLEN    I      Length of each hostname string in THOSTS
C      NTH      I      Actual number of hosts.
C   Output:
C      IND      I     FTAB location opened
C      IERR     I     Error code: 0 => ok
C                        1 = LUN already in use
C                        2 = file not found
C                        3 = volume not found
C                        4 = excl requested but not available
C                        5 = no room for lun
C                        6 = other open errors
C   This is a generic upper level Z routine.
C-----------------------------------------------------------------------
      CHARACTER SOCKET*(*)
      INTEGER   LUN, THLEN, NTH, IND, IERR
C                                       See also TPMON.FOR local include
      HOLLERITH THOSTS(*)
C
      LOGICAL   F
      INTEGER   ERRLUN, ERRTER, IER, I, ITRIM
      HOLLERITH HSOCK(12)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA F /.FALSE./
      DATA ERRLUN, ERRTER /12, 6/
C-----------------------------------------------------------------------
      IERR = 6
      IF ((LUN.EQ.ERRLUN) .OR. (LUN.EQ.ERRTER)) GO TO 980
      IF ((LUN.LE.0) .OR. (LUN.GT.128)) GO TO 980
      IERR = 0
C                                       allocate table
      CALL LSERCH ('OPEN', LUN, IND, F, IER)
      IF (IER.EQ.2) THEN
         IERR = 1
         WRITE (MSGTXT,1000) LUN
         GO TO 990
      ELSE IF (IER.EQ.3) THEN
         IERR = 5
         WRITE (MSGTXT,1010) LUN
         GO TO 990
         END IF
C                                       service name
 20   I = ITRIM (SOCKET)
      CALL CHR2H (I, SOCKET, 1, HSOCK)
      CALL ZVTPO3 (I, HSOCK, THOSTS, THLEN, NTH, FTAB(IND+NMOFF), IER)
      IF (IER.NE.0) THEN
         IERR = 6
         WRITE (MSGTXT,1020) LUN
         CALL MSGWRT (6)
C                                       remove FTAB
         CALL LSERCH ('CLOS', LUN, IND, F, IER)
         IND = 0
         END IF
      GO TO 999
C                                       Errors
C                                       illegal lun
 980  WRITE (MSGTXT,1980) LUN
C                                       print msg
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZVTPRO: LUN',I4,' ALREADY IN USE')
 1010 FORMAT ('ZVTPRO: LUN TABLE FULL, CAN''T FIT LUN',I4)
 1020 FORMAT ('ZVTPRO: CHANNEL ASSIGN ERROR FOR REMOTE TAPE LUN ',I4)
 1980 FORMAT ('ZVTPRO: LUN',I6,' ILLEGAL')
      END

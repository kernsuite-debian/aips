      SUBROUTINE YCNECT (X1, Y1, X2, Y2, IC, LBUFF, IERR)
C-----------------------------------------------------------------------
C! write line segment between 2 points on TV
C# Y0 TV-util
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
C   YCNECT writes a line segment on the TV.  This version will work on
C   all TVs.  It is called a Y routine to allow the use of hardware
C   vector generators on those TVs equiped with them.
C   Inputs:
C      X1      I      start X position
C      Y1      I      start Y position
C      X2      I      end X position
C      Y2      I      end Y position
C      IC      I      Channel (1 to NGRAY+NGRAPH)
C      LBUFF   I(*)   Buffer contains desired intensity (size > max
C                     horizontal or vertical line, e.g., 1280)
C   Output:
C      IERR    I      error code : 0 => ok
C   Virtual TV version for TV by COMMUNICATION
C-----------------------------------------------------------------------
      INTEGER    X1, Y1, X2, Y2, IC, LBUFF(*), IERR
C
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       test
      IF ((X1.LT.1) .OR. (X1.GT.MAXXTV(1))) GO TO 900
      IF ((Y1.LT.1) .OR. (Y1.GT.MAXXTV(2))) GO TO 900
      IF ((X2.LT.1) .OR. (X2.GT.MAXXTV(1))) GO TO 900
      IF ((Y2.LT.1) .OR. (Y2.GT.MAXXTV(2))) GO TO 900
      IF ((IC.LT.1) .OR. (IC.GT.NGRAY+NGRAPH)) GO TO 900
C                                       doit
      BUFFER(1) = X1
      BUFFER(2) = Y1
      BUFFER(3) = X2
      BUFFER(4) = Y2
      BUFFER(5) = IC
      BUFFER(6) = LBUFF(1)
      BUFSW = 6
      BUFSR = 0
      CALL MKYBUF ('WRIT', 'YCNECT', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
      GO TO 999
C                                       error
 900  WRITE (MSGTXT,1900) IC, X1, X2, Y1, Y2
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('YCNECT: BAD CHAN, X1,X2,Y1,Y2:',5I7)
      END

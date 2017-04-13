      SUBROUTINE YZOOMC (MAG, XZOOM, YZOOM, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! set the TV zoom magnification and center
C# Y1 TV-basic
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
C   YZOOM writes (ONLY!!!!) the zoom control registers of the TV
C   Inputs:
C      MAG     I     0-3 for magnification 1,2,4,8 times, resp.
C      XZOOM   I     X center of expansion (1-512, 1 => LHS)
C      YZOOM   I     Y center of expansion (1-512, 1 => bot)
C      VRTRTC  L     Do on vertical retrace only?
C   Output:
C      IERR    I     error code of Z...XF: 0 -> ok, 2 -> input error
C   YZOOMC updates the /TVDEV/ common TVZOOM parameter
C   Virtual TV version (for TV by communication).
C-----------------------------------------------------------------------
      INTEGER   MAG, XZOOM, YZOOM, IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      IERR = 2
      IF ((MAG.LT.0) .OR. (MAG.GT.ABS(MXZOOM))) GO TO 999
      IF ((XZOOM.LT.1) .OR. (XZOOM.GT.MAXXTV(1))) GO TO 999
      IF ((YZOOM.LT.1) .OR. (YZOOM.GT.MAXXTV(2))) GO TO 999
C                                        set data
      BUFFER(2) = MAG
      BUFFER(3) = XZOOM
      BUFFER(4) = YZOOM
      BUFFER(1) = 0
      IF (VRTRTC) BUFFER(1) = 1
C                                        header + IO
      BUFSW = 4
      BUFSR = 0
      CALL MKYBUF ('WRIT', 'YZOOMC', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                        update common /TVCHAR/
      IF (IERR.EQ.0) THEN
         TVZOOM(1) = MAG
         TVZOOM(2) = XZOOM
         TVZOOM(3) = YZOOM
         END IF
C
 999  RETURN
      END

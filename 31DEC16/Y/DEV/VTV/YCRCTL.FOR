      SUBROUTINE YCRCTL (OP, ON, X, Y, LINKX, LINKY, RBLINK, BUTTON,
     *   VRTRTC, IERR)
C-----------------------------------------------------------------------
C! controls the TV cursor visibility, position; reads trackball buttons
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
C   YCRCTL reads/writes the cursor/trackball control register of TV
C   Inputs:
C      OP      C*4   'READ' from TV or 'WRIT' to TV
C      VRTRTC  L     T => do on vertical retrace only
C   In/out:
C      ON      L     T => cursor visible, F => off
C      X       I     X position cursor center (1-512, 1 => LHS)
C      Y       I     Y position cursor center (1-512, 1 => bot)
C      LINKX   L     T => trackball moves cursor in X
C      LINKY   L     T => trackball moves cursor in Y
C      RBLINK  I     rate of cursor blink: 0-3 no-fast blink
C   Output:
C      BUTTON  I     button value (0 - 15)
C      IERR    I     error code of Z...XF : 0 => ok
C                                           2 => input error
C   Virtual TV (by communication) version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   X, Y, RBLINK, BUTTON, IERR
      LOGICAL   ON, LINKX, LINKY, VRTRTC
C
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      BUFFER(1) = 0
      IF (VRTRTC) BUFFER(1) = 1
      IF (OP.EQ.'WRIT') THEN
         BUFFER(2) = 0
         IF (ON) BUFFER(2) = 1
         BUFFER(3) = X
         BUFFER(4) = Y
         BUFFER(5) = 0
         IF (LINKX) BUFFER(5) = 1
         BUFFER(6) = 0
         IF (LINKY) BUFFER(6) = 1
         BUFFER(7) = RBLINK
         BUFFER(8) = BUTTON
         BUFSW = 8
         BUFSR = 0
      ELSE
         BUFSW = 1
         BUFSR = 8
         END IF
      CALL MKYBUF (OP, 'YCRCTL', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                        return answers
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) THEN
         CALL UNYBUF (BUFSR, BUFFER, HEADER)
         ON = BUFFER(2).EQ.1
         X = BUFFER(3)
         Y = BUFFER(4)
         LINKX = BUFFER(5).EQ.1
         LINKY = BUFFER(6).EQ.1
         RBLINK = BUFFER(7)
         BUTTON = BUFFER(8)
         END IF
C
 999  RETURN
      END

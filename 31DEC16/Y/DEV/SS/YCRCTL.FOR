      SUBROUTINE YCRCTL (OP, ON, X, Y, LINKX, LINKY, RBLINK, BUTTON,
     *   VRTRTC, IERR)
C-----------------------------------------------------------------------
C! controls the TV cursor visibility, position; reads trackball buttons
C# TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   YCRCTL reads/writes the cursor/trackball control register of TV
C   Inputs:
C      OP      C*4   'READ' from TV or 'WRIT' to TV
C      VRTRTC  L     T => do on vertical retrace only
C   In/out:
C      ON      L     T => cursor visible, F => off
C      X       I     X position cursor center (1-MAXXTV(1), 1 => LHS)
C      Y       I     Y position cursor center (1-MAXXTV(2), 1 => bot)
C      LINKX   L     T => trackball moves cursor in X
C      LINKY   L     T => trackball moves cursor in Y
C      RBLINK  I     rate of cursor blink: 0-3 no-fast blink
C   Output:
C      BUTTON  I     button value (0 - 15)
C      IERR    I     error code of Z...XF : 0 => ok
C                                           2 => input error
C  Version for SUN Screen Server
C  Brian Glendenning; mod E Greisen
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   X, Y, RBLINK, BUTTON, IERR
      LOGICAL   ON, LINKX, LINKY, VRTRTC
C
      INTEGER   OPCODE, DAT(4), NWSD, TMPBUF(6)
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                        write: check input
      IERR = 2
      IF (OP.EQ.'WRIT') THEN
         IF ((X.LT.1) .OR. (X.GT.MAXXTV(1))) GO TO 999
         IF ((Y.LT.1) .OR. (Y.GT.MAXXTV(2))) GO TO 999
         OPCODE = 63
         DAT(1) = X
         DAT(2) = Y
         CALL ZSSSXF (OPCODE, DAT, 0, TMPBUF, NWSD, IERR)
C                                        read cursor, then button
      ELSE IF (OP.EQ.'READ') THEN
         OPCODE = 64
         CALL ZSSSXF (OPCODE, DAT, 0, TMPBUF, NWSD, IERR)
         IF (IERR.EQ.0) THEN
            CALL ZI16IL (6, 1, TMPBUF, TMPBUF)
            X = TMPBUF(1)
            Y = TMPBUF(2)
            BUTTON = 0
            IF (TMPBUF(3).GT.0) BUTTON = BUTTON + 1
            IF (TMPBUF(4).GT.0) BUTTON = BUTTON + 2
            IF (TMPBUF(5).GT.0) BUTTON = BUTTON + 4
            IF (TMPBUF(6).GT.0) BUTTON = BUTTON + 8
            END IF
         END IF
C
 999  RETURN
      END

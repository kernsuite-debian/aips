      SUBROUTINE YCRCTL (OP, ON, X, Y, LINKX, LINKY, RBLINK, BUTTON,
     *   VRTRTC, IERR)
C-----------------------------------------------------------------------
C! controls the TV cursor visibility, position; reads trackball buttons
C# TV-basic
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
C   Version for IIS Model 70
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   X, Y, RBLINK, BUTTON, IERR
      LOGICAL   ON, LINKX, LINKY, VRTRTC
C
      INTEGER   BUFFER(3), HEADER(8), UNIT, BUFSIZ, BITS(16), NB, I
      LOGICAL   COMAND
      INCLUDE 'INCS:DTVC.INC'
      DATA UNIT, COMAND, BUFSIZ /10,.TRUE.,3/
C-----------------------------------------------------------------------
C                                        write: build control word
      IF (OP.EQ.'WRIT') THEN
         IERR = 2
         IF ((X.LT.1) .OR. (X.GT.MAXXTV(1))) GO TO 999
         IF ((Y.LT.1) .OR. (Y.GT.MAXXTV(2))) GO TO 999
         BUFFER(2) = X - 33
         BUFFER(3) = MAXXTV(2) - 32 - Y
         CALL FILL (16, 0, BITS)
         IF (ON    ) BITS(1) = 1
         IF (LINKX ) BITS(2) = 1
         IF (LINKY ) BITS(3) = 1
         I = MAX (0, MIN (3, RBLINK))
         IF (I.NE.0) I = 4 - I
         CALL ZGTBIT (2, I, BITS(4))
         CALL ZPTBIT (16, BUFFER(1), BITS)
         END IF
C                                        header & IO
 10   CALL YMKHDR (OP, UNIT, COMAND, BUFSIZ, 0, 0, 0, 0, VRTRTC, HEADER)
      NB = 2 * BUFSIZ
      CALL ZM70XF (OP, NB, HEADER, BUFFER, IERR)
C                                        return answers
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) THEN
         CALL ZGTBIT (16, BUFFER(1), BITS)
         ON     = BITS(1).EQ.1
         LINKX  = BITS(2).EQ.1
         LINKY  = BITS(3).EQ.1
         CALL ZPTBIT (4, BUTTON, BITS(9))
         CALL ZPTBIT (2, RBLINK, BITS(4))
         IF (RBLINK.NE.0) RBLINK = 4 - RBLINK
C                                        use button pushed bit to
C                                        prevent double hits
         IF (BITS(15).NE.1) BUTTON = 0
C         IF (BUFFER(2).GE.2048) BUFFER(2) = BUFFER(2) - 4096
         IF (BUFFER(3).GE.2048) BUFFER(3) = BUFFER(3) - 4096
         BUFFER(2) = MOD (BUFFER(2), 512)
         BUFFER(3) = MOD (BUFFER(3), 512)
         X = BUFFER(2) + 32
         X = MOD (X, MAXXTV(1)) + 1
         Y = 3*MAXXTV(2) - BUFFER(3) - 33
         Y = MOD (Y, MAXXTV(2)) + 1
         END IF
C
 999  RETURN
      END

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
C   IIS M75 version: 15 buttons -> 15 distinct values, but are
C   converted to just 4 to match the IIS M70.
C   M75 button    real value         output value
C     F1              1                  1 (M70 button A)
C     F2              2                  2 (M70 button B)
C     F3              3                  4 (M70 button C)
C     A1              4                  1 (M70 button A)
C     B1              5                  2 (M70 button B)
C     C1              6                  4 (M70 button C)
C     D1              7                  8 (M70 button D)
C     A2              4                  1 (M70 button A)
C     B2              5                  2 (M70 button B)
C     C2              6                  4 (M70 button C)
C     D2              7                  8 (M70 button D)
C     A3              4                  1 (M70 button A)
C     B3              5                  2 (M70 button B)
C     C3              6                  4 (M70 button C)
C     D3              7                  8 (M70 button D)
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   X, Y, RBLINK, BUTTON, IERR
      LOGICAL   ON, LINKX, LINKY, VRTRTC
C
      INTEGER   BUFFER(3), HEADER(8), UNIT, BUFSIZ, BITS(16), NB, I, IV
      LOGICAL   COMAND
      DATA UNIT, COMAND, BUFSIZ /10,.TRUE.,3/
C-----------------------------------------------------------------------
C                                       WRIT: build control word
      IF (OP.EQ.'WRIT') THEN
         IERR = 2
         IF ((X.LT.1) .OR. (X.GT.512)) GO TO 999
         IF ((Y.LT.1) .OR. (Y.GT.512)) GO TO 999
         BUFFER(2) = MOD (X-33, 512)
         BUFFER(3) = MOD (480-Y, 512)
         CALL FILL (16, 0, BITS)
         IF (ON    ) BITS(1) = 1
         IF (LINKX ) BITS(2) = 1
         IF (LINKY ) BITS(3) = 1
         I = MAX (0, MIN (3, RBLINK))
         IF (I.NE.0) I = 4 - I
         CALL ZGTBIT (2, I, BITS(4))
         CALL ZPTBIT (16, BUFFER(1), BITS)
         END IF
C                                       Header & IO
      CALL YMKHDR (OP, UNIT, COMAND, BUFSIZ, 0, 0, 0, 0, VRTRTC, HEADER)
      NB = 2 * BUFSIZ
      CALL ZM70XF (OP, NB, HEADER, BUFFER, IERR)
C                                       Return answers
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) THEN
         CALL ZGTBIT (16, BUFFER(1), BITS)
         ON     = BITS(1).EQ.1
         LINKX  = BITS(2).EQ.1
         LINKY  = BITS(3).EQ.1
         CALL ZPTBIT (4, IV, BITS(9))
         IF (IV.EQ.0) BUTTON = 0
         IF ((IV.GE.1) .AND. (IV.LE.3)) BUTTON = 2 ** (IV - 1)
         IF (IV.GE.4) BUTTON = 2 ** MOD (IV, 4)
         CALL ZPTBIT (2, RBLINK, BITS(4))
         IF (RBLINK.NE.0) RBLINK = 4 - RBLINK
C                                       Use button pushed bit to
C                                       prevent double hits
         IF (BITS(15).NE.1) BUTTON = 0
         X = MOD (BUFFER(2)+32, 512) + 1
         Y = 512 - MOD (BUFFER(3)+32, 512)
         END IF
C
 999  RETURN
      END

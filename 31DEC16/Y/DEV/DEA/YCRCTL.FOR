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
C   DeAnza version
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   X, Y, RBLINK, BUTTON, IERR
      LOGICAL   ON, LINKX, LINKY, VRTRTC
C
      INTEGER   I, CURON, CUROFF, BLON, BLOFF, WORK
      LOGICAL   T
      INCLUDE 'INCS:YDEA.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA T /.TRUE./
C                               Note!!  CURON is specified
C                               here for programmable cursor #1.
C                               For standard full screen
C                               cross cursor use Z0030.
      DATA CURON, CUROFF, BLON, BLOFF /16384,0, 4096,0/
C-----------------------------------------------------------------------
C                                        write: check input
      IF (OP.EQ.'WRIT') THEN
         IERR = 2
         IF ((X.LT.1) .OR. (X.GT.MAXXTV(1))) GO TO 999
         IF ((Y.LT.1) .OR. (Y.GT.MAXXTV(2))) GO TO 999
C                                       zero cursor soft registers
         CALL FILL (7, 0, YBUFF(CURREG))
C                                       Set position words
         YBUFF(CURX1) = X - 1
         YBUFF(CURY1) = Y - 1
C                                       Set control word
         IF (ON) THEN
            YBUFF(CURCON) = CURON
         ELSE
            YBUFF(CURCON) = CUROFF
            END IF
C                                       Set blink rate
         I = MAX (0, MIN (3, RBLINK))
C
         IF (I.EQ.0) THEN
            YBUFF(CURBLI) = BLOFF
         ELSE
            I = 3 * (4 - I)
            YBUFF(CURBLI) = BLON + I
            END IF
C                                       Load cursor register
         CALL ZDEAXF ('LCR ', YBUFF(CURREG), CRBYTE, 0, 0, T, IERR)
C                                       return answers
      ELSE
         CALL ZDEAXF ('RCR ', YBUFF(CURREG), CRBYTE, 0, 0, T, IERR)
C                                       translate x,y
         X = MOD (YBUFF(CURX1), MAXXTV(1)) + 1
         Y = MOD (YBUFF(CURY1), MAXXTV(2)) + 1
C                                       calculate blink
         RBLINK =  YBUFF(CURBLI)
         IF (RBLINK.NE.0)  RBLINK = 4 - MOD (RBLINK, 16)/3
C                                        read button register
C                                        use button pushed bit to
C                                        prevent double hits
         CALL ZDEAXF ('RPR ', WORK, 2, 0, 0, T, IERR)
         BUTTON = MOD (WORK, 64)
         WORK = (WORK - BUTTON) / 64
         IF (MOD(WORK,2).NE.1) BUTTON = 0
         ON = YBUFF(CURCON).NE.0
         END IF
C
 999  RETURN
      END

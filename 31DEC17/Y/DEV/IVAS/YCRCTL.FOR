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
C   IVAS version
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   X, Y, RBLINK, BUTTON, IERR
      LOGICAL   ON, LINKX, LINKY, VRTRTC
C
      INTEGER   NTRY
      CHARACTER IVASRT*14, JCHAR*8
      INTEGER   LBUT, X0, Y0, MODE, IWAIT, JERR, FIVASMOUSESTATUS,
     *   FIVASMOUSEPUT, FIVASGPHBLINKCURSOR,
C                                       attempt 1.1/2.0 together
     *   FIVASMOUSERESET, FIVASMOUSECURSOR
C                                       vers 2.0 routines
C     *   FIVASMOUSEBUTTON, FIVASMOUSEPOSITION
C                                       vers 2.0 routine
C     *   FIVASMOUSECLEAR
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL YVRTR (VRTRTC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Write
      IF (OP.NE.'WRIT') GO TO 20
         IERR = 2
         IF ((X.LT.1) .OR. (X.GT.MAXXTV(1))) GO TO 999
         IF ((Y.LT.1) .OR. (Y.GT.MAXXTV(2))) GO TO 999
         IERR = 0
C                                       Clear button buffer
         IVASRT = 'MOUSERESET'
         JERR = FIVASMOUSERESET ()
         IF (JERR.NE.0) GO TO 900
         MODE = 1
         IVASRT = 'MOUSECURSOR'
         JERR = FIVASMOUSECURSOR (MODE)
C         JERR = FIVASMOUSECLEAR (LBUT)
         IF (JERR.NE.0) GO TO 900
C                                       Mouse position
         X0 = X - 1
         Y0 = Y - 1
         MODE = 3
         IVASRT = 'MOUSEPUT'
         JERR = FIVASMOUSEPUT (X0, Y0, MODE)
         IF (JERR.NE.0) GO TO 900
C                                       ON and blink rate
         RBLINK = MAX (0, MIN (3, RBLINK))
         X0 = 0
         Y0 = 7
         IF (ON) THEN
            X0 = 7 - RBLINK
            Y0 = RBLINK
         ELSE
            X0 = 0
            Y0 = 7
            END IF
         IVASRT = 'GPHBLINKCURSOR'
         JERR = FIVASGPHBLINKCURSOR (X0, Y0)
         IF (JERR.NE.0) GO TO 900
C                                       Save in YBUFF common
         YBUFF(3) = 0
         IF (ON) YBUFF(3) = 1
         YBUFF(2) = RBLINK
         GO TO 999
C                                       return answers: Read
 20   CONTINUE
         ON = YBUFF(3).GT.0
         LINKX  = .TRUE.
         LINKY  = .TRUE.
         RBLINK = YBUFF(2)
         MODE = 3
         NTRY = 0
         IWAIT = 0
 25      JERR = FIVASMOUSESTATUS (LBUT, X0, Y0, MODE, IWAIT)
         IVASRT = 'MOUSESTATUS'
            IF (JERR.LT.0) JERR = 0
            IF (JERR.EQ.Y0) JERR = 0
            IF (JERR.NE.0) THEN
               CALL ZHEX (JERR, 8, JCHAR)
               WRITE (MSGTXT,1000) JERR, JCHAR
               CALL MSGWRT (6)
               IERR = 1
               GO TO 999
               END IF
            IF ((X0.GE.0) .AND. (X0.LT.MAXXTV(1)) .AND. (Y0.GE.0)
     *         .AND. (Y0.LT.MAXXTV(2))) GO TO 30
               NTRY = NTRY + 1
               IF (NTRY.LT.10) GO TO 25
                  X0 = MAXXTV(1) / 2
                  Y0 = MAXXTV(2) / 2
 30      X = X0 + 3 * MAXXTV(1)
         Y = Y0 + 3 * MAXXTV(2)
         X = MOD (X, MAXXTV(1)) + 1
         Y = MOD (Y, MAXXTV(2)) + 1
         CALL YBUTON (LBUT, BUTTON, IERR)
         GO TO 999
C                                       Error
 900  WRITE (MSGTXT,1900) IVASRT
      CALL MSGWRT (6)
      CALL YDOERR ('YCRCTL', JERR, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('YCRCTL: FIVASMOUSESTATUS ERROR ',I10,' = ',A)
 1900 FORMAT ('ERROR STARTED AT ROUTINE FIVAS',A)
      END

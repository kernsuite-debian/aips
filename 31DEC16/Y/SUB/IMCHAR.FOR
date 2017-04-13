      SUBROUTINE IMCHAR (CHAN, X, Y, IANGL, CENTER, STRING, SCRTCH,
     *   IERR)
C-----------------------------------------------------------------------
C! writes character string to TV
C# TV-util
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
C   IMCHAR causes characters to appear on the TV by calling YCHRW.
C   Inputs:
C      CHAN    I       channel number (1 - NGRAY+NGRAPH)
C      X       I       X position of string
C      Y       I       Y position of string
C      IANGL   I       0 - horizontal (to right), 3 - vertical (down)
C                      2 - to left, 4 up
C      CENTER  I       0 - XY are lower left of first character
C                      1 - XY are center of string
C                      2 - XY are upper right of last character
C      STRING  C*(*)   character string to go to TV - length from LEN
C   Output:
C      SCRTCH  I(*)    scratch buffer (TV size > 1280)
C      IERR    I       error code of ZM70XF: 0 - ok;   2 - input error
C-----------------------------------------------------------------------
      INTEGER   CHAN, X, Y, IANGL, CENTER, SCRTCH(*), IERR
      CHARACTER STRING*(*)
C
      INTEGER   X0, Y0, I, COUNT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                        test input
      IERR = 2
      COUNT = LEN (STRING)
      IF (COUNT.LE.0) GO TO 900
      IF ((CHAN.LT.1) .OR. (CHAN.GT.NGRAY+NGRAPH)) GO TO 900
      IF ((IANGL.NE.0) .AND. (IANGL.NE.3)) GO TO 900
C                                        find start position: lower
C                                        left of first char
      X0 = X
      Y0 = Y
C                                       centering
      IF (CENTER.EQ.1) THEN
         IF (IANGL.EQ.3) THEN
            X0 = X - CSIZTV(1)/2
            Y0 = Y + (COUNT*CSIZTV(2)+1)/2 - CSIZTV(2)
         ELSE IF (IANGL.EQ.2) THEN
            X0 = X + (COUNT*CSIZTV(1)+1)/2 - CSIZTV(1)
            Y0 = Y - CSIZTV(2)/2
         ELSE IF (IANGL.EQ.1) THEN
            X0 = X - CSIZTV(1)/2
            Y0 = Y - (COUNT*CSIZTV(2)+1)/2
         ELSE
            X0 = X - (COUNT*CSIZTV(1))/2
            Y0 = Y - CSIZTV(2)/2
            END IF
C                                       upper right of last
      ELSE IF (CENTER.EQ.2) THEN
         IF (IANGL.EQ.3) THEN
            X0 = X - CSIZTV(1) + 1
            Y0 = Y + CSIZTV(2)*(COUNT-2) + 1
         ELSE IF (IANGL.EQ.2) THEN
            Y0 = Y - CSIZTV(2) + 1
            X0 = X - CSIZTV(1) + 1
         ELSE IF (IANGL.EQ.1) THEN
            X0 = X - CSIZTV(1) + 1
            Y0 = Y - CSIZTV(2)*COUNT + 1
         ELSE
            Y0 = Y - CSIZTV(2) + 1
            X0 = X - COUNT*CSIZTV(1) + 1
            END IF
         END IF
C                                       vertical: 1 char / call
      IF (IANGL.EQ.3) THEN
         DO 20 I = 1,COUNT
            IF (Y0+CSIZTV(2).LE.MAXXTV(2)) THEN
               CALL YCHRW (CHAN, X0, Y0, STRING(I:I), SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            Y0 = Y0 - CSIZTV(2)
            IF (Y0.LE.0) GO TO 999
 20         CONTINUE
C                                        to left
      ELSE IF (IANGL.EQ.2) THEN
         DO 30 I = 1,COUNT
            IF (X0+CSIZTV(1).LE.MAXXTV(1)) THEN
               CALL YCHRW (CHAN, X0, Y0, STRING(I:I), SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            X0 = X0 - CSIZTV(1)
            IF (X0.LE.0) GO TO 999
 30         CONTINUE
C                                       up
      ELSE IF (IANGL.EQ.1) THEN
         DO 40 I = 1,COUNT
            IF (Y0.GE.1) THEN
               CALL YCHRW (CHAN, X0, Y0, STRING(I:I), SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            Y0 = Y0 + CSIZTV(2)
            IF (Y0+CSIZTV(2).GT.MAXXTV(2)) GO TO 999
 40         CONTINUE
C                                        horizontal to right
      ELSE
         IF ((X0.GE.1) .AND. (X0+COUNT*CSIZTV(1).LE.MAXXTV(1))) THEN
            CALL YCHRW (CHAN, X0, Y0, STRING, SCRTCH, IERR)
         ELSE
            DO 50 I = 1,COUNT
               IF (X0.GE.1) THEN
                  CALL YCHRW (CHAN, X0, Y0, STRING(I:I), SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 999
                  END IF
               X0 = X0 + CSIZTV(1)
               IF (X0+CSIZTV(1).GT.MAXXTV(1)) GO TO 999
 50            CONTINUE
            END IF
         END IF
      GO TO 999
C
 900  WRITE (MSGTXT,1900) COUNT, CHAN, IANGL
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('IMCHAR: BAD COUNT CHAN IANGL =',3I7)
      END

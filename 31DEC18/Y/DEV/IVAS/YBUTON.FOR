      SUBROUTINE YBUTON (LBUT, BUTTON, IERR)
C-----------------------------------------------------------------------
C! returns button buffer for the IVAS TV routines
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
C   YBUTON reads the state of the Button Buffer of the TV display.
C   Input:
C      LBUT     I   Button value returned from an initial status read
C                   (Send -1 if none done).
C   Output:
C      BUTTON   I   Button value: 0, 1, 2, 4, or 8
C      IERR     I   Error: 0 ok, 1, bad IO
C   IIS IVAS version - Button D simulated by re-reading twice to see if
C   > 1 button hit.
C-----------------------------------------------------------------------
      INTEGER   LBUT, BUTTON, IERR
C
      INTEGER   IER, TBUT(8), ZOR, JERR, JBUT1, JBUT2, JBUT3, MODE,
     *   IWAIT, X0, Y0, FIVASMOUSESTATUS
      REAL      TDEL
      CHARACTER JCHAR*8
      INCLUDE 'INCS:DMSG.INC'
      DATA TDEL /0.1/
      DATA TBUT /0, 4, 2, 8, 1, 8, 8, 8/
C-----------------------------------------------------------------------
      BUTTON = 0
      JBUT2 = 0
      JBUT3 = 0
      IERR = 0
      JERR = 0
      IWAIT = 0
      MODE = 3
      JBUT1 = LBUT
      IF ((JBUT1.LT.0) .OR. (JBUT1.GT.7))
     *   JERR = FIVASMOUSESTATUS (JBUT1, X0, Y0, MODE, IWAIT)
      IF (JERR.EQ.Y0) JERR = 0
      IF (JERR.LT.0) JERR = 0
      IF (JERR.NE.0) THEN
         CALL ZHEX (JERR, 8, JCHAR)
         WRITE (MSGTXT,1000) JERR, JCHAR
         CALL MSGWRT (6)
         GO TO 900
         END IF
      IF ((JBUT1.LT.0) .OR. (JBUT1.GT.7)) JBUT1 = 0
      IF (JBUT1.EQ.0) GO TO 999
C                                       Multiple buttons?
      IF ((JBUT1.EQ.3) .OR. (JBUT1.GT.4)) GO TO 10
         CALL ZDELAY (TDEL, IER)
         JERR = FIVASMOUSESTATUS (JBUT2, X0, Y0, MODE, IWAIT)
         IF (JERR.EQ.Y0) JERR = 0
         IF (JERR.LT.0) JERR = 0
         IF (JERR.NE.0) THEN
            CALL ZHEX (JERR, 8, JCHAR)
            WRITE (MSGTXT,1000) JERR, JCHAR
            CALL MSGWRT (6)
            GO TO 900
            END IF
         IF ((JBUT2.LT.0) .OR. (JBUT2.GT.7)) JBUT2 = 0
         CALL ZDELAY (TDEL, IER)
         JERR = FIVASMOUSESTATUS (JBUT3, X0, Y0, MODE, IWAIT)
         IF (JERR.LT.0) JERR = 0
         IF (JERR.EQ.Y0) JERR = 0
         IF (JERR.NE.0) THEN
            CALL ZHEX (JERR, 8, JCHAR)
            WRITE (MSGTXT,1000) JERR, JCHAR
            CALL MSGWRT (6)
            GO TO 900
            END IF
         IF ((JBUT3.LT.0) .OR. (JBUT3.GT.7)) JBUT3 = 0
C                                       OR the buttons
 10   BUTTON = ZOR (JBUT1, JBUT2)
      BUTTON = ZOR (BUTTON, JBUT3)
      BUTTON = TBUT(BUTTON+1)
      GO TO 999
C                                       just return error: DOERR fails
 900  IERR = 1
C     CALL YDOERR ('YBUTON', JERR, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('YBUTON: FIVASMOUSESTATUS ERROR ',I10,' = ',A)
      END

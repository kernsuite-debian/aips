      SUBROUTINE TVTPOS (DOMENU, TIMLIM, TVXY, TVBUTT, IERR)
C-----------------------------------------------------------------------
C! reads TV cursor and button with prompt and time out options
C# TV-appl
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
C   get tv position within a time limit.  The TV button buffer is
C   cleared on entry if TIMLIM >= 0.  The TV cursor is turned on if
C   needed, but it is not moved to do so.
C   Inputs:
C      DOMENU   I      Prompt message control:
C                        -1 => cursor position unimportant
C                         0 => picking an image pixel
C                         1 => picking a menu item
C                         2 => menu help avalable
C      TIMLIM   I      Return after TIMLIM seconds even if no button
C                         0 => infinite time limit
C                        <= -1 => no message and no wait
C   Outputs:
C      TVXY     R(2)   TV x,y pixel position: corrected for zoom, not
C                      scroll
C      TVBUTT   I      Selected button: 0 => timed out
C      IERR     I      Error code: > 10 => object not open properly
C-----------------------------------------------------------------------
      INTEGER   DOMENU, TIMLIM, TVBUTT, IERR
      REAL      TVXY(2)
C
      INTEGER   IX, IY, RBLINK, BUTTON, QUAD
      LOGICAL   ON, LINKX, LINKY, VRTRTC
      REAL      TD
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       get current cursor status
      CALL YCRCTL ('READ', ON, IX, IY, LINKX, LINKY, RBLINK, BUTTON,
     *   VRTRTC, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (.NOT.ON) THEN
         ON = .TRUE.
         CALL YCRCTL ('READ', ON, IX, IY, LINKX, LINKY, RBLINK, BUTTON,
     *      VRTRTC, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Quicky
      QUAD = -1
      IF (TIMLIM.LT.0) THEN
         CALL YCURSE ('READ', .FALSE., .TRUE., TVXY, QUAD, TVBUTT, IERR)
         TVBUTT = MAX (TVBUTT, BUTTON)
C                                       Prompted
      ELSE
         IF (TIMLIM.GT.0) THEN
            WRITE (MSGTXT,1000) TIMLIM
            CALL MSGWRT (1)
            END IF
         IF (DOMENU.LT.0) THEN
            MSGTXT = 'Press desired TV button'
         ELSE IF (DOMENU.EQ.0) THEN
            MSGTXT = 'Move cursor to desired position, press desired TV'
     *         // ' button'
         ELSE IF (DOMENU.EQ.1) THEN
            MSGTXT = 'Move cursor to desired menu item, ' //
     *         'press desired TV button'
         ELSE
            MSGTXT = 'Move cursor to desired menu item, ' //
     *         'press button A, B, or C to select'
            CALL MSGWRT (1)
            MSGTXT = 'Press button D for help'
            END IF
         CALL MSGWRT (1)
C                                       infinite
         IF (TIMLIM.EQ.0) THEN
            CALL YCURSE ('READ', .TRUE., .TRUE., TVXY, QUAD, TVBUTT,
     *         IERR)
C                                       Timed
         ELSE
            TD = 0.2
            IX = 5 * TIMLIM
            DO 20 IY = 1,IX
               CALL YCURSE ('READ', .FALSE., .TRUE., TVXY, QUAD, TVBUTT,
     *            IERR)
               IF ((IERR.NE.0) .OR. (TVBUTT.GT.0)) GO TO 999
               CALL ZDELAY (TD)
 20            CONTINUE
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('You have',I6,' seconds to:')
      END

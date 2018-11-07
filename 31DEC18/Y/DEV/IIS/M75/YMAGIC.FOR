      SUBROUTINE YMAGIC (IERR)
C-----------------------------------------------------------------------
C! initialize graphics, zoom, scroll units for IIS Model 75 (level 3)
C# Y3 TV-util
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
C   Set graphics subunit, zoom and scroll subunit.  A Level 3 routine.
C   This is needed to set up the IIS Model 75.
C   Output:
C      IERR   I*2   Error code of Z...XF: 0 => okay
C   IIS Model 75 version.
C-----------------------------------------------------------------------
      INTEGER   BUFFER(256), UNIT, ZMASK, TMASK, NB, IERR, BUFSIZ,
     *   HEADER(8)
      LOGICAL   VRTRTC, COMAND
C-----------------------------------------------------------------------
      IERR = 2
C                                       Set zoom and zcroll subunit
C                                       registers so that zoom, wrap,
C                                       and y are equal to 0.
      BUFFER(1) = 0
      BUFFER(2) = 0
      UNIT = 6
      COMAND = .FALSE.
      BUFSIZ = 2
C                                       ZMASK = 7F hex
      ZMASK = 127
C                                       TMASK = 1201 hex
      TMASK = 4609
      CALL YMKHDR ('WRIT', UNIT, COMAND, BUFSIZ, 0, 0, ZMASK, TMASK,
     *   VRTRTC, HEADER)
      NB = 2 * BUFSIZ
      CALL ZM70XF ('WRIT', NB, HEADER, BUFFER, IERR)
C                                       BUFFER(1),BUFFER(2) = 800 hex
      BUFFER(1) = 2048
      BUFFER(2) = 2048
C                                       ZMASK = 80 hex
      ZMASK = 128
      CALL YMKHDR ('WRIT', UNIT, COMAND, BUFSIZ, 0, 0, ZMASK, TMASK,
     *   VRTRTC, HEADER)
      CALL ZM70XF ('WRIT', NB, HEADER, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set graphics subunit so that
C                                       status plane is off and
C                                       deselected, ROI is deselected,
C                                       and able the graphics, video
C                                       and cursor.
C      BUFFER(1) = 0
C      UNIT = 9
C      COMAND = .TRUE.
C      BUFSIZ = 1
C                                       TMASK = 7FF hex
C      TMASK = 2047
C                                        header + IO
C      CALL YMKHDR ('WRIT', UNIT, COMAND, BUFSIZ, 0, 0, 0, TMASK,
C     *   VRTRTC, HEADER)
C      NB = 2 * BUFSIZ
C      CALL ZM70XF ('WRIT', NB, HEADER, BUFFER, IERR)
C      IF (IERR .NE. 0) GO TO 999
C                                       Set up graphics RAM
C      DO 20 I = 1,256
C         BUFFER(1) = '174037'o
C 20   CONTINUE
C      CALL YGGRAM ('WRIT', VRTRTC, BUFFER, IERR)
C      IF (IERR .NE. 0) GO TO 999
C                                       LUT constant for selectin of
C                                       digitizer and graphics ROI
      UNIT = 2
      BUFFER(1) = 0
      BUFSIZ = 1
      CALL YMKHDR ('WRIT', UNIT, COMAND, BUFSIZ, 16, 0, 0, 0,
     *   VRTRTC, HEADER)
      NB = 2 * BUFSIZ
      CALL ZM70XF ('WRIT', NB, HEADER, BUFFER, IERR)
C
 999  RETURN
      END

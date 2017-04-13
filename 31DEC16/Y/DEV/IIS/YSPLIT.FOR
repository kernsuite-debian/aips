      SUBROUTINE YSPLIT (OP, XSPLT , YSPLT , RCHANS, GCHANS, BCHANS,
     *   VRTRTC, IERR)
C-----------------------------------------------------------------------
C! set channel selection by split-screen quadrant
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
C   YSPLIT reads/writes the look up table/ split screen control
C   registers of the TV - turns channels on/off by quadrant
C   Inputs:
C      OP       C*4    'READ' from TV, 'WRIT' to TV
C      VRTRTC   L      T => do on vertical retrace only
C   In/Out:
C      XSPLT    I      X position of split (1-512, 1 => LHS)
C      YSPLT    I      Y position of split (1-512, 1 => bot)
C      RCHANS   I(4)   chan select bit mask 4 quadrants : red
C      GCHANS   I(4)   chan select bit mask 4 quadrants : green
C      BCHANS   I(4)   chan select bit mask 4 quadrants : blue
C   Output:
C      IERR     I      error code of Z...XF: 0 => ok, 2 => input error
C   Quadrants are numbered CCW from top right!!!!
C   Version for IIS Models 70 and 75
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   XSPLT, YSPLT, RCHANS(4), GCHANS(4), BCHANS(4), IERR
      LOGICAL   VRTRTC
C
      INTEGER   UNIT, BUFFER(14), HEADER(8), BUFSIZ, NB
      LOGICAL   COMAND
      INCLUDE 'INCS:DTVC.INC'
      DATA  UNIT, COMAND, BUFSIZ /2, .TRUE., 14/
C-----------------------------------------------------------------------
C                                        write: set buffer
      IF (OP.EQ.'WRIT') THEN
         IERR = 2
         IF ((XSPLT.LT.1) .OR. (XSPLT.GT.MAXXTV(1))) GO TO 999
         IF ((YSPLT.LT.1) .OR. (YSPLT.GT.MAXXTV(2))) GO TO 999
         BUFFER(2) = BCHANS(1)
         BUFFER(1) = BCHANS(2)
         BUFFER(3) = BCHANS(3)
         BUFFER(4) = BCHANS(4)
         BUFFER(6) = GCHANS(1)
         BUFFER(5) = GCHANS(2)
         BUFFER(7) = GCHANS(3)
         BUFFER(8) = GCHANS(4)
         BUFFER(10) = RCHANS(1)
         BUFFER( 9) = RCHANS(2)
         BUFFER(11) = RCHANS(3)
         BUFFER(12) = RCHANS(4)
         BUFFER(13) = XSPLT - 1
         BUFFER(14) = MAXXTV(2) - YSPLT
         END IF
C                                        header & IO
      CALL YMKHDR (OP, UNIT, COMAND, BUFSIZ, 0, 0, 0, 0, VRTRTC,
     *   HEADER)
      NB = 2 * BUFSIZ
      CALL ZM70XF (OP, NB, HEADER, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        reset common on Write
      IF (OP.EQ.'WRIT') THEN
         TVSPLT(1) = XSPLT
         TVSPLT(2) = YSPLT
C                                        return data
      ELSE
         BCHANS(1) = BUFFER(2)
         BCHANS(2) = BUFFER(1)
         BCHANS(3) = BUFFER(3)
         BCHANS(4) = BUFFER(4)
         GCHANS(1) = BUFFER(6)
         GCHANS(2) = BUFFER(5)
         GCHANS(3) = BUFFER(7)
         GCHANS(4) = BUFFER(8)
         RCHANS(1) = BUFFER(10)
         RCHANS(2) = BUFFER(9)
         RCHANS(3) = BUFFER(11)
         RCHANS(4) = BUFFER(12)
         XSPLT = BUFFER(13) + 1
         YSPLT = MAXXTV(2) - BUFFER(14)
         END IF
C
 999  RETURN
      END

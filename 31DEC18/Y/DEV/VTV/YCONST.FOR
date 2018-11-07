      SUBROUTINE YCONST (OP, RCONST, GCONST, BCONST, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! controls the constant registers added to the TV picture - not used
C# Y2 TV-basic
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
C   YCONST reads/writes the constants which are added to the 3 sum
C   channels on the TV.
C   Inputs:
C      OP      C*4   'READ' from TV, 'WRIT' to TV
C   In/out:
C      RCONST  I     constant for red channel
C      GCONST  I     constant for green channel
C      BCONST  I     constant for blue channel
C      VRTRTC  L     (could be Vertical retrace)
C   Output:
C      IERR    I     error code of Z...XF : 0 => ok
C   Virtual TV version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   RCONST, GCONST, BCONST, IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      BUFFER(1) = 0
      IF (VRTRTC) BUFFER(1) = 1
      BUFFER(2) = RCONST
      BUFFER(3) = GCONST
      BUFFER(4) = BCONST
      IF (OP.NE.'WRIT') THEN
         BUFSW = 1
         BUFSR = 4
      ELSE
         BUFSW = 4
         BUFSR = 0
         END IF
C                                        header & IO
      CALL MKYBUF (OP, 'YCONST', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                        read: return control parms
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) THEN
         CALL UNYBUF (BUFSR, BUFFER, HEADER)
         RCONST = BUFFER(2)
         GCONST = BUFFER(3)
         BCONST = BUFFER(4)
         END IF
C
 999  RETURN
      END

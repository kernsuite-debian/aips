      SUBROUTINE TVWHER (QUAD, RPOS, IBUT, IERR)
C-----------------------------------------------------------------------
C! turns on cursor, waits for button, returns quadrant, position, button
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
C   TVWHER is the routine to use if you want the user to point at
C   something on the TV screen.  It turns on the cursor, waits for a
C   button push, determines which quadrant the event occurred in, and
C   returns the quadrant number and the TV coordinates of the event
C   corrected for the scroll of the lowest numbered plane which is
C   "on" in that quadrant  and for zoom.
C   Outputs:
C      QUAD  I       Quadrant number
C      RPOS  R(2)    TV position corrected for scroll (& zoom)
C      IBUT  I       Value of button(s) pushed
C      IERR  I       Error code of ZM70XF
C-----------------------------------------------------------------------
      REAL      RPOS(2)
      INTEGER   QUAD, IBUT, IERR
C
      LOGICAL   T
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Read cursor, wait for button
      QUAD = 0
      CALL YCURSE ('READ', T, T, RPOS, QUAD, IBUT, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END

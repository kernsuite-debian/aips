      SUBROUTINE YCNECT (X1, Y1, X2, Y2, IC, BUFFER, IERR)
C-----------------------------------------------------------------------
C! write line segment between 2 points on TV
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
C   YCNECT writes a line segment on the TV.  This version will work on
C   all TVs.  It is called a Y routine to allow the use of hardware
C   vector generators on those TVs equiped with them.
C   Inputs:
C      X1      I      start X position
C      Y1      I      start Y position
C      X2      I      end X position
C      Y2      I      end Y position
C      IC      I      Channel (1 to NGRAY+NGRAPH)
C      BUFFER  I(*)   Buffer contains desired intensity (size > max
C                     horizontal or vertical line, e.g., 1280)
C   Output:
C      IERR    I      error code : 0 => ok
C   Version for no TV at all - stubbed.
C-----------------------------------------------------------------------
      INTEGER    X1, Y1, X2, Y2, IC, BUFFER(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'YCNECT: THERE IS NO TV CONFIGURED'
      CALL MSGWRT (8)
C
 999  RETURN
      END

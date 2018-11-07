      SUBROUTINE YGRAFE (OP, BLOTCH, STATUS, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! controls the graphics control register (IIS function)
C# Y3 TV-basic
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
C   YGRAFE reads/writes the TV graphics control register.  This
C   version does not support all the TV options: it forces the
C   black & white status monitor on and refuses to allow the disable
C   video, graphics, and cursor options.
C   Inputs:
C      OP      C*4   'READ' from TV or 'WRIT' to TV
C      VRTRTC  L     T => do it only during vertical retrace
C   In/Out:
C      BLOTCH  I     graphics plane used as blotch (1 - 7)
C      STATUS  I     graphics plane used as status (1 - 7)
C   Output:
C      IERR    I     error code of Z...XF : 0 => ok
C                                           2 => input error
C   NO TV VERSION - this is an IIS M70/M75 level 3 function
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   BLOTCH, STATUS, IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'THERE IS NO TV CONFIGURED WITH GRAPHICS CONTROL REGS'
      CALL MSGWRT (8)
C
 999  RETURN
      END

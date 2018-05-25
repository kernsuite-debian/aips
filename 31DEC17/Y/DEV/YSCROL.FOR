      SUBROUTINE YSCROL (CHANNL, SCROLX, SCROLY, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! write the scroll registers (shift location of 1 or more TV channels)
C# Y1 TV-basic
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
C   YSCROL writes the scroll registers on the TV.
C   Inputs:
C      CHANNL   I   bit map channel select: bits 1-NGRAY gray channels,
C                   bit NGRAY+1 => all graphics
C      VRTRTC   L   T => do it on vertical retrace only
C   In/Out:
C      SCROLX   I   amount of X scroll (>0 to right)
C      SCROLY   I   amount of Y scroll (>0 upwards)
C   Output:
C      IERR     I   error from Z...XF : 0 => ok, 2 => input error
C   YSCROL updates the scroll variables in /TVDEV/ common
C   No TV (scroll) version - stub
C-----------------------------------------------------------------------
      INTEGER   CHANNL, SCROLX, SCROLY, IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'THERE IS NO TV SCROLL CONFIGURED'
      CALL MSGWRT (8)
C
 999  RETURN
      END

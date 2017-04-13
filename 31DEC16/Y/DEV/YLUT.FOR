      SUBROUTINE YLUT (OP, CHANNL, COLOR, VRTRTC, LUT, IERR)
C-----------------------------------------------------------------------
C! read/write channel-based look-up-table
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
C   YLUT reads/writes full channel look up tables to TV.
C   Inputs:
C      OP      C*4    'READ' from TV, 'WRIT' to TV
C      CHANNL  I      channel select bit mask
C      COLOR   I      color select bit mask (RGB <-> 421)
C      VRTRTC  L      T => do it only during vertical retrace
C   In/Out:
C      LUT     I(*)   look up table (dimension = MAXINT+1, values to
C                     LUTOUT are used)
C   Out:
C      IERR    I      error code of Z...XF : 0 => ok, 2 => input error
C   No TV, with LUT anyway, version - i.e. a stub.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHANNL, COLOR, LUT(*), IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'THERE IS NO TV LUT CONFIGURED'
      CALL MSGWRT (8)
C
 999  RETURN
      END

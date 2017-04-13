      SUBROUTINE YSPLIT (OP, XSPLT , YSPLT , RCHANS, GCHANS, BCHANS,
     *   VRTRTC, IERR)
C-----------------------------------------------------------------------
C! set channel selection by split-screen quadrant
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
C   No TV version - stub.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   XSPLT, YSPLT, RCHANS(4), GCHANS(4), BCHANS(4), IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'YSPLIT: THERE IS NO TV CONFIGURED'
      CALL MSGWRT (8)
C
 999  RETURN
      END

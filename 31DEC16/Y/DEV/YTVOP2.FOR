      SUBROUTINE YTVOP2 (LUN, IND, IERR)
C-----------------------------------------------------------------------
C! open actual TV device (called by YTVOPN)
C# Y3 TV-IO
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
C   YTVOP2 performs a system "OPEN" on the TV device.  It is a Y
C   routine in order to call the appropriate Z routine only.
C   Inputs:
C      LUN    I   Logical unit number to use
C   Output:
C      IND    I   Pointer to FTAB entry for open device
C      IERR   I   Error code: 0 => ok
C                    1 = LUN already in use
C                    2 = file not found
C                    3 = volume not found
C                    4 = excl requested but not available
C                    5 = no room for lun
C                    6 = other open errors
C   No TV version - stub.
C-----------------------------------------------------------------------
      INTEGER   LUN, IND, IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'YTVOP2: THERE IS NO TV CONFIGURED'
      CALL MSGWRT (8)
C
 999  RETURN
      END

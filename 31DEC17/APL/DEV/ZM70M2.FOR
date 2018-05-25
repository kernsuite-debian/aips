      SUBROUTINE ZM70M2 (FCB, IERR)
C-----------------------------------------------------------------------
C! issues a master clear to an IIS Model 70 TV
C# IO-TV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003
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
C   Perform a master clear on an IIS model 70 TV device - second level z
C   Inputs:
C      FCB      I(*)        File control block in FTAB for TV device
C   Generic version - stubbed with STOP
C-----------------------------------------------------------------------
      INTEGER   FCB(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'ZM70M2: REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (10)
      IF (NLUSER.GT.0) STOP 'ZM70M2 NEEDS LOCAL DEVELOPMENT'
C
 999  RETURN
      END

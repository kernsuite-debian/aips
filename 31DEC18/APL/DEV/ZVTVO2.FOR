      SUBROUTINE ZVTVO2 (FCB, PNAME, IERR)
C-----------------------------------------------------------------------
C! open connection in client (virtual-TV) to server (remote, real-TV)
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   ZVTVO2 opens the connection in the Virtual TV program to the
C   remote computer which provides the actual TV device.
C   Inputs:
C      FCB     I(*)   File descriptor
C      PNAME   H*48   Environment variable (logical) "TVDEVn" identifies
C                     which TV is used and points to further ID info
C   Output:
C      IERR    I      Error: 0 => okay
C   Generic version - stub
C-----------------------------------------------------------------------
      HOLLERITH PNAME(*)
      INTEGER   FCB(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 8
      WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZVTVO2: REQUIRES DEVELOPMENT; GENERIC IS A STUB')
      END

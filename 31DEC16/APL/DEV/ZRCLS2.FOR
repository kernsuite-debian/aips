      SUBROUTINE ZRCLS2 (FCB, IRET)
C-----------------------------------------------------------------------
C! close a connection to a server -- generic version
C# IO-remote Z2
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
C   Close a connection to a server.
C
C   Inputs:
C      FCB      I(*)          AIPS file control block.
C
C   Outputs:
C      IRET     I             Error return status:
C                                0 => channel closed
C                                1 => failed to close channel
C
C   Generic version (stubbed).
C-----------------------------------------------------------------------
      INTEGER FCB(*), IRET

      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 1
      WRITE (MSGTXT, 1000)
      CALL MSGWRT (6)
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZRCLS2 REQUIRES LOCAL DEVELOPMENT')
      END

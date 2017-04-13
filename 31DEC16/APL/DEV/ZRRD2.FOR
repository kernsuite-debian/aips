      SUBROUTINE ZRRD2 (FCB, NBYTES, BUFF, IRET)
C-----------------------------------------------------------------------
C! read a message from a server -- generic version
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
C------------------------------------------------------------------------
C   Read a message from a server.
C
C   Inputs:
C      FCB      I(*)       AIPS file control block.
C
C   Input/output:
C      NBYTES   I          On input the maximum number of bytes to
C                          receive; on output the number of bytes
C                          actually read.
C
C   Outputs:
C      BUFF     I(*)       Message buffer.
C      IRET     I          Error return code:
C                             0 => message read successfully
C                             1 => message longer than NBYTES long
C                             2 => read failed
C                             3 => partner closed connection.
C
C   Generic version (stubbed)
C-----------------------------------------------------------------------
      INTEGER FCB(*), NBYTES, BUFF(*), IRET

      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 1
      WRITE (MSGTXT, 1000)
      CALL MSGWRT (6)
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZRRD2 REQUIRES LOCAL DEVELOPMENT')
      END

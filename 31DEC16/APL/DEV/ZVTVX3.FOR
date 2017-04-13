      SUBROUTINE ZVTVX3 (FCB, BUFSW, BUFFER, IERR)
C-----------------------------------------------------------------------
C! reads/writes from/to client (virtual TV) for the server (real TV)
C# TV-IO
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
C   ZVTVX3 reads into TVMON (on the computer with the real TV) from the
C   computer running AIPS VTV code and returns answers and error codes.
C   Inputs:
C      FCB      I      File descriptor
C      BUFSW    I      Number words to send (incl header); if = 0,
C                      implies this is a read instead.
C   In/out:
C      BUFFER   I(*)   8-word header plus data buffer: in FITS-standard
C                      16-bit integer form!!!!!
C   Output:
C      IERR     I      Error: 0 => okay
C                         3 => IO error
C                         4 => End of File
C   Generic version - stub.
C-----------------------------------------------------------------------
      INTEGER   FCB(*), BUFSW, BUFFER(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 8
      WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZVTVX3: REQUIRES DEVELOPMENT; GENERIC IS A STUB')
      END

      SUBROUTINE ZSCREA (SOCK, IRET)
C-----------------------------------------------------------------------
C! create an internet socket
C# Z IO Network
C;  Copyright (C) 1997
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
C   Create an internet stream (TCP) socket.  The resulting socket
C   may be used in a server (see ZSBIND, ZSLISN and ZSACPT) or a
C   client program (see ZSCONN).
C
C   Outputs:
C
C      SOCK     I      An integer used to refer to the socket just
C                       created (eg. a socket descriptor under Unix or
C                       a socket handle under win95)
C      IRET     I      Return code: 0 indicates that the socket was
C                                     created
C                                   1 indicates that a system-level
C                                     error occurred
C                                   99 indicates that the network
C                                      interface is not implemented
C
C   Usage notes:
C
C   - SOCK will be non-zero if IRET is zero
C   - Tasks should check for IRET = 99 to test whether network inter-
C     faces are available in AIPS on the host system.
C   - The caller should interpret IRET and issue any error messages
C
C   Generic version: no network interfaces
C-----------------------------------------------------------------------
      INTEGER   SOCK, IRET
C-----------------------------------------------------------------------
      SOCK = 0
      IRET = 99
C
 999  RETURN
      END

      SUBROUTINE ZSACPT (SOCK, CLILEN, NEWSOC, CLIENT, IRET)
C-----------------------------------------------------------------------
C! accept a connection from a client
C# Z IO Network
C-----------------------------------------------------------------------
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
C   Accept a connection from a client program. Should only be called for
C   sockets which have been set into "listen" mode using ZSLISN.
C
C   Inputs:
C
C      SOCK     I      An integer used to refer to the socket to be
C                      be used, as returned from ZSCREA.
C                      Precondition: SOCK .NE. 0
C      CLILEN   I      The length of the buffer used to receive the
C                      client name (CLIENT below).
C                      Precondition: CLILEN > 0
C
C   Outputs
C
C      NEWSOC   I      The ID number of a new socket.  Use this socket,
C                      not SOCK, to read and write to the client.
C      CLIENT   C*?    The name of the client.
C      IRET     I      Return code: 0 connection accepted
C                                   1 a bad argument value was detected
C                                   2 a system error occurred
C                                   99 indicates that the network
C                                      interface is not implemented
C
C   Usage notes:
C
C   - The caller should interpret IRET and issue any error messages
C   - NEWSOC will be non-zero if no errors are detected.
C   - Program execution is suspended until a connection is made.
C
C   Generic version: no network interface
C-----------------------------------------------------------------------
      INTEGER   SOCK, CLILEN, NEWSOC, IRET
      CHARACTER CLIENT*(*)
C
      CHARACTER DUMNAM*4
      PARAMETER (DUMNAM = 'None')
C-----------------------------------------------------------------------
      IRET   = 99
      NEWSOC = 0
      CLIENT = DUMNAM
C
 999  RERURN
      END

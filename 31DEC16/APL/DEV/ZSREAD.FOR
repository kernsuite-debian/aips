      SUBROUTINE ZSREAD (SOCK, DLEN, DATA, IRET)
C-----------------------------------------------------------------------
C! read data from a network connection.
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
C   Read data from the network.
C
C   Inputs:
C
C      SOCK     I      An integer designating the socket to be read
C                       from as returned by ZSCREA.
C                       Precondition: SOCK .NE. 0
C      DLEN     I      The number of bytes of data to write
C                       Precondition: DLEN .GE. 0
C
C   Outputs:
C
C      DATA     I(*)   Data buffer
C      IRET     I      Return code: 0 indicates that the data was read
C                                   1 indicates that ZSREAD was called
C                                     with bad arguments
C                                   2 indicates that a system-level
C                                     error was detected
C                                  -1 indicates that the remote end of
C                                     the connection was closed
C                                  99 indicates that the network
C                                     interface is not implemented
C
C   Usage notes:
C
C    - If IRET is -1 the local end of the connection should be closed;
C      any amount of data, short of DLEN bytes may have been tranferred
C      from the peer process.
C    - The calling process will be blocked until either all of the data
C      is read, the remote end of the connection is closed or an error
C      occurs
C    - The caller should interpret IRET and issue any error messages
C
C Generic version: no network interface
C-----------------------------------------------------------------------
      INTEGER   SOCK, DLEN, DATA(*), IRET
C-----------------------------------------------------------------------
      IRET = 99
C
 999  RETURN
      END


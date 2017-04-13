      SUBROUTINE ZSLISN (SOCK, QLEN, IRET)
C-----------------------------------------------------------------------
C! Place a socket into "listen" mode
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
C   Set a socket into "listen" mode so that client programs may
C   connect to it (use ZSACPT to accept incoming connections after
C   setting the socket mode).
C
C   Inputs:
C
C      SOCK     I      An integer designating the socket to use, as
C                       returned by ZSCREA.
C                       Precondition: SOCK .NE. 0
C      QLEN     I      The number of pending connections to be queued
C                       for servicing.
C                       Precondition: QLEN .GE. 0
C   Outputs:
C
C      IRET     I      Return code: 0 indicates that the socket is in
C                                     listen mode
C                                   1 indicates an invalid argument
C                                     value
C                                   2 indicates that a system error
C                                     occurred
C                                  99 indicates that the network
C                                     interface is not implemented
C
C   Usage notes:
C
C   - The caller should interpret IRET and issue any error messages
C
C   Generic version: no network interface
C-----------------------------------------------------------------------
      INTEGER   SOCK, QLEN, IRET
C-----------------------------------------------------------------------
      IRET = 99
C
 999  RETURN
      END


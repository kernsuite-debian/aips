      SUBROUTINE ZSCONN (SOCK, SRV, SRVLEN, SVC, SVCLEN, IRET)
C-----------------------------------------------------------------------
C! connect to another socket
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
C   Make a connection to a another socket.
C
C   Inputs:
C
C      SOCK     I      An integer designating the socket to be used as
C                       the local endpoint of the connection, as
C                       returned by ZSCREA.
C                      Precondition: SOCK .NE. 0
C      SRV      C*?    The name of the machine on which the server is
C                       running
C      SRVLEN   I      The number of characters in SRV.
C                      Precondition: SVCLEN .GT. 0 and
C                                    SRV(1:SRVLEN) .NE. ' '
C      SVC      C*?    The name of the service.
C      SVCLEN   I      The number of characters in SVC.
C                      Precondition: SVCLEN .GT. 0 and
C                                    SVC(1:SVCLEN) .NE. ' '
C
C   Outputs:
C
C      IRET     I      Return code: 0 connection established
C                                   1 bad argument values
C                                   2 system error while obtaining
C                                     information about the server
C                                   3 system error while obtaining
C                                     information about the service
C                                   4 system error while making the
C                                     connection
C                                   5 other system exceptions
C                                  99 indicates that the network
C                                     interface is not implemented
C
C    Usage notes:
C
C    - The caller should interpret IRET and issue any error messages
C
C    Generic version: no network interface
C-----------------------------------------------------------------------
      INTEGER   SOCK, SRVLEN, SVCLEN, IRET
      CHARACTER SRV*(*), SVC*(*)
C-----------------------------------------------------------------------
      IRET = 99
C
 999  RETURN
      END

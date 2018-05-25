      SUBROUTINE ZROPN2 (FCB, SRVLEN, SRVNAM, HSTLEN, HOSTNM, IRET)
C-----------------------------------------------------------------------
C! request a connection to a server -- generic version
C# IO-remote Z2
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2004
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
C   Request a connection to a server.
C   Inputs:
C      FCB         I(*)          File control block.
C      SRVLEN      I             Length of server name including
C                                trailing blanks.
C      SRVNAM      H*(*)         Server name.
C      HSTLEN      I             Length of host name including trailing
C                                blanks.
C      HOSTNM      H*(*)         Host name.
C   Output:
C      IERR        I             Error return status:
C                                   0 => channel opened successfully
C                                   1 => request failed
C   Generic version (stubbed)
C-----------------------------------------------------------------------
      INTEGER   FCB(*), SRVLEN , HSTLEN, IRET
      HOLLERITH SRVNAM(*), HOSTNM(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 1
      MSGTXT = 'ZROPN2 REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (6)
C
 999  RETURN
      END

      SUBROUTINE ZVTPO2 (FCB, PLEN, PNAME, IERR)
C-----------------------------------------------------------------------
C! open connection in client (virtual-tape) to server (remote&real-tape)
C# Tape Z2
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   ZVTPO2 opens the connection in the Virtual tape program to the
C   remote computer which provides the actual tape device.
C   Inputs:
C      FCB     I(*)   File descriptor
C      PLEN    I      Length of PNAME
C      PNAME   H(*)   Name of remote port as aipsmtn:machine where n
C                     is a number 1-NTapes and machine is the remote
C                     machine name.
C                     ' ' => real tape
C   Output:
C      IERR    I      Error: 0 => okay
C   Generic version - stub.
C-----------------------------------------------------------------------
      INTEGER   FCB(*), PLEN, IERR
      HOLLERITH PNAME(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 8
      MSGTXT = 'ZVTPO2: REQUIRES DEVELOPMENT; GENERIC IS A STUB'
      CALL MSGWRT (8)
C
 999  RETURN
      END

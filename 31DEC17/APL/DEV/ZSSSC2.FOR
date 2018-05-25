      SUBROUTINE ZSSSC2 (FCB, IERR)
C-----------------------------------------------------------------------
C! Close SUN Screen Server device, flushing any buffer
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
C   deassigns via 'close' certain devices here the SSS TV socket
C   If buffered, write the last buffer first      
C   Inputs:
C      FCB    I(*)   file control block of open socket
C   Output:
C      IERR   I      error code: 0 => okay
C                       1 => error in close
C                       2 => error in flush buffer
C   Generic stub
C-----------------------------------------------------------------------
      INTEGER   FCB(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 8
      MSGTXT = 'ZSSSC2: REQUIRES DEVELOPMENT; GENERIC IS A STUB'
      CALL MSGWRT (8)
C
 999  RETURN
      END

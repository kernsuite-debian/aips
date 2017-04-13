      SUBROUTINE ZTTOP2 (DLEN, DEVTTY, IERR)
C-----------------------------------------------------------------------
C! open a message terminal
C# Z2 Terminal
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
C   Open a message terminal for line buffered I/O with exclusive use
C   Inputs:
C      DLEN     I          Length in characters of DEVTTY
C      DEVTTY   C*(dlen)   Terminal device name
C   Output:
C      IERR     I          Error return code: 0 => no error
C                             1 => error
C   Generic version - stubbed.  See APLCVEX for example.
C-----------------------------------------------------------------------
      INTEGER   DLEN, IERR
      CHARACTER DEVTTY*(*)
C
C-----------------------------------------------------------------------
      IERR = 8
      WRITE (6,1000)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (' ZTTOP2: REQUIRES LOCAL DEVELOPMENT')
      END

      SUBROUTINE ZMEMR2 (GET, KBYTES, BASE, ADDR, OFFSET, IERR)
C-----------------------------------------------------------------------
C! allocate and free dynamic memory sections
C# Z2 System
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 2005, 2009
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
C   Allocates or frees memory blocks                   .
C   Inputs:
C      GET     I      0 get memory, 1 free memory
C      KBYTES  I      real KILO bytes to get, ignored on free
C      BASE    I      address of Fortran "buffer"
C   In/out:
C      ADDR    L      Out on get: address pointer allocated (a long)
C                     In on free: address pointer to free
C   Output:
C      OFFSET  L      get only: subscript in Fortran buffer for start
C                        of allocated memory
C      IERR    I      Error return code: 0 => no error
C                        1 => error, 2 => stubbed
C   Generic version stubbed
C-----------------------------------------------------------------------
      INTEGER   GET, KBYTES, BASE, IERR
      LONGINT   OFFSET, ADDR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      OFFSET = 0
      ADDR = 0
C
      MSGTXT = 'ZMEMR2 REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (8)
C
 999  RETURN
      END


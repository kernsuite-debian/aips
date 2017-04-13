      SUBROUTINE ZTPWA2 (FCB, IERR)
C-----------------------------------------------------------------------
C! wait for read/write from/to a tape device
C# Z2 IO-basic Tape
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
C   Wait for the completion of an asynchronous tape I/O operation and
C   returns its completion status and 8-bit byte count.
C   Inputs:
C      FCB    I(*)   File I/O control block
C   Output:
C      IERR   I      Error return  0 => ok
C                       3 => IO error
C                       4 => end of file
C                       5 => beginning of tape
C                       6 => end of tape
C                       7 => wait service error
C   Generic version - stub
C-----------------------------------------------------------------------
      INTEGER   FCB(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZTPWA2 REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END

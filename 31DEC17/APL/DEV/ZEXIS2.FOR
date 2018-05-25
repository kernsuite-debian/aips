      SUBROUTINE ZEXIS2 (PNAME, ISIZE, SYSERR, IERR)
C-----------------------------------------------------------------------
C! return size of disk file and if  it exists
C# Z2 IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   Determine the existence of a file and its size in AIPS-bytes
C   (1/2 of a local integer).
C   Inputs:
C      PNAME   H*48   Physical file name (H for Macro, C, ...)
C   Output:
C      SIZE    I      File size in AIPS 256-word blocks (if file exists,
C                     else 0)
C      SYSERR  I      System error code
C      IERR    I      Error return code: 0 => file exists
C                       1 => file does not exist
C                       2 => inputs error
C                       3 => other error
C   Generic version - stub
C-----------------------------------------------------------------------
      HOLLERITH PNAME(12)
      INTEGER   ISIZE, SYSERR, IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZEXIS2: REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END

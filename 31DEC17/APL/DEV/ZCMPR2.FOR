      SUBROUTINE ZCMPR2 (FCB, PNAME, NBLCKS, MAP, SYSERR, IERR)
C-----------------------------------------------------------------------
C! truncate a disk file, returning blocks to the system
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
C   Truncates a disk file.  It is assumed that the file is open.
C   Inputs:
C      FCB     I(*)   File descriptor from FTAB file control block
C      PNAME   H*48   File name (Hollerith for Macro, C, ...)
C      MAP     I      I/O type: 0 => non-map (single buffered)
C                               1 => map (double buffered)
C   In/out:
C      NBLCKS  I      Desired/actual size in 256-integer blocks
C   Output:
C      IERR    I      Error code: 0 = success
C                        1 = file name translation fails
C                        2 = error occurred
C                        3 = temporary file not deleted
C   Generic version - stub
C-----------------------------------------------------------------------
      INTEGER   FCB(*), MAP, NBLCKS, SYSERR, IERR
      HOLLERITH PNAME(12)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZCMPR2 REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END

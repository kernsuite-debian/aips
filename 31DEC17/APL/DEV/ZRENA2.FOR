      SUBROUTINE ZRENA2 (OLDNAM, NEWNAM, SYSERR, IERR)
C-----------------------------------------------------------------------
C! rename a file
C# Z2 IO-basic
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
C   Renames a file.
C   Inputs:
C      OLDNAM   H*48   Old physical file name (Hollerith for Macro...)
C      NEWNAM   H*48   New physical file name (Hollerith for Macro...)
C   Output:
C      SYSERR   I      System error code
C      IERR     I      Error return code: 0 => no error
C                         2 => old file not found
C                         3 => device not found
C                         4 => old file busy
C                         6 => new name already exists
C                         7 => other
C   Generic version - STUB
C-----------------------------------------------------------------------
      HOLLERITH OLDNAM(12), NEWNAM(12)
      INTEGER   SYSERR, IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZRENA2 REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END

      SUBROUTINE ZERRO2 (SYSERR, ELEN, ERRMSG)
C-----------------------------------------------------------------------
C! return system error message for given system error code
C# Z2 Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   This routine translates system error codes into an ASCII message
C   text to be returned for later use with MSGWRT.
C   Inputs:
C      SYSERR   I       The error code returned by a system service,
C                       right justified and padded on the left with
C                       zeros.
C   In/out:
C      ELEN     I       In - max length error message
C                       Out - actual length
C   Outputs:
C      ERRMSG   C*(*)   A one-line message describing the error
C                       CHARACTER on VMS, HOLLERITH on rest
C   Generic version - stub.
C-----------------------------------------------------------------------
      HOLLERITH ERRMSG(*)
      INTEGER   SYSERR, ELEN
C
      INTEGER   ITRIM
      CHARACTER ERROR*80
C-----------------------------------------------------------------------
      ERROR = 'ZERRO2: REQUIRES LOCAL DEVELOPMENT'
      ELEN = ITRIM (ERROR)
      CALL CHR2H (80, ERROR, 1, ERRMSG)
C
 999  RETURN
      END

      SUBROUTINE ZMKTMP (FLEN, FILNAM, IERR)
C-----------------------------------------------------------------------
C! convert a "temporary" file name into a unique name
C# Z2 IO-basic
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
C   Form a unique, fully qualified, temporary file name.
C   Inputs:
C      FLEN     I       Length of "filnam"
C   In/Out:
C      FILNAM   H*(*)   File name with the extension .XXXXXX, (e.g.,
C                       "ZXLPRT.XXXXXX").  The extension will be
C                       transformed by 'mktemp' to make "filnam" a
C                       unique file name.
C   Output:
C      IERR     I       Error return code: 0 => no error
C                          1 => inputs wrong
C                          3 => filnam too short
C   Generic version - stub
C-----------------------------------------------------------------------
      INTEGER   FLEN, IERR
      HOLLERITH FILNAM(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 1
      MSGTXT = 'ZMKTMP: REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
      CALL CHR2H (FLEN, MSGTXT, 1, FILNAM)
C
 999  RETURN
      END

      SUBROUTINE ZDIE
C-----------------------------------------------------------------------
C! closes a task at system level: delete /tmp/<task><npops>.<pid>
C# Z System
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
C   Deletes the /tmp/<task><npops>.<pid> file
C   For AIPS, deletes the guest TV account lock file if any
C   Could do more if needed
C   Generic - calls ZDIE2
C-----------------------------------------------------------------------
C
      INTEGER   IERR
      HOLLERITH HNAME(2)
      CHARACTER ANAME*6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Task lock file
      CALL BLDTNM (TSKNAM, NPOPS, ANAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       parameters must be reasonable
      CALL CHR2H (6, ANAME, 1, HNAME)
      CALL ZDIE2 (HNAME)
C
 999  RETURN
      END

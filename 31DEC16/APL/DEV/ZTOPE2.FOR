      SUBROUTINE ZTOPE2 (LUN, FILNAM, WAIT, SYSERR, IERR)
C-----------------------------------------------------------------------
C! open text file for ZTOPEN
C# Z2 Text
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
C   actually opens a text file
C   Inputs:
C      LUN      I       Logical unit number to use
C      FILNAM   C*(*)   Actual file name
C      WAIT     L       Do we wait for file to be available?
C   Outputs:
C      SYSERR   I       System error code
C      IERR     I       Error return: 0 => okay
C                         2 => file not found
C                         4 => file locked
C                         6 => other open errors
C   Generic version - simple fortran open, ignores WAIT, returns only
C   error 6 (or 0)
C-----------------------------------------------------------------------
      INTEGER   LUN, SYSERR, IERR
      CHARACTER FILNAM*(*)
      LOGICAL   WAIT
C
      INTEGER   ITRIM, I
C-----------------------------------------------------------------------
      I = ITRIM (FILNAM)
      OPEN (UNIT=LUN, FILE=FILNAM(1:I), STATUS='OLD', IOSTAT=SYSERR)
      IF (SYSERR.EQ.0) THEN
         IERR = 0
      ELSE
         IERR = 6
         END IF
C
 999  RETURN
      END

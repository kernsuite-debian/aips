      SUBROUTINE ZGMAIL (FILNAM, IERR)
C-----------------------------------------------------------------------
C! mail gripe text file to aipsmail@nrao.edu
C# Z System
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
C   ZGMAIL is a utility of the Gripes system which mails a copy of the
C   gripe to aipsmail@nrao.edu.
C   Input:
C      FILNAM   C*(*)   Full path name to text file to be mailed.
C                       File is deleted upon succesful completion
C   Output:
C      IERR     I       Error code: > 0 => failed and file still exists
C   Generic version - stub
C-----------------------------------------------------------------------
      CHARACTER FILNAM*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Not worth a STOP however
      MSGTXT = 'ZGMAIL REQUIRES LOCAL DEVELOPMENT: GRIPE NOT MAILED'
      CALL MSGWRT (9)
      IERR = 2
C
 999  RETURN
      END

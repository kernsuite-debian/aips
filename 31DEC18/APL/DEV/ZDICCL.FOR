      SUBROUTINE ZDICCL (LUN, FNAME, IRET)
C-----------------------------------------------------------------------
C! Close a Dicomed image file and (optionally) spool it
C# Z
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
C   Close a Dicomed image file and save it under a user-specified name
C   or spool it to the film recorder and delete it.
C
C   Input:
C      LUN       I       The logical unit number for the file.
C      FNAME     C*(*)   The name the file will be stored under; if
C                        no name is given the file will be placed in
C                        the film recorder queue automatically and
C                        not stored.
C
C   Output:
C      IRET      I       Return status:
C                          0 -> success
C
C   Generic version: stubbed.
C-----------------------------------------------------------------------
      INTEGER   LUN, IRET
      CHARACTER FNAME*(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      WRITE (MSGTXT, 1000)
      CALL MSGWRT (8)
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZDICCL REQUIRES LOCAL DEVELOPMENT')
      END

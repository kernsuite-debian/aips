      SUBROUTINE ZOLAVF (FCB, NFILES, IERR)
C-----------------------------------------------------------------------
C! advance files on an on-line "tape" device
C# Z2 IO-basic Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 2003
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
C   Advance files for on-line "tape devices"
C   Inputs:
C      FCB      I(*)   File control block for opened tape drive
C      NFILES   I      = 0 - start today's files
C                      < 0 - start yesterday's file
C                      > 0 - end of todays's file
C   Output:
C      IERR    I      Error return code: 0 => no error
C                        1 => at start today's despite NFILES < 0
C                        2 => no such operation
C                              3 => device not found
C                              6 => other open error
C   Generic version - stub
C-----------------------------------------------------------------------
      INTEGER   FCB(*), NFILES, IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZOLAVF REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END

      SUBROUTINE ZTPOP2 (FCB, PNAME, MODE, IERR)
C-----------------------------------------------------------------------
C! open a tape device for double-buffer, asymchronous IO
C# Z2 IO-basic Tape
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
C   Open a tape drive for "map" (double buffered, asynchronous) I/O.
C   Inputs:
C      PNAME   C*48   Physical file name
C      MODE    I      Mode indicator: 0 => read only
C                                     2 => read/write
C   Output:
C      FCB     I(*)   File control block for opened tape drive
C      IERR    I      Error return code: 0 => no error
C                              2 => no such logical device
C                              3 => device not found
C                              6 => other open error
C   Generic version - stub
C-----------------------------------------------------------------------
      INTEGER   FCB(*), MODE, IERR
      CHARACTER PNAME*48
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZTPOP2 REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END

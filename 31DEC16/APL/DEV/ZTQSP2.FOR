      SUBROUTINE ZTQSP2 (DOALL, MSLEV, NPROC)
C-----------------------------------------------------------------------
C! display AIPS account or all processes running on the system
C# Z2 System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   Displays information on the user's terminal regarding AIPS account
C   originated processes or all processes running on the system.
C   Inputs:
C      DOALL   R   > 0.0 => display all processes
C      MSLEV   I   Message level to use in calling MSGWRT.
C      NPROC   I   Max POPS number (#interactive + #batch + 1)
C   Generic version - stub.
C-----------------------------------------------------------------------
      REAL      DOALL
      INTEGER   MSLEV, NPROC
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Requires local development.
      WRITE (MSGTXT,9999)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 9999 FORMAT ('ZTQSP2: ROUTINE REQUIRES LOCAL DEVELOPMENT')
      END

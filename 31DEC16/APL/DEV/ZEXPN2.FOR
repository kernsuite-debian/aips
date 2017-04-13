      SUBROUTINE ZEXPN2 (FCB, MAP, NRECS, IERR)
C-----------------------------------------------------------------------
C! expand an open disk file
C# Z2 IO-basic
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
C   Expands an open disk file, reserving the disk space.
C   Inputs:
C      FCB     I(*)   File control block for opened disk file
C      MAP     I      Double buffer file? 1 => yes, 0 => no
C   In/out:
C      NRECS   I      In:  number of 256-integer records to expand
C                     Out: number of records expanded
C   Output:
C      IERR    I      Error return code: 0 => no error
C                        1 => error
C   Generic version - stub
C--------------------------------------------------------------------
      INTEGER   FCB(*), MAP, NRECS, IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZEXPN2: REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END

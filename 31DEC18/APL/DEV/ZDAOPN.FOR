      SUBROUTINE ZDAOPN (FCB, PNAME, MAP, EXCL, IERR)
C-----------------------------------------------------------------------
C! open the specified disk file
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
C   Opens a disk file for map (double buffered) or non-map (single
C   buffered) I/O and shared or exclusive use.
C   Inputs:
C      pname   H*48   Physical file name (H for Macro, C, ...')
C      map     I      I/O type: 0 => non-map (single buffered)
C                               1 => map (double buffered)
C      excl    I      Usage mode indicator: 0 => shared
C                                           1 => exclusive
C   Output:
C      fcb     I(*)   File control block for disk file involved
C      ierr    I      Error return code: 0 => no error
C                        2 => file not found
C                        3 => volume/logical not found
C                        4 => exclusive use requested but not ok
C                        6 => other open error
C   Generic version - stub
C-----------------------------------------------------------------------
      INTEGER   FCB(*), MAP, EXCL, IERR
      HOLLERITH PNAME(12)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZDAOPN: REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END

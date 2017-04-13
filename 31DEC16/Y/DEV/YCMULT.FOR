      SUBROUTINE YCMULT (FACTOR, IERR)
C-----------------------------------------------------------------------
C! write desired TV character scaling factor
C# Y1 TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 2012
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
C   YCMULT orders a change in the character scaling factor.
C   Inputs:
C      FACTOR   I   scale factor = 1, 2, 3, 4, or 5
C   Output:
C      IERR     I   Error status, 0 means success.
C   Version for generic TVs - stubbed
C-----------------------------------------------------------------------
      INTEGER   FACTOR, IERR
C
      INTEGER   DAT(4), NWSD, NWVD, OPCODE, BUFFER(3)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       Check inputs.
      MSGTXT = 'YCMULT IS STUBBED FOR NOW IN GENERIC VERSION'
      IERR = 2
C
 990  IF (IERR.NE.0) CALL MSGWRT (6)
C
 999  RETURN
      END

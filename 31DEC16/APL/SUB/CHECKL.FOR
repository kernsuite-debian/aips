      SUBROUTINE CHECKL (SUBNAM)
C-----------------------------------------------------------------------
C! checks LOCNUM common parameter
C# Plot-util Coordinates
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
C   checks the value of LOCNUM in the DLOC common: if it is out of range
C   it is set to 1 and reported
C   Inputs:
C      SUBNAM   C*(*)   Calling subroutine name
C   Output in common:
C      LOCNUM   I       location common subscript in range
C-----------------------------------------------------------------------
      CHARACTER SUBNAM*(*)
C
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF ((LOCNUM.LT.1) .OR. (LOCNUM.GT.NUMLOC)) THEN
         WRITE (MSGTXT,1000) SUBNAM, LOCNUM
         CALL MSGWRT (6)
         LOCNUM = 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A,' LOCNUM =',I12,' OUT OF RANGE, FORCED TO 1')
      END

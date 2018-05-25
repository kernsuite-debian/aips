@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QGET2 (HOST, AP, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Move integer data from "AP" to "host" memory.
C# AP-appl
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
C   Copy integer data from "AP" to "host"
C   Inputs:
C      AP     I     target position in APCORE; 0-based
C      N      I     number of elements to transfer
C   Output:
C      HOST   I(*)  data address in host
C-----------------------------------------------------------------------
      INTEGER   HOST(*)
      INTEGER   AP, I, N
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C
C     DO 100 I = 1,N
C        HOST(I) = APCORI(AP+I)
C100     CONTINUE
      CALL SCOPY(N,APCORI(AP+1),1,HOST,1)
C
      RETURN
      END

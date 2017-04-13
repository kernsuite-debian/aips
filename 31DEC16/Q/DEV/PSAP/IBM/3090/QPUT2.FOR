@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QPUT2 (HOST, AP, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Move integer data from "host" to "AP" memory.
C# AP-util
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
C   Copy integers from "host" to "AP"
C   Inputs:
C      HOST  I(*)  data address in host
C      AP    I     target position in APCORE; 0-relative
C      N     I     number of elements to transfer
C-----------------------------------------------------------------------
      INTEGER   HOST(*)
      INTEGER   AP, N, I
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Copy
C     DO 100 I = 1,N
C        APCORI(AP+I) = HOST(I)
C100     CONTINUE
      CALL SCOPY(N,HOST,1,APCORI(AP+1),1)
C
      RETURN
      END

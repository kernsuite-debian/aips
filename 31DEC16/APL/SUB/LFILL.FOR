      SUBROUTINE LFILL (N, C, A)
C-----------------------------------------------------------------------
C! fills a logical array with a logical constant
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1996
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
C   fills a logical array with a constant
C   Inputs:
C      N   I      number of points to be filled in array
C      C   L      constant
C   Output:
C      A   L(N)   array
C-----------------------------------------------------------------------
      INTEGER   N
      LOGICAL   C, A(*)
C
      INTEGER   I
C-----------------------------------------------------------------------
      IF (N.GT.0) THEN
         DO 10 I = 1,N
            A(I) = C
 10         CONTINUE
         END IF
C
 999  RETURN
      END

      SUBROUTINE CFILL (N, CFROM, CTO)
C-----------------------------------------------------------------------
C! fills an array of character strings with an input string
C# Utility
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
C   FILL places CFROM into an array of N character strings, CTO
C   Inputs:
C      N      I        number of words to be copied
C      CFROM  C*       input string
C   Outputs:
C      CTO    C*(N)    output array
C-----------------------------------------------------------------------
      INTEGER   N
      CHARACTER*(*) CFROM, CTO(*)
C
      INTEGER   I
C-----------------------------------------------------------------------
C                                       If something to copy
      IF (N.GT.0) THEN
C                                       For all items
         DO 10 I = 1,N
C                                       Copy the item
            CTO(I) = CFROM
 10         CONTINUE
         END IF
C
      RETURN
      END

      SUBROUTINE FMATCH (NS, AS, NL, AL, IL)
C-----------------------------------------------------------------------
C! returns pointer to location of small array in a bigger array
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
C   FMATCH returns a pointer to the location in a large array at which a
C   given smaller array begins.  NOTE: do not use Hollerith.
C   Inputs: NS    I        number words in smaller array
C           AS    I(NS)    smaller "search" array
C           NL    I        number of words in larger array
C           AL    I(NL)    larger "test" array
C   Output: IL    I        index in larger array (0 if no match)
C-----------------------------------------------------------------------
      INTEGER   NS, AS(*), NL, AL(*), IL
C
      INTEGER   I, J, N
C-----------------------------------------------------------------------
      IL = 0
      IF (NL.LT.NS) GO TO 999
         N = NL - NS + 1
         DO 20 I = 1,N
            DO 10 J = 1,NS
               IF (AS(J).NE.AL(I+J-1)) GO TO 20
 10            CONTINUE
            IL = I
            GO TO 999
 20         CONTINUE
C
 999  RETURN
      END

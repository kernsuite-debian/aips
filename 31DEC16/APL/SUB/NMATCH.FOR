      INTEGER FUNCTION NMATCH (IL, IH, IV, IARRAY)
C-----------------------------------------------------------------------
C! returns next word in INTEGER array not matching a specified constant
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
C   NMATCH returns array position of next non-matching word
C   Inputs:
C      IL      I       where to start in array
C      IH      I       where to stop in array
C      IV      I       value to be matched
C      IARRAY  I(>IH)  array to be tested
C   Outputs:
C      NMATCH  I       location >= IL of 1st word in IARRAY not equal IV
C                      = IH+1 when all words match
C-----------------------------------------------------------------------
      INTEGER   IL, IH, IV, IARRAY(*)
C
      INTEGER   I
C-----------------------------------------------------------------------
      DO 10 I = IL,IH
         IF (IARRAY(I).NE.IV) GO TO  20
 10      CONTINUE
      I = IH + 1
C
 20   NMATCH = I
C
 999  RETURN
      END

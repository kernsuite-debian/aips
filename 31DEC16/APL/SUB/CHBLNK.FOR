      SUBROUTINE CHBLNK (NT, NS, CH, NP)
C-----------------------------------------------------------------------
C! returns position of first non-blank character in portion of string
C# Character
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
C   CHBLNK searches a string to find the first non-blank character
C   Inputs: NT   I       Total length of string
C           NS   I       Start position in string for search
C           CH   C*(*)   String
C   Output: NP   I       first non-blank character position (from start
C                        of string)
C-----------------------------------------------------------------------
      CHARACTER CH*(*)
      INTEGER   NT, NS, NP
C
      INTEGER   IS
C-----------------------------------------------------------------------
      NP = 0
      IF (NT.LE.0) GO TO 999
      IS = NS
      IF (IS.LE.0) IS = 1
      IF (IS.GT.NT) GO TO 999
      DO 10 NP = IS,NT
         IF (CH(NP:NP).NE.' ') GO TO 999
 10      CONTINUE
      NP = 0
C
 999  RETURN
      END

      SUBROUTINE CHFILL (NCHAR, CHAR, NBP, STRING)
C-----------------------------------------------------------------------
C! fills portion of HOLLERITH string with a specified character
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
C   CHFILL fills a HOLLERITH string with a character
C   Inputs: NCHAR   I       Number of char positions to fill
C           CHAR    H       Char in char position 1
C           NBP     I       Start char position to fill
C   Output: STRING  H(*)    Filled string
C-----------------------------------------------------------------------
      HOLLERITH CHAR, STRING(*)
      INTEGER   NCHAR, NBP
C
      INTEGER   IT, NB, J1, J2, J
C-----------------------------------------------------------------------
      IF (NCHAR.LE.0) GO TO 999
         CALL ZGETCH (IT, CHAR, 1)
         NB = MAX (NBP, 1)
         DO 10 J = 1,NCHAR
            J1 = (NB-1) / 4 + 1
            J2 = MOD (NB-1, 4) + 1
            CALL ZPUTCH (IT, STRING(J1), J2)
            NB = NB + 1
 10         CONTINUE
C
 999  RETURN
      END

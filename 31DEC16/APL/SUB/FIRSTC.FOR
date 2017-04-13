      CHARACTER*1 FUNCTION FIRSTC (STRING)
C-----------------------------------------------------------------------
C! returns first non-blank letter in STRING forced to upper case
C# CHARACTER
C-----------------------------------------------------------------------
C;  Copyright (C) 2013
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
C   FIRSTC returns the first non-blank character in STRING forced to
C   upper cae
C   Inputs:
C      STRING   C*(*)   Character buffer
C   Outputs:
C      FIRSTC   C*1    First non-blank (or blank if none)
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
C
      INTEGER   I, N, JTRIM
      CHARACTER CC*1
C-----------------------------------------------------------------------
      N = JTRIM (STRING)
      FIRSTC = ' '
      DO 10 I = 1,N
         IF (STRING(I:I).NE.' ') THEN
            CC = STRING(I:I)
            CALL CHLTOU (1, CC)
            FIRSTC = CC
            GO TO 999
            END IF
 10      CONTINUE
C
 999  RETURN
      END

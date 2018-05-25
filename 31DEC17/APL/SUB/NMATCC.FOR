      INTEGER FUNCTION NMATCC (IL, CV, STRING)
C-----------------------------------------------------------------------
C! returns next character in a string not matching a specified constant
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
C   NMATCC returns array position of next non-matching character
C   Inputs:
C      IL      I       where to start in array
C      CV      C*1     value to be matched
C      STRING  C*(*)   character string to be tested
C   Outputs:
C      NMATCC  I       location >= IL of 1st char in STRING not equal CV
C                      = IH+1 when all chars match (IH = LEN(STRING))
C-----------------------------------------------------------------------
      INTEGER   IL
      CHARACTER CV*1, STRING*(*)
C
      INTEGER   I, IH
C-----------------------------------------------------------------------
      IH = LEN (STRING)
      IF (IH.GE.IL) THEN
         DO 10 I = IL,IH
            IF (STRING(I:I).NE.CV) GO TO  20
 10         CONTINUE
         END IF
      I = IH + 1
C
 20   NMATCC = I
C
 999  RETURN
      END

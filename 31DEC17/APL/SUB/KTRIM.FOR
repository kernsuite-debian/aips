      INTEGER FUNCTION KTRIM (STRING)
C-----------------------------------------------------------------------
C! clears bad chars, returns length of CHAR variable to last non-blank
C# Character Utility
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
C   Function to determine length of a string. I.e., it blanks all of the
C   STRING following the first null or non-printable character, then
C   trims trailing blanks.  Use with calls like:
C         TRIMMED = GROSS(1:KTRIM(GROSS))
C   If there can't be NULL characters in STRING, use ITRIM.
C   In/out:
C      STRING   C*(*)   String to be cleared of nulls, length found
C   Output:
C      KTRIM    I       Length to last non-blank of cleared string
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
C
      INTEGER   ILEN, I, IBL
      CHARACTER NULL*1
C-----------------------------------------------------------------------
      ILEN = LEN (STRING)
C                                       check forwards for null
      NULL = CHAR (0)
      IBL = ICHAR (' ')
      I = 0
10    CONTINUE
         I = I + 1
         IF (I.GT.ILEN) GO TO 20
         IF ((STRING(I:I).NE.NULL) .AND. (ICHAR(STRING(I:I)).GE.IBL))
     *      GO TO 10
         STRING(I:ILEN) = ' '
20    KTRIM = I
C                                       look backwards for non-blank
 30   CONTINUE
         KTRIM = KTRIM - 1
         IF (KTRIM.LT.1) GO TO 999
         IF (STRING(KTRIM:KTRIM).EQ.' ') GO TO 30
C
 999  RETURN
      END

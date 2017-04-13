      INTEGER FUNCTION JTRIM (STRING)
C-----------------------------------------------------------------------
C! clears nulls, returns length of CHARACTER variable to last non-blank
C# Character Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2008
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
C   Function to determine length of a string. I.e., it blanks all of the
C   STRING following the first non-printable character (< ' '), then
C   trims trailing blanks.  Use with calls like:
C         TRIMMED = GROSS(1:JTRIM(GROSS))
C   This is safer than ITRIM.  TABs are changed to blanks but do not
C   cause a string termination like the other non-printing characters.
C   In/out:
C      STRING   C*(*)   String to be cleared of nulls, length found
C   Output:
C      JTRIM    I       Length to last non-blank of cleared string
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
C
      INTEGER   ILEN, I
      CHARACTER TABCHR
C-----------------------------------------------------------------------
      ILEN = LEN (STRING)
      CALL ZTAB (TABCHR)
C                                       check forwards for null
      I = 0
10    I = I + 1
      IF (I.LE.ILEN) THEN
         IF (STRING(I:I).EQ.TABCHR) STRING(I:I) = ' '
         IF (STRING(I:I).GE.' ') GO TO 10
         STRING(I:ILEN) = ' '
         END IF
      JTRIM = I
C                                       look backwards for non-blank
 30   JTRIM = JTRIM - 1
      IF (JTRIM.GE.1) THEN
         IF (STRING(JTRIM:JTRIM).EQ.TABCHR) STRING(JTRIM:JTRIM) = ' '
         IF (STRING(JTRIM:JTRIM).EQ.' ') GO TO 30
         END IF
C
 999  RETURN
      END

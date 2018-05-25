      SUBROUTINE STRTKN (STRING, TOKENS, NTOK)
C-----------------------------------------------------------------------
C! split a string into tokens
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
C   Split string into tokens.  A token is a string of consecutive
C   non-blank characters that is separated from neighbouring tokens by
C   one or more blank characters.
C
C   This routine may be used to extract fields from a line read from a
C   text file that does not used fixed widths for its fields.
C
C   It is the caller's responsibility to dimension TOKENS so as to
C   be large enough to hold the largest expected output.  This is not
C   checked.
C
C   Inputs:
C      STRING    C*(*)      String to be split
C
C   Output:
C      TOKENS    C(*)*(*)   Tokenized version of STRING
C      NTOK      I          The number of tokens in the string
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), TOKENS(*)*(*)
      INTEGER   NTOK
C
      INTEGER   I, J
C------------------------------------------------------------------------
      NTOK = 0
      I = 1
      J = 1
   10 IF (I .LE. LEN (STRING)) THEN
C
C        Find next token:
C
   20    IF (I .LE. LEN (STRING)) THEN
            IF (STRING(I:I) .EQ. ' ') THEN
               I = I + 1
               GO TO 20
            END IF
         END IF
         IF (I .LE. LEN (STRING)) THEN
C
C           Find the end of the token:
C
            J = I + 1
   30       IF (J .LE. LEN (STRING)) THEN
               IF (STRING(J:J) .NE. ' ') THEN
                  J = J + 1
                  GO TO 30
               END IF
            END IF
C
            NTOK = NTOK + 1
            TOKENS(NTOK) = STRING(I: J - 1)
            I = J + 1
         END IF
         GO TO 10
      END IF
      END

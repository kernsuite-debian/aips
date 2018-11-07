      SUBROUTINE MATCH (TEXT1, TEXT2, FOUND, N1, N2)
C-----------------------------------------------------------------------
C! Tests whether either of two strings is wholly contained in the other.
C# Character Parsing
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
C   tests whether either of two strings is wholly contained in the other
C   Inputs:
C      TEXT1   C*(*)   Input character strings.  Length should not
C      TEXT2   C*(*)   exceed 132 characters.
C   Outputs:
C      FOUND   I       Match results. 0: no match,
C                         1: case insensitive match.
C                         2: case sensitive match,
C      N1      I       > 0 => the starting character position of TEXT2
C                      in TEXT1.
C      N2      I       > 0 => the starting character position of TEXT1
C                      in TEXT2.
C   Author: Mark Calabretta, Australia Telescope.
C      Origin; 1988/Apr/28.  Code last modified; 1989/Sep/05.
C-----------------------------------------------------------------------
      INTEGER   FOUND, N1, N2
      CHARACTER TEXT1*(*), TEXT2*(*)
C
      INTEGER   I1, I2, J1, J2
      CHARACTER STR1*132, STR2*132
C-----------------------------------------------------------------------
C                                       Find the first and last non-
C                                       blank, non-tab characters
      STR1 = TEXT1
      STR2 = TEXT2
      CALL TXTLEN (STR1, I1, I2)
      CALL TXTLEN (STR2, J1, J2)
C                                       Try for a case sensitive match
      FOUND = 2
      N1 = INDEX (STR1(1:I2), STR2(J1:J2))
      N2 = INDEX (STR2(1:J2), STR1(I1:I2))
C                                       Perhaps a case insensitive match
      IF ((N1.LE.0) .AND. (N2.LE.0)) THEN
         FOUND = 1
         CALL CHLTOU (132, STR1)
         CALL CHLTOU (132, STR2)
         N1 = INDEX (STR1(1:I2), STR2(J1:J2))
         N2 = INDEX (STR2(1:J2), STR1(I1:I2))
         END IF
C                                       No match
      IF ((N1.LE.0) .AND. (N2.LE.0)) FOUND = 0
C
 999  RETURN
      END

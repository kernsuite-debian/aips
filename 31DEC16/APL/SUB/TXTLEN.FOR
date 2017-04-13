      SUBROUTINE TXTLEN (STRING, I1, I2)
C-----------------------------------------------------------------------
C! Find the first and last non-blank, non-tab characters in a string.
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
C   finds the positions of the first and last non-blank, non-tab
C   characters in a string.
C   Inputs:
C      STRING   C*(*)   Input character string.
C   Outputs:
C      I1       I       Position of the first non-blank, non-tab; one
C                       plus the length of the string if none
C      I2       I       Position of the last non-blank, non-tab; 0 if
C                       none
C   Author: Mark Calabretta, Australia Telescope.
C      Origin; 1988/May/23.  Code last modified; 1989/Sep/06.
C-----------------------------------------------------------------------
      INTEGER   I1, I2
      CHARACTER STRING*(*)
C
      INTEGER   NCH
      CHARACTER TAB*1
C-----------------------------------------------------------------------
      TAB = CHAR(9)
      NCH = LEN(STRING)
C                                       Search forward for the start.
      DO 10 I1 = 1,NCH
         IF (STRING(I1:I1).EQ.' ') GO TO 10
         IF (STRING(I1:I1).EQ.TAB) GO TO 10
         GO TO 20
 10      CONTINUE
C                                       Search backward for the end.
 20   DO 30 I2 = NCH,1,-1
         IF (STRING(I2:I2).EQ.' ') GO TO 30
         IF (STRING(I2:I2).EQ.TAB) GO TO 30
         GO TO 999
 30      CONTINUE
C
 999  RETURN
      END

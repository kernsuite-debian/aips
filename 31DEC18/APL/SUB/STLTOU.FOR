      SUBROUTINE STLTOU (NC, STR)
C-----------------------------------------------------------------------
C! converts any characters beween single quotes to upper case
C# POPS-lang Character
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2007
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
C   STLTOU converts any lower case characters found between single
C   quotes (') to upper case.  Lower case characters outside single
C   quotes are ignored.  If the close ' does not appear within the NC
C   characters of STR, then the characters following the open ' are not
C   converted.
C   Inputs:
C      NC      I       Number of characters in STR
C   In/out:
C      STR     C*(*)   String to be fixed up
C-----------------------------------------------------------------------
      CHARACTER STR*(*)
      INTEGER   NC
C
      CHARACTER IT
      INTEGER   I, LP, NP
      LOGICAL   INSIDE
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (NC.LE.0) GO TO 999
      INSIDE = .FALSE.
      LP = NC + 1
      DO 30 I = 1,NC
         IT = STR(I:I)
         IF (IT.EQ.'''') THEN
            IF (INSIDE) THEN
               NP = I - LP
               IF (NP.GT.0) CALL CHLTOU (NP, STR(LP:LP+NP-1))
               INSIDE = .FALSE.
               LP = NC + 1
            ELSE
               LP = I + 1
               INSIDE = .TRUE.
               END IF
            END IF
 30      CONTINUE
C
 999  RETURN
      END

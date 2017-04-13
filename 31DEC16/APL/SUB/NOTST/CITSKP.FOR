      SUBROUTINE CITSKP (S, N, I)
C-----------------------------------------------------------------------
C! KEYIN routine: Find next non blank character in a string.
C# Parsing
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999
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
C   Finds next non blank character in a loose string.
C    Inputs:
C      S(N)  C    String to search
C      N     I    Length of string
C      I     I    First character to search
C    Outputs:
C      I     I    Next non blank character, or N if none found.
C   T.J.PEARSON  1978 MAY 11
C   MODIFIED 1979 FEB 6  (VAX/VMS FORTRAN 4)
C   Modified  1985 June for AIPS and renamed
C-----------------------------------------------------------------------
      CHARACTER S*(*), BLANK*1, TAB*1, CR*1
      INTEGER   I, N
      CHARACTER ZSCHAR
      EXTERNAL  ZSCHAR
      DATA      BLANK /' '/
C-----------------------------------------------------------------------
      TAB = ZSCHAR ('tab')
      CR  = ZSCHAR ('cr')
   10 IF (I.GT.N) GO TO 999
         IF (S(I:I).NE.BLANK .AND. S(I:I).NE.TAB
     *       .AND. S(I:I).NE.CR) GO TO 999
         I = I + 1
         GO TO 10
C
 999  RETURN
      END

      SUBROUTINE CITC2I (S, N, I, IVALUE)
C-----------------------------------------------------------------------
C! KEYIN routine: parse an integer from a character string.
C# Parsing
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
C  CITC2I decodes an integer in the character string S starting at
C  location I;  I is updated to point after the end of the value on
C  return.
C   Inputs:
C     S(N)    C    Character string with value encoded
C     N       I    Length of S
C     I       I    Pointer in S to beginning of value.
C   Outputs:
C     I       I    Pointer in S to first character after value.
C     IVALUE  I    Integer value encodes in character string.
C   TJP 1977 DECEMBER 4  CIT 370/158
C   MODIFIED 1979 FEB 6  VAX/VMS FORTRAN 4
C   Modified 1985 June  for AIPS and renamed
C-----------------------------------------------------------------------
      CHARACTER S*(*), DIGITS*10
      INTEGER   IVALUE, N, I, IPOINT
      DATA      DIGITS /'0123456789'/
C-----------------------------------------------------------------------
      IVALUE = 0
C                                       Check for end of string
  10     IF (I.GT.N) GO TO 999
C                                       Find digit.
         IPOINT = INDEX (DIGITS, S(I:I))
C                                       Found any?
         IF (IPOINT.LE.0) GO TO 999
C                                       Add to value
         IVALUE = IVALUE * 10 + IPOINT - 1
         I = I + 1
         GO TO 10
C
 999  RETURN
      END

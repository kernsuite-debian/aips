      SUBROUTINE CITC2D (S, N, I, YVALUE)
C-----------------------------------------------------------------------
C! KEYIN routine: parses Double precision value from a character string
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
C  CITC2D evaluates a REAL encoded in string S beginning at I,
C  updating I. Only the characters 0123456789 are parsed; any other
C  characters will cause a return.
C   Inputs:
C     S(N)    C    Character string with value encoded.
C     N       I    Length of S
C     I       I    Pointer in S to beginning of value.
C   Outputs:
C     I       I    Pointer in S to first character after value.
C     YVALUE  D    Value encoded in character string.
C   T.J. PEARSON 1979 JUN 4  VAX/VMS FORTRAN 4
C   Modified 1985 June  for AIPS and renamed
C-----------------------------------------------------------------------
      CHARACTER S*(*), DIGITS*10
      INTEGER   N, I, IPOINT, INDEX
      DOUBLE PRECISION YVALUE
      DATA      DIGITS /'0123456789'/
C-----------------------------------------------------------------------
      YVALUE = 0.0D0
 10      IF (I.GT.N) GO TO 999
         IPOINT = INDEX (DIGITS, S(I:I))
         IF (IPOINT.LE.0) GO TO 999
         YVALUE = YVALUE * 10.0D0 + (IPOINT-1)
         I = I + 1
         GO TO 10
C
 999  RETURN
      END

      SUBROUTINE CITC2R (S, N, I, XVALUE)
C-----------------------------------------------------------------------
C! KEYIN routine: parses a floating value from a character string
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
C   CITC2R decodes a floating point value from character string S
C   starting at location I.  On return I is updated to position after
C   value.
C   Interpretes an imbedded colon (:) as "divide what follows by 60"
C   Inputs:
C     S(N)    C    Character string with value encoded
C     N       I    Length of S
C     I       I    Pointer in S to beginning of value.
C   Outputs:
C     I       I    Pointer in S to first character after value.
C     XVALUE  D    Floating value encoded in string.
C   T.J.PEARSON  1978 MAY 11
C   MODIFIED 1979 FEB 6  VAX/VMS FORTRAN 4
C   Modified 1985 June for AIPS and renamed
C-----------------------------------------------------------------------
      CHARACTER S*(*), COLON*1, PLUS*1, MINUS*1, POINT*1, CHEXP*4
      DOUBLE PRECISION XVALUE, YVALUE, FRACTN, FACT
      INTEGER   IVALUE, SIGN, N, I, ISTART, J
      LOGICAL   ISEXP
      DATA COLON, PLUS, MINUS, POINT,  CHEXP
     *   /  ':',  '+',   '-',   '.',  'EeDd'/
C-----------------------------------------------------------------------
      XVALUE = 0.0D0
      FACT = 1.0D0
      IF (I.GT.N) GO TO 999
      SIGN = +1
      IF (S(I:I).EQ.MINUS) THEN
C                                       Negative
         I = I + 1
         SIGN = -1
         CALL CITSKP (S, N, I)
      ELSE IF (S(I:I).EQ.PLUS) THEN
C                                       Leading "+"
         I = I + 1
         CALL CITSKP (S, N, I)
         END IF
      ISTART = I
 150  CALL CITC2D (S, N, I, YVALUE)
      XVALUE = XVALUE + FACT * YVALUE * SIGN
      IF (I.GT.N) GO TO 999
      IF (S(I:I).EQ.COLON) THEN
C                                       Colon
         I = I + 1
         FACT = FACT / 60.0D0
         GO TO 150
         END IF
C                                       Look for fractional part, if any
      IF (S(I:I).EQ.POINT) THEN
         J = I + 1
         CALL CITC2D (S, N, J, YVALUE)
         FRACTN = FACT * YVALUE * SIGN
         XVALUE = XVALUE + FRACTN * 10.D0**(I-J+1)
         I = J
         IF (I.GT.N) GO TO 999
         END IF
C                                       Look for exponent, if any;
C                                       an exponent may not stand alone
      ISEXP = INDEX (CHEXP, S(I:I)) .GT. 0
      IF (ISEXP .AND. (I.NE.ISTART)) THEN
         I = I + 1
         IF (I.GT.N) GO TO 999
         SIGN = +1
         IF (S(I:I).EQ.MINUS) THEN
            I = I+1
            SIGN = -1
         ELSE IF (S(I:I).EQ.PLUS) THEN
            I = I + 1
            END IF
         CALL CITC2I (S, N, I, IVALUE)
         XVALUE = XVALUE * (10.0D0 ** (SIGN * IVALUE))
         END IF
C
 999  RETURN
      END

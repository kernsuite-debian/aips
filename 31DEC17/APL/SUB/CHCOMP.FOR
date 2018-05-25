      SUBROUTINE CHCOMP (NCHAR, KP1, STR1, KP2, STR2, EQUAL)
C-----------------------------------------------------------------------
C! compares two HOLLERITH strings
C# Character
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
C   CHCOMP compares two character strings
C   Inputs: NCHAR  I       # characters to compare
C           KP1    I       starting character in string 1
C           STR1   H(*)    string 1
C           KP2    I       starting character in string 2
C           STR2   H(*)    string 2
C   Output: EQUAL  L       T => strings are same
C-----------------------------------------------------------------------
      HOLLERITH STR1(*), STR2(*)
      INTEGER   NCHAR, KP1, KP2
      LOGICAL   EQUAL
C
      INTEGER   KA1, KA2, I1, I2, J1, J2, C1, C2, I
C-----------------------------------------------------------------------
      EQUAL = NCHAR.LE.0
      IF (EQUAL) GO TO 999
         KA1 = MAX (1, KP1) - 1
         KA2 = MAX (1, KP2) - 1
         DO 10 I = 1,NCHAR
            I1 = KA1 / 4 + 1
            I2 = KA2 / 4 + 1
            J1 = MOD (KA1, 4) + 1
            J2 = MOD (KA2, 4) + 1
            CALL ZGETCH (C1, STR1(I1), J1)
            CALL ZGETCH (C2, STR2(I2), J2)
            IF (C1.NE.C2) GO TO 999
            KA1 = KA1 + 1
            KA2 = KA2 + 1
 10         CONTINUE
         EQUAL = .TRUE.
C
 999  RETURN
      END

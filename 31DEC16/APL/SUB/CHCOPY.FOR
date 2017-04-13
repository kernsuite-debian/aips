      SUBROUTINE CHCOPY (NCHAR, NP1, STR1, NP2, STR2)
C-----------------------------------------------------------------------
C! moves characters from one HELLERITH string to another
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
C   CHCOPY  moves characters from one HOLLERITH string to another
C   Inputs: NCHAR   I       Number of characters to move
C           NP1     I       Start char position in input string
C           STR1    H(*)    Input string
C           NP2     I       Start char position in output string
C   Output: STR2    H(*)    Output string
C-----------------------------------------------------------------------
      HOLLERITH STR1(*), STR2(*)
      INTEGER   NCHAR, NP1, NP2
C
      INTEGER   J1, J2, I1, I2, I, J, NA1, NA2
C-----------------------------------------------------------------------
      IF (NCHAR.LE.0) GO TO 999
         NA1 = MAX (NP1, 1)
         NA2 = MAX (NP2, 1)
C                                       slow copy
         IF ((MOD(NCHAR,4).EQ.0) .AND. (MOD(NA1-1,4).EQ.0)
     *      .AND. (MOD(NA2-1,4).EQ.0)) GO TO 20
            DO 10 I = 1,NCHAR
               I1 = (NA1-1) / 4 + 1
               I2 = (NA2-1) / 4 + 1
               J1 = MOD (NA1-1, 4) + 1
               J2 = MOD (NA2-1, 4) + 1
               CALL ZGETCH (J, STR1(I1), J1)
               CALL ZPUTCH (J, STR2(I2), J2)
               NA1 = NA1 + 1
               NA2 = NA2 + 1
 10            CONTINUE
            GO TO 999
C                                       Fast copy
 20      CONTINUE
            J1 = NCHAR / 4
            I1 = (NA1-1) / 4
            I2 = (NA2-1) / 4
            DO 30 I = 1,J1
               STR2(I2+I) = STR1(I1+I)
 30            CONTINUE
C
 999  RETURN
      END

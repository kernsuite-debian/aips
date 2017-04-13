      SUBROUTINE CHLTOU (N, STRING)
C-----------------------------------------------------------------------
C! converts a CHARACTER string to all upper case letters
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
C   CHLTOU converts any lower case characters in a CHARACTER string to
C   upper case.
C   Inputs: N       I       Number of characters
C   In/out: STRING  C*(*)   String to be converted.
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
      INTEGER   N
C
      INTEGER   I, XT, JT, LT, IC, BITS(15), KT, NP
      CHARACTER IT*1, ALPH*26
      INCLUDE 'INCS:DDCH.INC'
      DATA ALPH /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
      JT = NBITWD / 8
      XT = NBITWD - (JT-1) * 8
      DO 10 I = 1,N
         IT = STRING(I:I)
C                                       convert to ASCII
         CALL ZCLC8 (1, IT, JT, LT)
         NP = NBITWD / 32
         CALL ZI32IL (1, NP, LT, KT)
         CALL ZGTBIT (XT, KT, BITS)
         IC = 0
         CALL ZPTBIT (8, IC, BITS(XT-7))
C                                       raise to upper case
         IF ((IC.LE.96) .OR. (IC.GE.123)) GO TO 10
            IC = IC - 96
            STRING(I:I) = ALPH(IC:IC)
 10      CONTINUE
C
 999  RETURN
      END

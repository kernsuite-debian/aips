      SUBROUTINE GETKEY (SYMBOL, KL, NPNT, NUMKEY, KEYWRD, KEYVAL,
     *   KEYCHR, KEYTYP, IERR)
C-----------------------------------------------------------------------
C! Parses symbol = value from a character string.
C# Parsing
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2000, 2007
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
C   Given a character string of the form: "SYMBOL = some_value",
C   determine the type of some_value, its value, and put all of this
C   stuff in KEYVAL.
C   Inputs:
C      SYMBOL   C*8      Characters from KL. "SYMBOL = ..."
C      KL       C*(*)    Input line of the form "SYMBOL = some_value"
C   In/Out:
C      NPNT     I        The character position after the "="
C      NUMKEY   I        The number of keywords already in KEYVAL etc.
C      KEYWRD   C(*)*8   Keywords
C      KEYVAL   D(*)     List of arbitrary keyword numeric values:
C                           KEYVAL(n) => Value in D.
C      KEYCHR   C(*)*8   List of arbitrary keyword character values:
C                           KEYCHR(n) => string if KEYTYP(n)=3.
C      KEYTYP   I(*)     Type code: 1=>Double, 2=>single, 3=>char.
C                           4=>long integer, 5=>logical
C      IERR     I        Error code. 0=ok.
C-----------------------------------------------------------------------
      CHARACTER SYMBOL*8, KL*(*), KEYWRD(*)*8, KEYCHR(*)*8
      INTEGER   NPNT, NUMKEY, KEYTYP(*), IERR
      DOUBLE PRECISION KEYVAL(*)
C
      CHARACTER CHR*32, TIC*1, DOT*1, EE*2, DD*2, T*1, F*1, FSTCHR*1
      INTEGER   INB, NCHAR, TYPCOD, INDEX
      DOUBLE PRECISION X
      INCLUDE 'INCS:DDCH.INC'
      DATA TIC,  DOT,  EE,  DD,  T,   F
     *   / '''', '.', 'Ee','Dd','T', 'F'/
C-----------------------------------------------------------------------
      NUMKEY = NUMKEY + 1
      X = 0.0D0
      CHR = ' '
C                                       Guess data type
C                                       Find 1st character.
      DO 20 INB = NPNT,80
         IF (KL(INB:INB).NE.' ') GO TO 30
 20      CONTINUE
C                                       First non blank found.
C                                       Can tell a lot from 1st char.
 30   FSTCHR = KL(INB:INB)
C                                       Character.
      IF (FSTCHR.EQ.TIC) THEN
         TYPCOD = 3
         CALL GETSTR (KL, 80, 32, NPNT, CHR, NCHAR)
         IF (INDEX (SYMBOL,'DATE').NE.0) CALL DATFST ('F2L', CHR)
C                                       Logical?
      ELSE IF (FSTCHR.EQ.T) THEN
         X = 1.0D0
         TYPCOD = 5
      ELSE IF (FSTCHR.EQ.F) THEN
         X = -1.0D0
         TYPCOD = 5
C                                       Must be a number.
      ELSE
         NPNT = INB
         CALL GETNUM (KL, 80, NPNT, X)
         IF (X.EQ.DBLANK) THEN
            NUMKEY = NUMKEY - 1
            IERR = 1
            GO TO 999
            END IF
C                                       Determine: Real, Double, Integer
         TYPCOD = 4
C                                       Look for ".", "E", "D", "e" or
C                                       "d"
         IF (INDEX (KL(INB:NPNT), DOT).GT.0) TYPCOD = 2
         IF (INDEX (KL(INB:NPNT), EE(1:1)).GT.0) TYPCOD = 2
         IF (INDEX (KL(INB:NPNT), EE(2:2)).GT.0) TYPCOD = 2
         IF (INDEX (KL(INB:NPNT), DD(1:1)).GT.0) TYPCOD = 1
         IF (INDEX (KL(INB:NPNT), DD(2:2)).GT.0) TYPCOD = 1
         END IF
C                                       Save type, put values in KEYVAL
      KEYWRD(NUMKEY) = SYMBOL
      KEYVAL(NUMKEY) = X
      KEYCHR(NUMKEY) = CHR
      KEYTYP(NUMKEY) = TYPCOD
C
 999  RETURN
      END

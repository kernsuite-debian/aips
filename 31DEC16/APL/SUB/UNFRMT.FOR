      SUBROUTINE UNFRMT (STRING, SPECHR, EXTCHR, INX)
C-----------------------------------------------------------------------
C! removes excess blanks from formatted strings
C# Character Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 2008
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
C   When formatting a string, it is usually necessary to specify large I
C   formats because (for example) channel numbers can be in the 1000's.
C   But they usually are not so all the blanks look dumb.  UNFRMT like
C   REFRMT compresses the string replacing consecutive blanks with a
C   single blank.  To allow for consecutive blanks to appear in the
C   output in selected places, a special character may be put in the
C   input string to be replaced by blank but not counted as blank.
C   UNFRMT also allows another special character to appear in a string
C   that causes no blanks to appear in that space.  This could be used
C   for say a channel range: "1-85" rather than "1- 85".
C   Input/output:
C      STRING   C*(*)   String to be modified.
C   Input:
C      SPECHR   C*1     Special character, changed to blank in the output
C                          string.   ' ' => none
C      EXTCHR   C*1     Character => that no blanks should follow
C                          no character added to output for this input
c                          character. If ' ', this option is ignored.
C   Output:
C      INX      I       Last non-blank character in STRING
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), SPECHR*1, EXTCHR*1
      INTEGER   INX
C
      INTEGER   IP, OP, L, I, JTRIM
      LOGICAL   ISPEC
C-----------------------------------------------------------------------
      ISPEC = SPECHR.NE.' '
      OP = 0
      L = JTRIM (STRING)
      I = 0
      INX = 0
      DO 20 IP = 1,L
         IF (STRING(IP:IP).EQ.' ') THEN
            IF (I.EQ.0) THEN
               OP = OP + 1
               STRING(OP:OP) = ' '
               I = 1
               END IF
         ELSE IF (STRING(IP:IP).EQ.EXTCHR) THEN
            I = 1
         ELSE
            I = 0
            OP = OP + 1
            IF ((ISPEC) .AND. (STRING(IP:IP).EQ.SPECHR)) THEN
               STRING(OP:OP) = ' '
            ELSE
               IF (OP.LT.IP) STRING(OP:OP) = STRING(IP:IP)
               END IF
            END IF
         IF (STRING(OP:OP).NE.' ') INX = OP
 20      CONTINUE
      IF (INX.LT.L) STRING(INX+1:) = ' '
C
 999  RETURN
      END

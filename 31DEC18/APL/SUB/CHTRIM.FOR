      SUBROUTINE CHTRIM (INSTR, INLEN, OUTSTR, OUTLEN)
C-----------------------------------------------------------------------
C! removes leading and trailing blanks, returns actual length of string
C# Character
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   CHTRIM will trim leading and trailing blanks from an input string.
C   The output buffer can be the same as the input buffer.  If OUTSTR
C   is not long enough to hold the trimmed string then it will be
C   truncated.
C   Inputs:
C      INSTR    C*(*)   input string.
C      INLEN    I       length of string in characters
C   Outputs:
C      OUTSTR   C*(*)   buffer to hold resultant string.
C                       It can be the same as INSTR.
C      OUTLEN   I       number of characters transfered to OUTSTR,
C-----------------------------------------------------------------------
      CHARACTER INSTR*(*), OUTSTR*(*)
      INTEGER   INLEN, OUTLEN
C
      INTEGER   I, IS, IEND, JTRIM, OUTLIM
C-----------------------------------------------------------------------
      OUTLEN = 0
      OUTLIM = LEN (OUTSTR)
C                                       find last real character
C                                       removing special characters
      IEND = 0
      IF (INLEN.GT.0) IEND = JTRIM (INSTR)
C                                       Transfer all but leading &
C                                       trailing blanks.
      IF (IEND.GT.0) THEN
C                                       find first non-blank
         IS = 0
         DO 10 I = 1,IEND
            IS = I
            IF (INSTR(I:I).NE.' ') GO TO 15
 10         CONTINUE
C                                       transfer
 15      DO 20 I = IS,IEND
            IF (OUTLEN .LT. OUTLIM) THEN
               OUTLEN = OUTLEN + 1
               OUTSTR(OUTLEN:OUTLEN) = INSTR(I:I)
               END IF
 20         CONTINUE
         END IF
C                                       blank fill
      IF (OUTLEN .LT. OUTLIM) THEN
         OUTSTR(OUTLEN+1:OUTLIM) = ' '
         END IF
C
 999  RETURN
      END

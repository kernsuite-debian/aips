      SUBROUTINE GETLG (KB, LIMIT, KBP, IL)
C-----------------------------------------------------------------------
C! returns value of logical variable from character buffer
C# Parsing FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2010
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
C   GETLG obtains the value of a logical variable from buffer.
C   Inputs:
C      KB       C*80  Buffer of card image
C      LIMIT    I     Number of words in buffer
C      KBP      I     Pointer position at start
C   Outputs:
C      KBP      I     Pointer position of next field
C      IL       I     Value of logical field
C                     0--> .false.
C                     1--> .true.
C                     2--> invalid
C-----------------------------------------------------------------------
      CHARACTER KB*80
      INTEGER   LIMIT, KBP, IL
C
      CHARACTER LOGWRD(8)*8, RWORD*8
      INTEGER   I, LP, NKAR
      DATA LOGWRD / 'T', 'F', '.TRUE.', '.FALSE.','TRUE', 'FALSE',
     *   '1', '-1'/
C-----------------------------------------------------------------------
      NKAR = 0
      IL = 2
C                                       Skip leading blanks
 10   IF (KBP.GT.LIMIT) GO TO 999
C                                       allow quoted string but
C                                       value must follow directly
         IF (KB(KBP:KBP).EQ.'''') THEN
            KBP = KBP + 1
            GO TO 20
            END IF
         IF (KB(KBP:KBP).NE.' ') GO TO 20
            KBP = KBP + 1
            GO TO 10
C                                       Store this word
 20   LP = KBP
 30   NKAR = NKAR + 1
         KBP = KBP + 1
         IF (KBP.GT.LIMIT) GO TO 999
            IF (KB(KBP:KBP).NE.' ') GO TO 30
C                                       We have hit a space
C                                       Check word and convert
      IF (NKAR.LE.7) THEN
C                                       Compare with legal words
         RWORD = KB(LP:LP+NKAR-1)
         DO 40 I = 1,8
            IF (RWORD.EQ.LOGWRD(I)) IL = MOD (I, 2)
 40         CONTINUE
         END IF
C
 999  RETURN
      END

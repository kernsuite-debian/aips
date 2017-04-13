      SUBROUTINE GETSSY (LBUFF, NPNT, SYM, IERR)
C-----------------------------------------------------------------------
C! returns next symbol in character-form card image
C# Parsing FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 2007
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
C   GETSYM scrutinizes a card image to look for the next symbol.  A
C   symbol begins with a letter - or a single numeric digit followed by
C   a letter and contains up to eight alpha-numeric letter characters
C  (A-Z,0-9,_).  This routine differs from GETSYM since it can handle
C   table keywords of form iKEYWnnV.
C   Inputs:
C      LBUFF  C*80  Loose packed card image
C      NPNT   I     Pointer to first character
C   Output:
C      NPNT   I     Pointer value after getting symbol
C      SYM    C*8   Symbol, padded with blanks
C      IERR   I     Return code
C                      0--> Found legal symbol followed by '='
C                      1--> Ran off the end of the card
C                      2--> Symbol had >8 characters
C                      3--> Found legal symbol with no '='
C                           or SYM is HISTORY or COMMENT
C                      4--> Found a '/' symbol
C                      5--> Symbol contains an illegar char
C-----------------------------------------------------------------------
      CHARACTER LBUFF*80, SYM*8
      INTEGER   NPNT, IERR
C
      CHARACTER LCH*1, TSYM*8, NCH*1
      INTEGER   LPNT, LIMIT, IC, NKAR
      DATA LIMIT /80/
C-----------------------------------------------------------------------
C                                      Initialization
      NKAR = 0
      IERR = 0
      SYM = ' '
C                                       Check card limit
 10   IF (NPNT.GT.LIMIT) GO TO 980
C                                       Skip leading blanks
         LCH = LBUFF(NPNT:NPNT)
         IF (LCH.NE.' ') GO TO 20
            NPNT = NPNT + 1
            GO TO 10
C                                       Is the first character legal?
 20   IC = ICHAR(LCH)
      IF ((IC.GE.ICHAR('A')) .AND. (IC.LE.ICHAR('Z'))) GO TO 40
         IF (NPNT+1.LE.LIMIT) THEN
            NCH = LBUFF(NPNT+1:NPNT+1)
            IF ((LCH.GE.'0') .AND. (LCH.LE.'9') .AND. (NCH.GE.'A') .AND.
     *         (NCH.LE.'Z')) GO TO 40
            END IF

C                                       If we find a '/', good bye
         IF (LCH.EQ.'/') GO TO 940
C                                       Not legal.  Skip to next blank
 30      NPNT = NPNT + 1
         IF (NPNT.GT.LIMIT) GO TO 980
            LCH = LBUFF(NPNT:NPNT)
            IF (LCH.NE.' ') GO TO 30
C                                       Found another blank. begin
C                                       searching again
         NPNT = NPNT + 1
         GO TO 10
C                                       Find rest of symbol
 40   LPNT = NPNT
 50   NKAR = NKAR + 1
      IF (NKAR.LE.8) TSYM(NKAR:NKAR) = LCH
         NPNT = NPNT + 1
         IF (NPNT.GT.LIMIT) GO TO 980
         LCH = LBUFF(NPNT:NPNT)
         IC = ICHAR(LCH)
C                                       Is the character legal?
         IF ((IC.GE.ICHAR('A')) .AND. (IC.LE.ICHAR('Z'))) GO TO 50
         IF ((IC.GE.ICHAR('0')) .AND. (IC.LE.ICHAR('9'))) GO TO 50
         IF (LCH.EQ.'_') GO TO 50
         IF (LCH.EQ.'-') GO TO 50
C                                       Have we hit a ' ' or
C                                          a '='?
      IF (LCH.EQ.'=') GO TO 70
      IF (LCH.EQ.' ') GO TO 65
C                                       Illegal char.  Look for
C                                          the next space.
 62   NPNT = NPNT + 1
      IF (NPNT.GT.LIMIT) GO TO 980
      IF (LBUFF(NPNT:NPNT).NE.' ') GO TO 62
      GO TO 930
C                                       Look for an '='
C                                         or an end of card
 65   NPNT = NPNT + 1
      IF (NPNT.GT.LIMIT) GO TO 67
      IF (LBUFF(NPNT:NPNT).EQ.' ') GO TO 65
      IF (LBUFF(NPNT:NPNT).EQ.'=') GO TO 70
C                                       Legal symbol with no '='
 67   IERR = 3
      NPNT = NPNT - 2
C                                      Legal symbol with an '='
 70   NPNT = NPNT + 1
      IF (NKAR.GT.8) GO TO 970
C                                       Pack symbol
      SYM = TSYM(1:NKAR)
      IF (IERR.NE.0) GO TO 999
C                                       Check for HISTORY =, etc.
      IERR = 3
      IF (SYM.EQ.'HISTORY ') GO TO 999
      IF (SYM.EQ.'COMMENT ') GO TO 999
      IERR = 0
      GO TO 999
C
C-----------------------------------------------------------------------
C
C                                       Symbol contains illegal char
 930  IERR = 5
      GO TO 999
C                                       Found a '/' symbol
 940  IERR = 4
      NPNT = NPNT + 1
      GO TO 999
C                                       Symbol has more than 8 char
 970  IERR = 2
      GO TO 999
C                                       Ran out of card
 980  IERR = 1
C
 999  RETURN
      END

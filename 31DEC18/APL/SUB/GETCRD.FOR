      SUBROUTINE GETCRD (ICARD, NOSYM, STRSYM, SYMTAB, FITBLK, NPNT,
     *   KL, SYMBOL, TABNO, ISHIST, END, IERR)
C-----------------------------------------------------------------------
C! parses card image from FITS header, returns recognized keyword
C# FITS Parsing
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2001
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
C   GETCRD (get card) will unpack a given card image from a header block
C   of FITS data, look for a recognizable key word from a supplied table
C   and return information to the calling routine.
C   Looks for keywords after 'HISTORY AIPS', 'HISTORY VLACV',
C  'HISTORY RANCID', or 'HISTORY VLA'
C   Inputs:
C      ICARD   I            the card image (1-36) in FITS data block.
C      NOSYM   I            the number of entries in key word table.
C      STRSYM  I            Start search with symbol # STRSYM
C      SYMTAB  C(NOSYM)*8   keywords
C      FITBLK  C*2880       the block of FITS header cards.
C   In/out:
C      NPNT    I            The position to start scan in array KL.
C                           Returns the last position scanned plus one.
C      KL      C*80         input the unpacked card image if NPNT > 1,
C                           else returns the unpacked card image.
C   Outputs:
C      SYMBOL  C*8          the symbol found on the card.
C      TABNO   I            SYMBOL matches SYMTAB(TABNO).
C      ISHIST  L            True if history card else false.
C      END     L            True if end card found, else false.
C      IERR    I            0=match found, 1=no match on otherwise
C                           valid keyword, 2=card ends or other trouble
C                           -1 => special AIPS header keyword card -
C                           feed it to PUTCRD to put it in the catalog
C                           header file or ignore it.
C-----------------------------------------------------------------------
      INTEGER   ICARD, NOSYM, STRSYM, TABNO, NPNT, IERR
      CHARACTER SYMTAB(*)*8, FITBLK*2880, KL*80, SYMBOL*8
      LOGICAL   END, ISHIST
C
      INTEGER   ITYP, I, INC, JTRIM, LSTRS
      LOGICAL   T, F
      CHARACTER AIPS(4)*8
      DATA T, F /.TRUE.,.FALSE./
      DATA AIPS /'AIPS    ','VLACV   ','RANCID  ','VLA     '/
C-----------------------------------------------------------------------
      IERR = 0
      END = F
      ISHIST = F
      LSTRS = STRSYM
      IF (LSTRS.LT.1) LSTRS = 1
C                                       Unpack new card.
      IF (NPNT.LE.1) THEN
         INC = (ICARD - 1) * 80 + 1
         KL = FITBLK(INC:INC+79)
         I = JTRIM (KL)
         IF (I.GT.19) THEN
            IF (KL(:19).EQ.'HISTORY AIPS HEADER') THEN
               IERR = -1
               GO TO 999
               END IF
            END IF
         END IF
C                                       Get next symbol on card
 10   CALL GETSYM (KL, NPNT, SYMBOL, ITYP)
C                                       Test for END card.
      IF (SYMBOL.NE.'END') GO TO 20
         END = T
         GO TO 999
C                                       No KEYWORD= format.
 20   IF (ITYP.EQ.0) GO TO 100
         IF (ITYP.NE.3) GO TO 990
C                                       Check for AIPS, RANCID, or
C                                       VLACV history
         IF (SYMBOL.NE.'HISTORY') GO TO 980
         CALL GETSYM (KL, NPNT, SYMBOL, ITYP)
         IF (ITYP.EQ.0) GO TO 980
         IF (ITYP.NE.3) GO TO 990
         DO 30 I = 1,4
            IF (SYMBOL.EQ.AIPS(I)) GO TO 40
 30         CONTINUE
         GO TO 980
C                                       Parse our kind of history for
C                                       KEYWORD=
 40      ISHIST = T
         CALL GETSYM (KL, NPNT, SYMBOL, ITYP)
C                                       NO =. Check for SORT ORDER.
         IF (ITYP.EQ.0) GO TO 100
            IF (ITYP.NE.3) GO TO 990
C                                       We have ANT record.
            IF (SYMBOL.EQ.'ANT') GO TO 980
            IF (SYMBOL.NE.'SORT') GO TO 40
            CALL GETSYM (KL, NPNT, SYMBOL, ITYP)
            IF (ITYP.EQ.3) GO TO 40
            IF (ITYP.NE.0) GO TO 990
C                                       SORT ORDER = found. Test.
            IF (SYMBOL.EQ.'ORDER') SYMBOL = 'SORTORDR'
C                                       Check KEYWORD= for recognizable
C                                       symbol.
 100  DO 110 TABNO = LSTRS,NOSYM
         IF (SYMBOL.EQ.SYMTAB(TABNO)) GO TO 999
 110     CONTINUE
C                                       No recognized keyword.
 980  IERR = 1
      GO TO 999
C                                       Other problem
 990  IERR = 2
C
 999  RETURN
      END

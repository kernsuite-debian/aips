      SUBROUTINE IDWCRD (CARD, NOSYM, NLIMIT, SYMTAB, NPNT,
     *   SYMBOL, TABNO, NFOUND, IERR)
C-----------------------------------------------------------------------
C! returns allowed keyword from FITS header card image
C# Parsing FITS
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
C-----------------------------------------------------------------------
C   IDWCRD (identify wild card) search table SYMTAB for a recognizable
C   key word in the form KEYWORDn where 'n' is an integer between 1 and
C   NLIMIT(i) where i corresponds to the position of the keyword in
C   SYMTAB. If NLIMIT(i)=0 KEYWORD must match the SYMTAB entry exactly.
C   Inputs:
C      CARD    C*80         Input card.
C      NOSYM   I            the number of entries in key word table.
C      NLIMIT  I(NOSYM)     Upper limit on 'n'. 0 means KEYWORD must
C                           match symbol table value exactly.
C      SYMTAB  C(NOSYM)*8   Keywords.
C   Outputs:
C      NPNT    I            Pointer in CARD after "="
C      SYMBOL  C*8          Symbol before "=" (unpacked)
C      TABNO   I            KEYWORD matches SYMTAB(TABNO).
C      NFOUND  I            Value of 'n' for KEYWORDn.
C      IERR    I            0=match found, 1=no match on otherwise
C                           valid keyword, 2=card ends or other trouble
C-----------------------------------------------------------------------
      INTEGER   NOSYM, NLIMIT(*), NPNT, TABNO, NFOUND, IERR
      CHARACTER SYMTAB(*)*8, CARD*80, SYMBOL*8
C
      CHARACTER TABSYM*8
      INTEGER   ITYP, ILEN, ILEN2
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get next symbol on card
 10   CALL GETSYM (CARD, NPNT, SYMBOL, ITYP)
C                                       Test for END card.
      IF (SYMBOL.EQ.'END') GO TO 999
C                                       No KEYWORD= format.
 20   IF (ITYP.NE.0) GO TO 980
C                                       Check KEYWORD= for recognizable
C                                       symbol.
 100  DO 110 TABNO = 1,NOSYM
         TABSYM = SYMTAB(TABNO)
         ILEN = 8
         IF (NLIMIT(TABNO).NE.0) CALL CHTRIM (TABSYM, 8, TABSYM, ILEN)
C                                       Character parts are equal.
         IF (SYMBOL(1:ILEN).EQ.TABSYM(1:ILEN)) THEN
            NFOUND = 0
            ILEN = ILEN + 1
            IF (ILEN.GT.8) GO TO 999
            TABSYM = SYMBOL
            CALL CHTRIM (TABSYM, 8, TABSYM, ILEN2)
            CALL CH2NUM (ILEN, ILEN2, TABSYM, NFOUND, IERR)
            IF (IERR.NE.0) GO TO 110
            IF (NFOUND.LT.NLIMIT(TABNO)) GO TO 999
            END IF
 110     CONTINUE
C                                       Keyword but not in list
      IERR = 1
      GO TO 999
C                                       No recognized keyword.
 980  IERR = 2
      GO TO 999
C
 999  RETURN
      END

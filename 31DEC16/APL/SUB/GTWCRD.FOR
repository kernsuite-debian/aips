      SUBROUTINE GTWCRD (ICARD, NOSYM, NLIMIT, SYMTAB, FITBLK, NPNT,
     *   CARD, SYMBOL, TABNO, NFOUND, IFOUND, CVER, END, IERR)
C-----------------------------------------------------------------------
C! returns allowed keyword from FITS header card image
C# Parsing FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2007
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
C   GTWCRD (get wild card) will unpack a given card image from a header
C   block of FITS data, search table SYMTAB for a recognizable key word
C   in the form KEYWORDn where 'n' is an integer between 1 and
C   NLIMIT(i) where i corresponds to the position of the keyword in
C   SYMTAB. If NLIMIT(i)=0 KEYWORD must match the SYMTAB entry exactly.
C   Inputs:
C      ICARD   I            the card image (1-36) in FITS data block.
C      NOSYM   I            the number of entries in key word table.
C      NLIMIT  I(NOSYM)     Upper limit on 'n'. 0 means KEYWORD must
C                           match symbol table value exactly.
C                           < 0 => must match char 2:N, return IFOUND
C      SYMTAB  C(NOSYM)*8   Keywords.
C      FITBLK  C*2880       the block of FITS header cards.
C   Outputs:
C      NPNT    I            Pointer in CARD after "="
C      CARD    C*80         Packed card.
C      SYMBOL  C*8          Symbol before "=" (unpacked)
C      TABNO   I            KEYWORD matches SYMTAB(TABNO).
C      NFOUND  I            Value of 'n' for KEYWORDn is the column
C                              number of the parameter
C      IFOUND  I            Value of 'i' for iKEYWnn is the axis number
C                              for array in cell
C      CVER    C*1          Coordinate version code: 'C' in iKEYWnnC
C      END     L            True if end card found, else false.
C      IERR    I            0=match found, 1=no match on otherwise
C                           valid keyword, 2=card ends or other trouble
C-----------------------------------------------------------------------
      INTEGER   ICARD, NOSYM, NLIMIT(*), NPNT, TABNO, NFOUND, IFOUND,
     *   IERR
      LOGICAL   END
      CHARACTER SYMTAB(*)*8, FITBLK*2880, CARD*80, SYMBOL*8, CVER*1
C
      CHARACTER TABSYM*8
      INTEGER   ITYP, ILEN, ILEN2, INC, JTRIM, IP, KBP
C-----------------------------------------------------------------------
      IERR = 0
      END = .FALSE.
      CVER = ' '
      NFOUND = 0
      IFOUND = 0
C                                       Unpack new card.
      INC = (ICARD - 1) * 80 + 1
      CARD = FITBLK(INC:INC+79)
      ILEN = JTRIM (CARD)
C                                       Get next symbol on card
      NPNT = 1
      CALL GETSSY (CARD, NPNT, SYMBOL, ITYP)
C                                       Test for END card.
      IF (SYMBOL.EQ.'END') THEN
         END = .TRUE.
C                                       No KEYWORD= format.
      ELSE IF (ITYP.NE.0) THEN
         IERR = 2
C                                       Check KEYWORD= for recognizable
C                                       symbol.
      ELSE
         DO 100 TABNO = 1,NOSYM
            TABSYM = SYMTAB(TABNO)
            ILEN = 8
            IP = 1
            IF (NLIMIT(TABNO).LT.0) IP = 2
            IF (NLIMIT(TABNO).NE.0) CALL CHTRIM (TABSYM, 8, TABSYM,
     *         ILEN)
C                                       Character parts are equal.
            IF (SYMBOL(IP:ILEN).EQ.TABSYM(IP:ILEN)) THEN
               NFOUND = 0
               IFOUND = 0
               IERR = 0
               ILEN = ILEN + 1
               IF (ILEN.LE.8) THEN
                  TABSYM = SYMBOL
                  CALL CHTRIM (TABSYM, 8, TABSYM, ILEN2)
                  IF (IP.EQ.2) THEN
                     KBP = 1
                     CALL GETINT (TABSYM, 8, KBP, IFOUND)
                     END IF
                  KBP = ILEN
                  CALL GETINT (TABSYM, 8, KBP, NFOUND)
                  IF (KBP.LE.8) CVER = TABSYM(KBP:KBP)
                  END IF
               IF (NFOUND.LE.ABS(NLIMIT(TABNO))) GO TO 999
               END IF
 100        CONTINUE
C                                       Keyword but not in list
         IERR = 1
         END IF
C
 999  RETURN
      END

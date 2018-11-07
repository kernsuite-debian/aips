      SUBROUTINE SDTCRD (ICARD, FITBLK, MAXKEY, NUMKEY, KEYWRD, KEYVAL,
     *   KEYCHR, KEYTYP, WARN, NRCX)
C-----------------------------------------------------------------------
C! Parse 'SINGLDSH' FITS table headers, get some keywords.
C# Parsing FITS SDish Tape IO-appl EXT-util
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
C   SDTCRD process a 'SINGLDSH' FITS table entry and return keywords
C   and values for selected keywords.  Receiver dependent values will
C   have the last two characters of the keyword replaced with the
C   receiver number.
C   Inputs:
C      ICARD    I        the card image (1-36) in FITS data block.
C      FITBLK   C*2880   the block of FITS header cards.
C      MAXKEY   I        maximum number of keywords allowed.
C   Input/Output:
C      NUMKEY   I        The number of keywords already in KEYVAL.
C      KEYWRD   C(*)*8   Keywords
C      KEYVAL   D(*)     List of arbitrary numeric, keyword values:
C                           KEYVAL(n) => Value in D.
C      KEYCHR   C(*)*8   List of character keyword values.
C      KEYTYP   I(*)     Type code: 1=>Double, 2=>single, 3=>char.
C                           4=>integer, 5=>logical
C      WARN     L        True if user needs to be warned about
C                           exceeding MAXKEY.
C      NRCX     I        Current receiver number.
C-----------------------------------------------------------------------
      CHARACTER FITBLK*(*), KEYWRD(*)*8, KEYCHR(*)*8
      INTEGER   ICARD, MAXKEY, NUMKEY, KEYTYP(*), NRCX
      LOGICAL   WARN
      DOUBLE PRECISION KEYVAL(*)
C
      CHARACTER SYMBOL*8, SNGDSH*8, SDWORD(34)*8, CARD*80, ITEMP*2
      INTEGER   NSD, NSDCOM, TABNO, IERR, INC, ITYP, I, NPNT, LIM1, IP
      INCLUDE 'INCS:DMSG.INC'
      DATA SNGDSH /'SINGLDSH'/
C                                       Recognized single dish keywords
C                                       Receiver dependent ones follow
C                                       'RCVR' which must be no.
C                                       NSDCOM+1
      DATA NSD, NSDCOM /34,26/
      DATA SDWORD      /'SCAN    ', 'UTDATE  ', 'HOURUT  ',
     *   'CYCLLEN ', 'EPOCH   ', 'TELESCOP', 'OBSERVER',
     *   'OBJECT  ', 'SAMPRAT ', 'EPOCRA  ', 'EPOCDEC ',
     *   'ORIENT  ', 'EXPT1   ', 'EXPT2   ', 'EXPT3   ',
     *   'EXPT4   ', 'BFWHM   ', 'OFFSCAN ', 'VELOCTY ',
     *   'VELDEF  ', 'FRONTEND', 'PTCON1  ', 'PTCON2  ',
     *   'PTCON3  ', 'INDX    ', 'INDY    ',
     *   'RCVR    ', 'OBSFREQ ', 'CALVOLTS', 'TCAL    ',
     *   'RHO     ', 'THETA   ', 'POLARIZ ', 'BW      '/
C-----------------------------------------------------------------------
      INC = (ICARD - 1) * 80 + 1
C                                       Verify Single dish
      IF (FITBLK(INC:INC+7).NE.SNGDSH) GO TO 999
C                                       get new card.
      CARD = FITBLK(INC:INC+79)
C                                       Get keyword (3rd item)
      NPNT = 19
      CALL GETSYM (CARD, NPNT, SYMBOL, ITYP)
C                                       Make sure = is there
      IF (ITYP.NE.0) GO TO 999
C                                       Search for recognized keyword
C                                       Shorter search if in rcx
C                                       dependent.
      LIM1 = 1
      IF (NRCX.GT.1) LIM1 = NSDCOM+1
      DO 100 I = LIM1,NSD
         TABNO = I
         IF (SYMBOL.EQ.SDWORD(I)) GO TO 110
 100     CONTINUE
C                                       No match
      GO TO 999
C                                       Match - Check keyword limit
 110  IF (NUMKEY.LT.MAXKEY) THEN
         CALL GETKEY (SYMBOL, CARD, NPNT, NUMKEY, KEYWRD, KEYVAL,
     *      KEYCHR, KEYTYP, IERR)
C                                       Check valid type - ignore
C                                       invalid ones.
         IF ((KEYTYP(NUMKEY).LE.0) .OR. (KEYTYP(NUMKEY).GT.5)) THEN
            WRITE (MSGTXT,1001) SYMBOL
            CALL MSGWRT (6)
            NUMKEY = NUMKEY - 1
            GO TO 999
            END IF
C                                       Blew limit
      ELSE
         IF (WARN) THEN
            WRITE (MSGTXT,1002) MAXKEY
            CALL MSGWRT (6)
            WARN = .FALSE.
            END IF
         GO TO 999
         END IF
      IF (TABNO.LE.NSDCOM) GO TO 900
C                                       Handle receiver dependent
C                                       keywords
      IF (TABNO.EQ.(NSDCOM+1)) THEN
C                                       Receiver number - save
         NRCX = KEYVAL(NUMKEY) + 0.1
         IF (NRCX.GT.99) NRCX = 99
C                                       Drop as keyword
         NUMKEY = NUMKEY - 1
         GO TO 999
         END IF
C                                       Add rcx no. to keyword.
      WRITE (ITEMP,1100) NRCX
C                                       Trim off blanks
      CALL CHTRIM (KEYWRD(NUMKEY), 8, KEYWRD(NUMKEY), IP)
      IP = IP + 1
      IF (IP.GT.7) IP = 7
      KEYWRD(NUMKEY)(IP:IP+1) = ITEMP(1:2)
      GO TO 999
C                                       Tell scan number
 900  IF (TABNO.EQ.1) THEN
         WRITE (MSGTXT,1900) KEYVAL(NUMKEY)
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('SDTCRD: KEYWORD ',A8,' OF INDETERMINATE DATA TYPE')
 1002 FORMAT ('SDTCRD: BLEW NO. OF KEYWORD LIMIT OF ',I5,' IGNORE REST')
 1100 FORMAT (I2.2)
 1900 FORMAT ('Scan number =',F10.0)
      END

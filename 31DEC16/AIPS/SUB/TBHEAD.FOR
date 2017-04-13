      SUBROUTINE TBHEAD (OPER, IVOL, ISLOT, IERR)
C-----------------------------------------------------------------------
C! verbs to put/get table header values
C# POPS-appl Ext-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2006
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
C   This routine reads and writes values from/to an AIPS table header.
C   Keywords with names passed in KEYWRD are read or written.  The are
C   two special cases:
C      If KEYWORD='NUM ROW' then the number of rows in the table are
C   returned in KEYVAL.
C      If KEYWRD='FIND COL' then the column with the label given in
C   KEYSTR will be located and it's column number returned in KEYVAL.
C   The search will be only for the number of characters specified.
C   Inputs:
C      OPER    C*4  'READ' or 'WRIT' operation desired.
C      IVOL    I    Disk number
C      ISLOT   I    Catalog slot number
C   Common: (though adverbs in common)
C      INEXT     H*2  Table type (e.g. AN, SU)
C      INVER     R    the version number of the table.
C      KEYWORD  H*8   FITS keyword corresponding to a header value
C      KEYVALUE R(2)  Numeric value out/in
C      KEYSTRNG H*16  String value out/in
C   Output:
C      IERR     I     Error code, 0=> OK else failed
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   IVOL, ISLOT, IERR
C
      CHARACTER PRGNAM*8, THEWRD*8, STVALU*16, TABTYP*2, KEYS*24,
     *   HILINE*72, HILIN2*72, NEWSTR*8, OLDSTR*8, CDUM, DATYPE*4
      INTEGER   POTERR, IROUND, VER, IBUFF2(512), TLUN, IKEY, NREC,
     *   NCOL, DATP(128,2), NKEY, KEYTYP, KEYLOC, KEYVI, LKEY,
     *   KOL, I, JERR, ITRIM, IERH, IHLUN, OLDVI, IDUM
      HOLLERITH KEYVH(2), OLDVH(2)
      REAL     KEYVR, OLDVR, RDUM(2), DOHI
      DOUBLE PRECISION D, TEMPD, KEYVD, OLDVD
      LOGICAL   KEYVL, OLDVL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHIS.INC'
      EQUIVALENCE (KEYVI, KEYVR, KEYVH, KEYVD, KEYVL)
      EQUIVALENCE (OLDVI, OLDVR, OLDVH, OLDVD, OLDVL)
      DATA TLUN, IHLUN /28,29/
      DATA PRGNAM /'TBHEAD'/
C-----------------------------------------------------------------------
      POTERR = 0
C                                       Read CATBLK
      CALL CATIO ('READ', IVOL, ISLOT, CATBLK, 'REST', IBUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         IF (IERR.EQ.5) MSGTXT = 'FILE IS BUSY'
         POTERR = 33
         GO TO 970
         END IF
C                                       Open table
      IKEY = 1
      NREC = 10
      NCOL = 0
      CALL ADVERB ('INEXT', 'C', 1, 2, IDUM, RDUM, TABTYP)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INVERS', 'I', 1, 0, VER, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       Find highest if no version given
      IF (VER.LE.0) CALL FNDEXT (TABTYP, CATBLK, VER)
C                                       Open table
      CALL TABINI (OPER, TABTYP, IVOL, ISLOT, VER, CATBLK, TLUN,
     *   IKEY, NREC, NCOL, DATP, IBUFF2, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1100) IERR, OPER, TABTYP, VER
         POTERR = 61
         GO TO 970
         END IF
      NKEY = 1
C                                       Decode KEYWORD
      CALL ADVERB ('KEYWORD', 'C', 1, 8, IDUM, RDUM, THEWRD)
      IF (ERRNUM.NE.0) GO TO 980
C                                       Trap special cases here
C                                       Number of rows in the table
      IF (THEWRD.EQ.'NUM ROW') THEN
         RDUM(1) = IBUFF2(5)
         RDUM(2) = 0.0
         CALL ADVRBS ('KEYVALUE', 'R', 2, 0, IDUM, RDUM, CDUM)
         GO TO 800
         END IF
C                                       Find column with label in KEYSTR
      IF (THEWRD.EQ.'FIND COL') THEN
         NKEY = 1
         KEYS = '  '
         CALL ADVERB ('KEYSTRNG', 'C', 1, 16, IDUM, RDUM, KEYS)
         IF (ERRNUM.NE.0) GO TO 800
         LKEY = ITRIM (KEYS)
         CALL FNDCOL (NKEY, KEYS, LKEY, .TRUE., IBUFF2, KOL, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1200) IERR, KEYS
            CALL MSGWRT (7)
            POTERR = 61
            GO TO 800
            END IF
         RDUM(1) = KOL
         RDUM(2) = 0.0
         CALL ADVRBS ('KEYVALUE', 'R', 2, 0, IDUM, RDUM, CDUM)
         GO TO 800
         END IF
C                                       Find Keyword to get type.
      IF (OPER.EQ.'WRIT') MSGSUP = 32000
      CALL TABKEY ('READ', THEWRD, NKEY, IBUFF2, KEYLOC, OLDVI, KEYTYP,
     *   IERR)
      MSGSUP = 0
      IF (IERR.NE.0) THEN
C                                       new keyword to write
         IF ((OPER.EQ.'WRIT') .AND. (IERR.GT.20)) THEN
C                                       KEYTYPE
            CALL ADVERB ('KEYTYPE', 'C', 1, 4, IDUM, RDUM, DATYPE)
            IF (ERRNUM.NE.0) GO TO 980
            KEYTYP = 0
            IF (DATYPE(:1).EQ.'D') KEYTYP = 1
C                                       Note R*4 deprecated
            IF (DATYPE(:1).EQ.'R') KEYTYP = 1
            IF (DATYPE(:1).EQ.'C') KEYTYP = 3
            IF (DATYPE(:1).EQ.'I') KEYTYP = 4
            IF (DATYPE(:1).EQ.'L') KEYTYP = 5
            IF (KEYTYP.EQ.0) THEN
               MSGTXT = 'NEW KEYWORD INVALID TYPE = ''' // DATYPE(:1) //
     *            ''''
               CALL MSGWRT (7)
               POTERR = 61
               GO TO 800
               END IF
         ELSE
            WRITE (MSGTXT,1300) IERR, THEWRD
            CALL MSGWRT (7)
            POTERR = 61
            GO TO 800
            END IF
         END IF
C                                       Get ready to write value
      IF (OPER.EQ.'WRIT') THEN
C                                       Enter data in VALUE via
C                                       EQUIVALENCE
         CALL ADVERB ('KEYSTRNG', 'C', 1, 16, IDUM, RDUM, STVALU)
         IF (ERRNUM.NE.0) GO TO 800
         CALL ADVERB ('KEYVALUE', 'R', 2, 0, IDUM, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 800
         KEYVD = 0.0D0
         CALL POPSRD ('R2D', RDUM, TEMPD)
         IF (KEYTYP.EQ.1) KEYVD = TEMPD
         IF (KEYTYP.EQ.2) KEYVR = RDUM(1)
         IF (KEYTYP.EQ.3) CALL CHR2H (8, STVALU, 1, KEYVH)
         IF (KEYTYP.EQ.4) KEYVI = IROUND (RDUM(1))
         IF (KEYTYP.EQ.5) KEYVL = STVALU(:1).EQ.'T'
         KEYLOC = 1
         END IF
C                                       Read/update keyword.
      CALL TABKEY (OPER, THEWRD, NKEY, IBUFF2, KEYLOC, KEYVI, KEYTYP,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1500) IERR, OPER
         CALL MSGWRT (7)
         POTERR = 61
         GO TO 800
         END IF
C                                       Put value back into adverbs
      IF (OPER.EQ.'READ') THEN
         IF (KEYTYP.EQ.1) THEN
            CALL RCOPY (NWDPDP, KEYVD, D)
            CALL POPSRD ('D2R', RDUM, D)
         ELSE IF (KEYTYP.EQ.2) THEN
            RDUM(1) = KEYVR
            RDUM(2) = 0.0
         ELSE IF (KEYTYP.EQ.3) THEN
            STVALU = ' '
            CALL H2CHR (8, 1, KEYVH, STVALU)
         ELSE IF (KEYTYP.EQ.4) THEN
            RDUM(1) = KEYVI
            RDUM(2) = 0.0
         ELSE IF (KEYTYP.EQ.5) THEN
            IF (KEYVL) THEN
               STVALU = 'T'
            ELSE
               STVALU = 'F'
               END IF
            END IF
         IF ((KEYTYP.EQ.3) .OR. (KEYTYP.EQ.5)) THEN
            CALL ADVRBS ('KEYSTRNG', 'C', 1, 16, IDUM, RDUM, STVALU)
            IF (ERRNUM.NE.0) GO TO 800
         ELSE IF ((KEYTYP.EQ.1) .OR. (KEYTYP.EQ.2) .OR. (KEYTYP.EQ.4))
     *      THEN
            CALL ADVRBS ('KEYVALUE', 'R', 2, 0, IDUM, RDUM, CDUM)
            IF (ERRNUM.NE.0) GO TO 800
            END IF
         END IF
C                                       Close table
 800  CALL TABIO ('CLOS', 1, I, IBUFF2, IBUFF2, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1800) JERR, TABTYP, VER
         IF (POTERR.EQ.0) POTERR = 61
         IF (IERR.EQ.0) IERR = JERR
         GO TO 970
         END IF
C                                       Any history
      CALL ADVERB ('DOHIST', 'R', 1, 0, IDUM, DOHI, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((OPER.EQ.'WRIT') .AND. (DOHI.GT.-1.5)) THEN
C                                       Open history file
         CALL HIOPEN (IHLUN, IVOL, ISLOT, IBUFF2, IERH)
         IF (IERH.NE.0) GO TO 980
C                                       Which table?
         WRITE (HILINE,2800) TSKNAM, 'PUTTHEAD', TABTYP, VER
         CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
C                                       Add entries by type
C                                       Double precision
         IF (KEYTYP.EQ.1) THEN
            WRITE (HILINE,2801) TSKNAM, 'PUTTHEAD', THEWRD, KEYVD
            WRITE (HILIN2,3801) TSKNAM, 'PUTTHEAD', OLDVD
C                                       Real
         ELSE IF (KEYTYP.EQ.2) THEN
            WRITE (HILINE,2802) TSKNAM, 'PUTTHEAD', THEWRD, KEYVR
            WRITE (HILIN2,3802) TSKNAM, 'PUTTHEAD', OLDVR
C                                       Character
         ELSE IF (KEYTYP.EQ.3) THEN
            CALL H2CHR (8, 1, KEYVH, NEWSTR)
            CALL H2CHR (8, 1, OLDVH, OLDSTR)
            WRITE (HILINE,2803) TSKNAM, 'PUTTHEAD', THEWRD, NEWSTR
            WRITE (HILIN2,3803) TSKNAM, 'PUTTHEAD', OLDSTR
C                                       Integer
         ELSE IF (KEYTYP.EQ.4) THEN
            WRITE (HILINE,2804) TSKNAM, 'PUTTHEAD', THEWRD, KEYVI
            WRITE (HILIN2,3804) TSKNAM, 'PUTTHEAD', OLDVI
C                                       Logical
         ELSE IF (KEYTYP.EQ.5) THEN
            CALL H2CHR (1, 1, KEYVH, NEWSTR)
            CALL H2CHR (1, 1, OLDVH, OLDSTR)
            WRITE (HILINE,2805) TSKNAM, 'PUTTHEAD', THEWRD, NEWSTR(1:1)
            WRITE (HILIN2,3805) TSKNAM, 'PUTTHEAD', OLDSTR(1:1)
            END IF
C                                       Add history entries
         IF (IERH.EQ.0) CALL HIADD (IHLUN, HILINE, IBUFF2, IERH)
         IF (IERH.EQ.0) CALL HIADD (IHLUN, HILIN2, IBUFF2, IERH)
         CALL HICLOS (IHLUN, .TRUE., IBUFF2, IERH)
         END IF
      GO TO 980
C-----------------------------------------------------------------------
C                                       Print error.
 970  CALL MSGWRT (8)
C                                       AIPS error management.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CATALOG READ ERROR',I5)
 1100 FORMAT ('TABINI ERROR ',I3,' OPEN FOR ',A4,1X,A2,', TABLE ',
     *   ' VER=',I3)
 1200 FORMAT (' ERROR ',I4,' FINDING COLUMN ',A)
 1300 FORMAT (' ERROR ',I4,' FINDING HEADER KEYWORD ',A)
 1500 FORMAT ('TABKEY ERROR',I3,1X,A,'ING TABLE KEYWORD')
 1800 FORMAT ('TABIO ERROR ',I3,' CLOSING ',A2,' TABLE VER=',I3)
 2800 FORMAT (A,':',A,': INEXT=''',A,''', INVERS=',I3,' /Table edited')
 2801 FORMAT (A,':',A,': KEYWORD=''',A,''',KEYVALUE=', 1PD20.12)
 3801 FORMAT (A,':',A,': / Old value =',1PD20.12)
 2802 FORMAT (A,':',A,': KEYWORD =''',A,''',KEYVALUE(1)=',1PE15.5)
 3802 FORMAT (A,':',A,': / Old value =',1PE12.5)
 2803 FORMAT (A,':',A,': KEYWORD =''',A,''',KEYSTRNG=''',A,'''')
 3803 FORMAT (A,':',A,': / Old value =''',A,'''')
 2804 FORMAT (A,':',A,': KEYWORD =''',A,''', KEYVALUE(1)=',I10)
 3804 FORMAT (A,':',A,': / Old value = ',I10)
 2805 FORMAT (A,':',A,': KEYWORD =''',A,''', KEYSTRNG=''',A,'''')
 3805 FORMAT (A,':',A,': / Old value =''',A,'''')
      END

      SUBROUTINE TABHDK (CARD, HLUN, HBLK, MAXKEY, NUMKEY, KEYWRD,
     *   KEYVAL, KEYCHR, KEYTYP, ENDKEY, IERR)
C-----------------------------------------------------------------------
C! Reads a FITS table header.
C# EXT-util FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2016
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
C   TABHDK interprets TABle HeaDer Keywords on an input card image and
C   puts the data for recognized symbols into a set of output arrays.
C   Selected 'SINGLDSH' keywords are kept; the rest are ignored
C   Inputs:
C      CARD     C*80     FITS Card to decoded and stored
C      HLUN     I        History file LUN.
C      HBLK     I(256)   History file I/O buffer.
C      MAXKEY   I        Maximum number of keys in arrays
C   In/Out:
C      NUMKEY   I        Location to store next keyword
C   Outputs:
C      KEYWRD   C(*)*8   Keywords
C      KEYVAL   D(*)     List of arbitrary numeric/logical values:
C                           KEYVAL(n) => Value in D.
C                           logicals coded as -1.0=>F, 1.0=>T.
C      KEYCHR   C(*)*8   List of character keyword values values.
C      KEYTYP   I(*)     Keyword type codes.
C      ENDKEY   L        True when END card is found.
C      IERR     I        error code 0=ok. 1=error.
C   Commons:
C      /EXTHDR/    Extension file values.
C      /THDR/      Header values for a tables extension file.
C-----------------------------------------------------------------------
      CHARACTER KEYWRD(*)*8, KEYCHR(*)*8
      INTEGER   HLUN, HBLK(256), MAXKEY, NUMKEY, KEYTYP(*), IERR
      DOUBLE PRECISION KEYVAL(*)
      LOGICAL ENDKEY
C                                       Define number of Keywords
      INTEGER   NN
      PARAMETER (NN=14)
      CHARACTER SYMBOL*8, STR*80, TWORD(NN)*8, CARD*80, HILINE*72,
     *   HISTRY*8, COMENT*8, SNGDSH*8
      DOUBLE PRECISION X
      INTEGER   TLIMIT(NN), N, TABNO, NBYT, NPNT, NCHAR, IHIERR, KTYPE,
     *   COUNT, IST, HERR, JERR, NRCX, ICARD, ITRIM
      LOGICAL   ISHIS, WARN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
C                                       Remember to warn only once
      SAVE  WARN, IHIERR, HERR
      DATA  WARN, IHIERR, HERR /.TRUE., 0, 0/
      DATA  HISTRY,     COMENT,     SNGDSH
     *    /'HISTORY ', 'COMMENT ', 'SINGLDSH'/
C                                       Special keywords
      DATA TWORD     /'TFIELDS ',  'EXTNAME ',  'EXTVER  ',
     *   'EXTLEVEL',  'TTYPE   ',  'TBCOL   ',  'TFORM   ',
     *   'TUNIT   ',  'TSCAL   ',  'TZERO   ',  'TNULL   ',
     *   'TFDIM   ',  'AUTHOR  ',  'REFERENC'/
      DATA TLIMIT / 0,0,0,0,999,999,999,999,999,999,999,999,0,0/
C-----------------------------------------------------------------------
      NRCX = 1
C                                       Find symbol
      NPNT = 1
      CALL IDWCRD (CARD, NN, TLIMIT, TWORD, NPNT, SYMBOL, TABNO, N,
     *   JERR)
C                                       Exit on finding END
      ENDKEY = SYMBOL(1:3).EQ.'END'
      IF (ENDKEY) GO TO 999
C                                       unrecognized keyword =
      IF (JERR.EQ.1) THEN
C                                       check keyword limit
         IF (NUMKEY.LT.MAXKEY) THEN

            CALL GETKEY (SYMBOL, CARD, NPNT, NUMKEY, KEYWRD, KEYVAL,
     *         KEYCHR, KEYTYP, JERR)
C                                       ignore invalid ones.
            IF ((KEYTYP(NUMKEY).LE.0) .OR. (KEYTYP(NUMKEY).GT.6))
     *         THEN
               WRITE (MSGTXT,1001) SYMBOL
               CALL MSGWRT (6)
               NUMKEY = NUMKEY - 1
C                                       else Key is ok
C                                       Set common number of keywords
            ELSE
               ITANKY = NUMKEY
               END IF
C                                       blew keyword limit
         ELSE
            IF (WARN) THEN
               WRITE (MSGTXT,1002) MAXKEY
               CALL MSGWRT (6)
               WARN = .FALSE.
               END IF
            END IF
      END IF
C                                       If not recognised add to
C                                       history if not single dish
      IF ((JERR.NE.0) .AND. (JERR.NE.1)) THEN
C                                       Not normal keyword=value card.
         IF (CARD(1:8).EQ.SNGDSH) THEN
C                                       Process selected SINGLDSH
C                                       keywords.
C           CALL SDTCRD (ICARD, FITBLK, MAXKEY, NUMKEY, KEYWRD,
C     *        KEYVAL, KEYCHR, KEYTYP, WARN, NRCX[B)
C                                       table type 'SD'
            ITYPE = 'SD'
         ELSE
C                                       No "KEYWORD =" put card in
C                                       history if history exists
            IF (HERR.EQ.0) THEN
               IHIERR = IHIERR + 1
C                                       Explanation for first hist.
               IF (IHIERR.LE.0) THEN
                  WRITE (HILINE,1110) TSKNAM
                  CALL HIADD (HLUN, HILINE, HBLK, HERR)
                  END IF
C                                       History card begins with
C                                       HISTROY, COMMENT or blanks
               ISHIS = (CARD(:8).EQ.HISTRY) .OR. (CARD(:8).EQ.COMENT)
     *            .OR. (CARD(:8).EQ.' ')
               IST = 1
C                                       remove word 'HISTORY'
               IF (ISHIS) IST = 9
               CALL HIAD80 (HLUN, IST, CARD, HBLK, HERR)
               IF (.NOT.ISHIS) THEN
                  WRITE (MSGTXT,1100) SYMBOL
                  CALL MSGWRT (6)
                  END IF
               END IF
            END IF
         END IF
C                                       If a Recognized keyword
      IF (JERR.EQ.0) THEN
C                                       Figure out what we have.
         GO TO (210, 220, 230, 240, 250, 260, 270,
     *       280, 290, 300, 310, 320, 330, 330), TABNO
C                                       Number of fields.
 210     CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 990
         ITNCOL = X + .5
         IF (ITNCOL.GT.127) THEN
            WRITE (MSGTXT,1210) ITNCOL
            CALL MSGWRT (7)
            MSGTXT = 'WILL KEEP ONLY THE FIRST 127 COLUMNS'
            CALL MSGWRT (7)
            ITNCOL = 127
            END IF
         GO TO 999
C                                       Extension name.
 220     NBYT = 24
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         EXTNAM = STR(1:NCHAR)
C                                       If standard AIPS table type
C                                       then EXTNAM is of the form
C                                       'AIPS XX' where XX is the
C                                       table type.
         IF ((EXTNAM(1:5).NE.'AIPS ').AND.(EXTNAM(1:4).NE.'VLA '))
     *      THEN
            ITYPE = 'UK'
         ELSE
            ITYPE = STR(6:7)
            END IF
         GO TO 999
C                                       Extension version.
 230     CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 990
         EXTVER = X + .01
         GO TO 999
C                                       Extension level.
 240     CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 990
         EXTLEV = X + .01
         GO TO 999
C                                       Field type.
 250     NBYT = 24
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         TTYPE(N) = STR(1:NCHAR)
         GO TO 999
C                                       Starting column.
 260     CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 990
         TBCOL(N) = X + .5
         GO TO 999
C                                       Field format.
 270     NBYT = 8
         CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
         TFORM(N) = STR(1:NCHAR)
C                                       Decode format.
C                                       ASCII table
            CALL TABSPC (TFORM(N), COUNT, TWIDTH(N), TFRAC(N),
     *              KTYPE, JERR)
            TFCODE(N) = KTYPE + (10 * COUNT)
C                                       characters or bit length in dimension
            IF (KTYPE.EQ.3 .OR. KTYPE.EQ.7)
     *         TFCODE(N) = KTYPE + (10 * TWIDTH(N))
            GO TO 999
C                                       Field units.
 280        NBYT = 8
            CALL GETSTR (CARD, 80, NBYT, NPNT, STR, NCHAR)
            TUNIT(N) = STR(1:NCHAR)
            GO TO 999
C                                       Field scale.
 290        CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 990
            TSCAL(N) = X
            GO TO 999
C                                       Field offset.
 300        CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 990
            TZERO(N) = X
            GO TO 999
C                                       Field null value.
 310        NBYT = 8
            CALL GETSTR (CARD, 24, NBYT, NPNT, STR, NCHAR)
            TNULL(N) = STR(1:NCHAR)
            GO TO 999
C                                       Column width or dimension.
 320        CALL GETNUM (CARD, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 990
C                                       if not characters or bit type
            KTYPE = MOD ( TFCODE(N), 10)
            IF (KTYPE.NE.3 .AND. KTYPE.NE.7) THEN
               COUNT = X + 0.01
               TFCODE(N) = KTYPE + (10 * COUNT)
               END IF
            GO TO 999
C                                       AUTHOR or REFERENC add to hist.
 330     CONTINUE
            WRITE (HILINE,1120,ERR=999) TSKNAM
            ICARD = MIN(ITRIM(CARD),66)
            HILINE(7:7+ICARD-1) = CARD(1:ICARD)
            CALL HIADD (HLUN, HILINE, HBLK, HERR)
            GO TO 999
         END IF
      GO TO 999
C
 990  IERR = 1
      MSGTXT = 'TABHDK: NUMBER OUT OF RANGE ON ' // SYMBOL
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TABHDK: ERROR READING TABLE EXTENSION FROM TAPE')
 1001 FORMAT ('TABHDK: KEYWORD ',A8,' OF INDETERMINATE DATA TYPE')
 1002 FORMAT ('TABHDK: BLEW NO. OF KEYWORD LIMIT OF ',I5,' IGNORE REST')
 1100 FORMAT ('TABHDK: UNKNOWN SYMBOL ',A8)
 1110 FORMAT (A6,'/ UNKNOWN CARDS IN TABLE HEADER')
 1120 FORMAT (A6,A66)
 1210 FORMAT ('TABHDK:',I5,' COLUMNS EXCEEDS AIPS LIMIT OF 127')
      END

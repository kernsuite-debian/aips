      SUBROUTINE TABPAK (NUMKEY, KEYWRD, KEYVAL, KEYCHR, KEYTYP,
     *   KEYV, KEYLOC, BUFFER, IERR)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C  TABPAK puts arrays of table keywords into the form necessary for a
C  write to AIPS table files.
C  Inputs:
C     NUMKEY I            Number of keywords to read/write.
C     KEYWRD C(NUMKEY)*8  Keywords to read/write
C     KEYVAL D(NUMKEY)    Value of Keyword for numeric types
C     KEYCHR C(NUMKEY)*8  Value of Keyword for character type
C     KEYTYP I(NUMKEY)    The type code of the keywords:
C  Output:
C     KEYV   I(2*NUMKEY)  The array of keyword values
C     KEYLOC I(NUMKEY)    The word offset of first short integer
C                         word of keyword value in array KEYV.
C     BUFFER I(*)         Buffer being use for table I/O >= 512 words.
C     IERR   I            Error code. 0=ok. >0 => Error
C-----------------------------------------------------------------------
      CHARACTER KEYWRD(*)*8, KEYCHR(*)*8
      INTEGER   NUMKEY, KEYTYP(*), KEYV(*), KEYLOC(*), BUFFER(*), IERR
      DOUBLE PRECISION KEYVAL(*)
C                                       Define max number of keywords
      DOUBLE PRECISION KEYD
      REAL      KEYR(2)
      HOLLERITH KEYH(2)
      LOGICAL   KEYL
      INTEGER   KEYI, IKEY, II, LENKEY(5)
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (KEYL, KEYI, KEYH, KEYR, KEYD)
      DATA LENKEY / 4, 1, 2, 1, 1/
C-----------------------------------------------------------------------
C                                       Length of keyword values
      LENKEY(1) = NWDPDP
C                                       Prepare keywords
      IKEY = 1
      DO 30 II = 1,NUMKEY
         KEYLOC(II) = IKEY
         IF (KEYTYP(II).EQ.1) KEYD = KEYVAL(II)
         IF (KEYTYP(II).EQ.2) KEYR(1) = KEYVAL(II)
         IF (KEYTYP(II).EQ.3) CALL CHR2H (8, KEYCHR(II), 1, KEYH)
         IF (KEYTYP(II).EQ.4) KEYI = KEYVAL(II)
         IF (KEYTYP(II).EQ.5) KEYL = KEYVAL(II).GT.0.0D0
         CALL COPY (LENKEY(KEYTYP(II)), KEYD, KEYV(IKEY))
         IKEY = IKEY + LENKEY(KEYTYP(II))
 30      CONTINUE
C                                       Write keywords.
      CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KEYLOC,
     *         KEYV, KEYTYP, IERR)
 999  RETURN
C-----------------------------------------------------------------------
      END

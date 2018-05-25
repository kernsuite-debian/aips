      SUBROUTINE TABREQ (CARDS, XTENS, BITPIX, NAXIS, IERR)
C-----------------------------------------------------------------------
C! parse FITS tape record for required extension file FITS keywords
C# FITS tape EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   TABREQ parses FITS cards, searching for
C   required cards of a FITS extension header block, namely XTENSION,
C   BITPIX, NAXIS, NAXISn, PCOUNT, GCOUNT.
C   Inputs via common:
C      CARDS     C(3)*80 Input First FITS cards of a TABLE
C   Outputs:
C      XTENS     C*16    Extension type
C      BITPIX    I       Bits per Pixel, 8=Ascii
C      NAXIS     I       Number of dimensions
C      IERR      I       0=ok, 1=first Keyword not XTENSION,
C                        2=BITPIX error, 3=NAXIS error
C                        An error message will be printed.
C-----------------------------------------------------------------------
      CHARACTER XTENS*16, CARDS(*)*80
      INTEGER   BITPIX, NAXIS, IERR
C
      DOUBLE PRECISION X
      CHARACTER EWORD(3)*8, STRING*80, CARD*80, SYMBOL*8
      INTEGER   IKEYWD, NPNT, ITABNO, NCHAR, IFOUND
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA EWORD /'XTENSION', 'BITPIX  ',  'NAXIS   '/
C-----------------------------------------------------------------------
C                                       Look for XTENSION= 'type' card
      IKEYWD = 1
      CARD = CARDS(IKEYWD)
      NPNT = 1
      CALL IDWCRD (CARD, 1, 0, EWORD(IKEYWD), NPNT, SYMBOL, ITABNO,
     *   IFOUND, IERR)
      IF ((IERR.NE.0) .OR. (SYMBOL.EQ.'END')) THEN
         IERR = 1
         GO TO 999
         END IF
      CALL GETSTR (CARD, 80, 16, NPNT, STRING, NCHAR)
      XTENS = STRING(1:NCHAR)
C
      IKEYWD = 2
      NPNT = 1
      CARD = CARDS(IKEYWD)
      CALL IDWCRD (CARD, 1, 0, EWORD(IKEYWD),  NPNT, SYMBOL, ITABNO,
     *   IFOUND, IERR)
      IF ((IERR.NE.0) .OR. (SYMBOL.EQ.'END')) GO TO 970
C                                       Get value of BITPIX
      CALL GETNUM (CARD, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      IF (X.GE.0.0) BITPIX = X + 0.1
      IF (X.LT.0.0) BITPIX = X - 0.1
C                                       Warning-Non standard bits/pixel
      IF ((BITPIX.NE.8) .AND. (BITPIX.NE.16) .AND. (BITPIX.NE.32) .AND.
     *   (BITPIX.NE.-32)) THEN
         WRITE (MSGTXT,1010) BITPIX
         CALL MSGWRT (5)
         END IF
C                                       Get NAXIS
      IKEYWD = 3
      NPNT = 1
      CARD = CARDS(IKEYWD)
      CALL IDWCRD (CARD, 1, 0, EWORD(IKEYWD),  NPNT, SYMBOL, ITABNO,
     *   IFOUND, IERR)
      IF ((IERR.NE.0) .OR. (SYMBOL.EQ.'END')) GO TO 970
      CALL GETNUM (CARD, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      NAXIS = X + .01
C                                       Invalid number of axis.
 960  IF (NAXIS.LT.50) GO TO 999
      WRITE (MSGTXT,1960) NAXIS
      IERR = IKEYWD
      GO TO 980
C                                       Expected keyword not found.
 970  WRITE (MSGTXT,1970) EWORD(IKEYWD), SYMBOL
      IERR = IKEYWD
      GO TO 980
 975  MSGTXT = 'TABREQ: NUMBER OUT OF RANGE ON ' // SYMBOL
      IERR = 1
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR READING FITS TABLE')
 1010 FORMAT ('INVALID TABLE BITS PER PIXEL =',I6)
 1960 FORMAT ('NUMBER OF TABLE AXIS TOO LARGE FOR BUFFER =',I6)
 1970 FORMAT ('EXPECTED TABLE KEYWORD ',A8,'. FOUND ',A8,'.')
      END

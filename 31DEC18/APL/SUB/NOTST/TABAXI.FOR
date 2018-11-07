      SUBROUTINE TABAXI (CARDS, NAXIS, NAXIES, PCOUNT, GCOUNT, IERR)
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
C   TABAXI parses FITS cards, searching for specification of AXIES
C   and Random and GROUP count
C   Inputs:
C      CARDS     C(*)*80 Input First FITS cards of a TABLE
C      NAXIS     I       Number of dimensions
C   Outputs:
C      NAXIES    I(NAXIS) Length of each dimension
C      PCOUNT    I       Random parameter count
C      GCOUNT    I       Groupt count
C      IERR      I       0=ok, 4=NAXIES error
C                        5=PCOUNT error, 6=GOUNT error,
C                        An error message will be printed.
C-----------------------------------------------------------------------
      CHARACTER CARDS(*)*80
      INTEGER   NAXIS, NAXIES(*), PCOUNT, GCOUNT, IERR
C
      DOUBLE PRECISION X
      CHARACTER EWORD(3)*8, CARD*80, SYMBOL*8
      INTEGER   IKEYWD, NPNT, ITABNO, I, IFOUND
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA EWORD / 'NAXIS   ', 'PCOUNT  ', 'GCOUNT  '/
C-----------------------------------------------------------------------
C                                       Find NAXISi
      IKEYWD = 1
      DO 50 I = 1,NAXIS
         NPNT = 1
         CARD = CARDS(I)
C                                       Match 'NAXIS' part of NAXISn
         CALL IDWCRD (CARD, 1, 5, EWORD(IKEYWD), NPNT, SYMBOL, ITABNO,
     *      IFOUND, IERR)
C                                        IFOUND is the dimension,
C                                        which must be in order
         IF ((IERR.NE.0) .OR. (SYMBOL.EQ.'END') .OR. (IFOUND.NE.I)) THEN
            WRITE (EWORD(IKEYWD),1940,ERR=970) I
            GO TO 970
            END IF
         CALL GETNUM (CARD, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 975
         NAXIES(I) = X + .01
 50      CONTINUE
C                                       PCOUNT
      IKEYWD = 2
      NPNT = 1
      CARD = CARDS(NAXIS+1)
      CALL IDWCRD (CARD, 1, 0, EWORD(IKEYWD),  NPNT, SYMBOL, ITABNO,
     *   IFOUND, IERR)
      IF ((IERR.NE.0) .OR. (SYMBOL.EQ.'END')) GO TO 970
      CALL GETNUM (CARD, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      PCOUNT = X + 0.01
C                                       Get GCOUNT
      IKEYWD = 3
      NPNT = 1
      CARD = CARDS(NAXIS+2)
      CALL IDWCRD (CARD, 1, 0, EWORD(IKEYWD),  NPNT, SYMBOL, ITABNO,
     *   IFOUND, IERR)
      IF ((IERR.NE.0) .OR. (SYMBOL.EQ.'END')) GO TO 970
      CALL GETNUM (CARD, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      GCOUNT = X + 0.01
      GO TO 999
C                                       Expected keyword not found.
 970  WRITE (MSGTXT,1970) EWORD(IKEYWD), SYMBOL
      IERR = IKEYWD + 3
      GO TO 980
 975  MSGTXT = 'TABAXI NUMBER OUT OF RANGE ON ' // SYMBOL
      IERR = 1
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR READING FITS TABLE')
 1940 FORMAT ('NAXIS',I2,' ')
 1970 FORMAT ('EXPECTED TABLE KEYWORD ',A8,'. FOUND ',A8,'.')
      END

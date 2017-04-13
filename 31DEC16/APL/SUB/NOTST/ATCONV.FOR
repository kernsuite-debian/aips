      SUBROUTINE ATCONV (NUMKEY, KEYWRD, KEYVAL, KEYTYP, KEYCHR, SRTORD)
C-----------------------------------------------------------------------
C! Fix AIPS FITS tables
C# FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2011
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
C   This routine will fix up table files that conform to AIPS
C   conventions.  In particular, this subroutine will find and remove
C   the sort order information in the keyword value pair array.
C   In/out:
C      NUMKEY   I          Number of keyword value pairs.
C      KEYWRD   C(100)*8   Keyword
C      KEYVAL   D(100)     Keyword values (KEYVAL(*))
C      KEYTYP   I(100)     keyword type
C   Output:
C      SRTORD   I          Sort order.
C   Common
C      /THDR/
C-----------------------------------------------------------------------
      INTEGER   NUMKEY, KEYTYP(*), SRTORD
      CHARACTER KEYWRD(*)*8, KEYCHR(*)*(*)
      DOUBLE PRECISION KEYVAL(100)
C
      CHARACTER ISORTU*8, CHLOG*8
      INTEGER   I, LOOPSZ, II, ISTART
      INCLUDE 'INCS:DTHD.INC'
      DATA ISORTU, CHLOG /'ISORTORD','LOGICAL '/
C-----------------------------------------------------------------------
      DO 200 I = 1,ITNCOL
C                                       Check for a logical.
         IF (TUNIT(I).EQ.CHLOG) TFCODE(I) = 5
C                                       Short width means short int.
         IF ((TFCODE(I).EQ.4).AND.(TWIDTH(I).LT.7)) TFCODE(I) = 6
 200     CONTINUE
C                                       Check for AIPS table sort order.
      SRTORD = 0
      IF (NUMKEY.EQ.0) GO TO 999
      LOOPSZ = NUMKEY
      DO 300 I = 1,LOOPSZ
C                                       Found "ISORTORD".
         IF (KEYWRD(I).NE.ISORTU) GO TO 300
            IF (KEYVAL(I).GE.0.0) SRTORD = KEYVAL(I) + .1
            IF (KEYVAL(I).LT.0.0) SRTORD = KEYVAL(I) - .1
C                                       Ripple the rest of the stuff
C                                       down covering up ISORTORD which
C                                       will be kept in internal format
C                                       rather than as a KEYVAL value
C                                       pair.
            ISTART = I + 1
            IF (ISTART.LE.LOOPSZ) THEN
               DO 250 II = ISTART,LOOPSZ
                  KEYWRD(II-1) = KEYWRD(II)
                  KEYVAL(II-1) = KEYVAL(II)
                  KEYTYP(II-1) = KEYTYP(II)
                  KEYCHR(II-1) = KEYCHR(II)
 250              CONTINUE
               END IF
            NUMKEY = NUMKEY - 1
            GO TO 999
 300     CONTINUE
C
 999  RETURN
      END

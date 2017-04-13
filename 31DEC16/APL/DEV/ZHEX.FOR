      SUBROUTINE ZHEX (IVAL, NC, HVAL)
C-----------------------------------------------------------------------
C! encode an integer into hexadecimal (base 16) characters
C# Z Service
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
C  Convert decimal to nc character hexidecimal 'string' - leading
C  blanks are made into 0's, i.e. "illegal" Fortran format Znc.nc
C  The extended hexadecimal (base 36) is done by ZEHEX.
C  Input:
C     IVAL   I       Decimal value to convert
C     NC     I       Width of receiving field
C  Output:
C     HVAL   C*(*)   String receiving hexidecimal conversion
C  Generic version - klunky but works
C-----------------------------------------------------------------------
      INTEGER   IVAL, NC
      CHARACTER HVAL*(*)
C
      INTEGER   I, J, JVAL, L, IBITS(64), LC
      CHARACTER HEXC*16
      INCLUDE 'INCS:DDCH.INC'
      DATA HEXC /'0123456789ABCDEF'/
C-----------------------------------------------------------------------
      HVAL = ' '
      LC = MIN (NC, LEN(HVAL))
C                                       get the bits
      CALL ZGTBIT (NBITWD, IVAL, IBITS)
      L = NBITWD / 4
      DO 10 I = 1,LC
         IF (I.LE.L) THEN
            J = I * 4 - 3
            CALL ZPTBIT (4, JVAL, IBITS(J))
            JVAL = JVAL + 1
         ELSE
            JVAL = 1
            END IF
         J = LC + 1 - I
         HVAL(J:J) = HEXC(JVAL:JVAL)
 10      CONTINUE
C                                       check overflow
      IF (LC.LT.L) THEN
         J = LC * 4 + 1
         CALL ZPTBIT (4, JVAL, IBITS(J))
         IF (JVAL.GT.0) HVAL(1:1) = '*'
         END IF
C
 999  RETURN
      END

      SUBROUTINE ZEHEX (IVAL, NC, HVAL)
C-----------------------------------------------------------------------
C! encode an integer into extended hexadecimal (base 36) characters
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
C  Convert decimal to nc character extended hexidecimal 'string' -
C  leading blanks are made into 0's.
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
      INTEGER   I, J, JVAL, LVAL, LC
      CHARACTER HEXC*36
      INCLUDE 'INCS:DDCH.INC'
      DATA HEXC /'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-----------------------------------------------------------------------
      HVAL = ' '
      LC = MIN (NC, LEN(HVAL))
C                                       loop through digits
      LVAL = IVAL
      DO 10 I = 1,LC
         J = LC + 1 - I
         JVAL = MOD (LVAL, 36) + 1
         HVAL(J:J) = HEXC(JVAL:JVAL)
         LVAL = LVAL / 36
 10      CONTINUE
C
 999  RETURN
      END

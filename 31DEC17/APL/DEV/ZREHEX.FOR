      SUBROUTINE ZREHEX (NC, HVAL, IVAL)
C-----------------------------------------------------------------------
C! converts extended hexadecimal (base 36) characters into integer
C# Z Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1999
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
C  Convert nc character extended hexidecimal 'string' to decimal
C  Input:
C     NC     I       Width of receiving field
C     HVAL   C*(*)   Extended hexidecimal string
C  Output:
C     IVAL   I       Decimal value to convert
C  Generic version - klunky but works
C-----------------------------------------------------------------------
      INTEGER   IVAL, NC
      CHARACTER HVAL*(*)
C
      INTEGER   I, J, LC
      CHARACTER HEXC*36
      INCLUDE 'INCS:DDCH.INC'
      DATA HEXC /'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-----------------------------------------------------------------------
      LC = MIN (NC, LEN(HVAL))
      IVAL = 0
C                                       loop through digits
      DO 20 I = 1,LC
         DO 10 J = 1,36
            IF (HEXC(J:J).EQ.HVAL(I:I)) GO TO 15
 10         CONTINUE
         GO TO 999
C                                       char found
 15      IVAL = IVAL * 36 + J - 1
 20      CONTINUE
C
 999  RETURN
      END

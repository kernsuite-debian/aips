      SUBROUTINE ZRHPRL (NVAL, NP, INB, OUTB)
C-----------------------------------------------------------------------
C! convert 32-bit HP floating buffer to local REAL values
C# Binary
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
C   Converts from 32 bit Hewitt-Packard floating format to local single
C   precision.
C      HP R*4 format:
C                1         2         3
C      01234567890123456789012345678901
C      smmmmmmmmmmmmmmmmmmmmmmmeeeeeeez
C   The value can be determined as follows:
C      man = smmmmmmmmmmmmmmmmmmmmmmm is a two's complement signed
C            integer.
C      exp = zeeeeeee is a two's complement signed integer.
C      value = man * 2.0 ** (exp - 23)
C   Inputs:
C      NVAL   I      Number of values to convert
C      NP     I      First value in INB to convert
C      INB    R(*)   32-bit HP format values
C   Output:
C      OUTB   R(*)   Local format values.
C   Generic version; should work on any machine.
C-----------------------------------------------------------------------
      INTEGER   NVAL, NP
      REAL      INB(*), OUTB(*)
C
      INTEGER LOOP, NB, L24, IEXP, IEX, IEXMAX, IEXMIN, ITEMP(4), IESIGN
      REAL    HPV4
      DATA IEXMAX, IEXMIN /103, -103/
C-----------------------------------------------------------------------
      NB = NP
      DO 500 LOOP = 1,NVAL
         HPV4 = 0.0
C                                       mantissa
         CALL ZI8IL (4, 1, INB(NB), ITEMP)
         IF (ITEMP(1).GE.128) THEN
C                                       Negative
            L24 = ((ITEMP(1)-255) * 65536) + ((ITEMP(2)-255) * 256) +
     *         (ITEMP(3)-255) - 1
         ELSE
C                                       Positive
            L24 = ((ITEMP(1) * 65536) + (ITEMP(2) * 256) + ITEMP(3))
            END IF
         IF (L24.NE.0) THEN
C                                       exponent
            IEXP = ITEMP(4) / 2
C                                       exponent sign
            IESIGN = ITEMP(4) - (ITEMP(4)/2) * 2
            IF (IESIGN.NE.0) IEXP = IEXP - 128
C                                       Correct for radix point in
C                                       mantessa.
            IEX = IEXP - 23
C                                       Set to zero when an overflow or
C                                       underflow occures.
            IF ((IEX.GT.IEXMAX) .OR. (IEX.LT.IEXMIN)) GO TO 490
            HPV4 = L24 * (2.0**IEX)
            END IF
 490     NB = NB + 1
         OUTB(LOOP) = HPV4
 500     CONTINUE
 999  RETURN
      END

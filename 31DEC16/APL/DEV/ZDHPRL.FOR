      SUBROUTINE ZDHPRL (NVAL, NP, INB, OUTB)
C-----------------------------------------------------------------------
C! convert 64-bit HP floating buffer to local DOUBLE PRECISION values
C# Z Binary
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
C   Converts from 64 bit Hewitt-Packard floating format to local double
C   precision.
C      HP R*8 format:
C                1         2         3         4         5         6
C      0123456789012345678901234567890123456789012345678901234567890123
C      smmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmnnnnnnnnnnnnnnnnnnnnnnnneeeeeeez
C   The value can be determined as follows:
C      man1 = smmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm is a two's complement
C             signed integer.
C      man2 = nnnnnnnnnnnnnnnnnnnnnnnn is a POSITIVE unsigned integer.
C      exp = zeeeeeee is a two's complement signed integer.
C      value = (man1 * 16777216 + man2) * 2.0D0 ** (exp - 55)
C   Inputs:
C      NVAL   I      Number of values to convert
C      NP     I      First value in INB to convert
C      INB    D(*)   64-bit HP format values
C   Output:
C      OUTB   D(*)   Local format values.
C   Generic version; should work on any machine.
C-----------------------------------------------------------------------
      INTEGER   NVAL, NP
      DOUBLE PRECISION INB(*), OUTB(*)
C
      INTEGER   LOOP, NB, L24, L32, IEXP, IEX, IEXMAX, ITEMP(4), IESIGN
      DOUBLE PRECISION HPV8, MAN, F32, F24, DFLOAT
      DATA IEXMAX /72/
C-----------------------------------------------------------------------
      NB = NP
      DO 500 LOOP = 1,NVAL
         HPV8 = 0.0D0
C                                       mantissa (msb)
         CALL ZI32IL (1, 1, INB(NB), ITEMP)
         L32 = ITEMP(1)
C                                       (lsb)
         CALL ZI8IL (4, 5, INB(NB), ITEMP)
         L24 = (ITEMP(1) * 65536) + (ITEMP(2) * 256) + ITEMP(3)
         F32 = DFLOAT (L32)
         F24 = DFLOAT (L24)
C                                       This really is right!
         MAN = F32 * 16777216.0D0  + F24
         IF (MAN.NE.0.0D0) THEN
C                                       exponent
            IEXP = ITEMP(4) / 2
C                                       exponent sign
            IESIGN = ITEMP(4) - (ITEMP(4)/2) * 2
            IF (IESIGN.NE.0) IEXP = IEXP - 128
C                                       Correct for radix point in
C                                       mantessa.
            IEX = IEXP - 55
C                                       Set to zero when an overflow
C                                       occures.
            IF (IEX.GT.IEXMAX) GO TO 490
            HPV8 = MAN * (2.0**IEX)
            END IF
 490     NB = NB + 1
         OUTB(LOOP) = HPV8
 500     CONTINUE
C
 999  RETURN
      END

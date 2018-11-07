      SUBROUTINE ZGTBIT (NBITS, WORD, BITS)
C-----------------------------------------------------------------------
C! get array of bits from a word
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
C   Get the lowest order "nbits" bits of the bit pattern in "word"
C   and return them in the array BITS with the lsb in bits[0].  For
C   example, if
C   word  = 0 0 0 0 0 1 0 1 ... 0 0 0 1 0 0 1 1
C           ^                                 ^
C          MSB                               LSB
C   and nbits = 3 then bits[0] = 1 , bits[1] = 1 and bits[2] = 0
C   Inputs:
C      NBITS   I      Number of bits
C      WORD    I      Word from which to extract bits
C   Output:
C      BITS    I(*)   Bit array (values 0 or 1)
C   Generic version - slow.
C-----------------------------------------------------------------------
      INTEGER   NBITS, WORD, BITS(*)
C
      INTEGER   I, J, K, L, ZAND
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (NBITS.GT.0) THEN
         L = NBITS + 1
         L = MIN (L, NBITWD) - 1
         DO 10 I = 1,L
            J = 2 ** (I-1)
            K = ZAND (WORD, J)
            BITS(I) = 0
            IF (K.NE.0) BITS(I) = 1
 10         CONTINUE
         END IF
C
 999  RETURN
      END

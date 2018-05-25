      SUBROUTINE ZPTBIT (NBITS, WORD, BITS)
C-----------------------------------------------------------------------
C! put array of bits into a word
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
C   Build WORD from NBITS bit values contained in the array BITS, where
C   BITS[1] supplies the lsb, BITS[2] the next higher bit, etc.  The
C   rest of the bits in WORD are set to zero.  For example, if
C
C   bits[1:*] = 0 1 1 1 0 1 0 1 ... 0 1 1 1 0 0 0 0
C               ^                                 ^
C              LSB                               MSB
C   and NBITS = 4, then WORD = 14 (decimal)
C   Inputs:
C      NBITS   I      Number of bits to use from the array "bits"
C      BITS    I(*)   Array of bit values (0 or 1)
C   Output:
C      WORD    I      Result containing bit pattern from the first
C                     "nbits" values of the array "bits"
C   Generic version - stub
C-----------------------------------------------------------------------
      INTEGER   NBITS, WORD, BITS(*)
C
      INTEGER   I, L
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (NBITS.GT.0) THEN
         L = NBITS + 1
         L = MIN (L, NBITWD) - 1
         WORD = 0
         DO 10 I = 1,L
            IF (BITS(I).GT.0) WORD = WORD + 2 ** (I-1)
 10         CONTINUE
         END IF
C
 999  RETURN
      END

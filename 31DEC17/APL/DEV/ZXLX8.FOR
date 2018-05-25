      SUBROUTINE ZXLX8 (NBIT, INB, OUTB)
C-----------------------------------------------------------------------
C! convert AIPS bit array to FITS binary table bit array
C# FITS
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
C   ZXLX8 converts an AIPS table bit array to an FITS bit array. An AIPS
C   bit array has the bits in locations of increasing signifigance
C   NBITWD per short integer.  A FITS bit array has the bits in order of
C   decreasing signifigance with 8 bits per real world byte, zero filled
C   Inputs:
C      NBIT   I      Number of bits
C      INB    I(*)   Input buffer of AIPS bit array data.
C   Output:
C      OUTB   I(*)   Out buffer, note an integral number of 16-bit
C                    integers in OUTB are modified.  OUTB is left in
C                    the form of IEEE integers (i.e. ms byte first).
C   Generic version: works whenever local integer is a multiple of 16
C   bits and has no more than 64 bits.
C-----------------------------------------------------------------------
      INTEGER   NBIT, INB(*), OUTB(*)
C
C                                       NUMBIT = number of bits in
C                                       a local integer (max)
      INTEGER   NUMBIT
      PARAMETER (NUMBIT=64)
C
      INTEGER   IBITS(NUMBIT), OBITS(NUMBIT), IWORK(NUMBIT/16), J,
     *   NBITS, NCONV, NWORDS, NLEFT, LOOP, ILOOP, NLOOP, IPOINT
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      NWORDS = (NBIT-1) / NBITWD + 1
      NLEFT = NBIT
C                                       Loop over input words
      DO 100 LOOP = 1,NWORDS
         NBITS = NBITWD
         IF (NBITS.GT.NLEFT) NBITS = NLEFT
C                                       Input bits are NBITWD per
C                                       integer:
C                                       Extract the bits.
         CALL ZGTBIT (NBITS, INB(LOOP), IBITS)
C                                       Reverse bit order.
         DO 20 ILOOP = 1,NBITS
            OBITS(NBITWD-ILOOP+1) = IBITS(ILOOP)
 20         CONTINUE
C                                       Zero remaining output bits
         J = NBITS + 1
         DO 40 ILOOP = J, NBITWD
            OBITS(NBITWD-ILOOP+1) = 0
 40         CONTINUE
C                                       Output bytes are written 2 at a
C                                       time as 16 bit standard
C                                       integers:
C                                       Repack bits 16 per word
         IPOINT = NBITWD - 15
         NLOOP = (NBITS-1) / 16 + 1
         DO 60 ILOOP = 1,NLOOP
            CALL ZPTBIT (16, IWORK(ILOOP), OBITS(IPOINT))
            IPOINT = IPOINT - 16
C                                       Funny business to avoid
C                                       overflows.
            IF (IWORK(ILOOP).GT.32767)
     *         IWORK(ILOOP) = IWORK(ILOOP) - 65536
 60         CONTINUE
C                                       Convert to standard 16 bit
C                                       integers.
         NCONV = NLOOP
         CALL ZILI16 (NCONV, IWORK, 1, OUTB(LOOP))
         NLEFT = NLEFT - NBITS
 100     CONTINUE
C
 999  RETURN
      END

      SUBROUTINE ZX8XL (NBIT, INB, OUTB)
C-----------------------------------------------------------------------
C! convert FITS table bit array to AIPS bit array
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
C   ZXLX8 converts an FITS table bit array to an AIPS bit array. An AIPS
C   bit array has the bits in locations of increasing signifigance
C   NBITWD per integer.  A FITS bit array has the bits in order of
C   decreasing signifigance with 8 bits per real world byte, zero filled
C   Inputs:
C      NBIT   I      Number of bits
C      INB    I(*)   Input buffer of FITS bit array data as 8-bit byte
C                    stream.
C   Output:
C      OUTB   I(*)   Out buffer of AIPS bit array data.
C   Generic version: works whenever local integer is a multiple of 16
C   bits and has no more than 64 bits.
C-----------------------------------------------------------------------
      INTEGER   NBIT, INB(*), OUTB(*)
C                                       NUMBIT = number of bits in
C                                       a local integer
      INTEGER   NUMBIT
      PARAMETER (NUMBIT=64)
      INTEGER   IBITS(NUMBIT), OBITS(NUMBIT), IWORK(NUMBIT/16), J,
     *   NBITS, NCONV, NWORDS, NLEFT, LOOP, ILOOP, NLOOP, IPOINT
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      NWORDS = (NBIT-1) / NBITWD + 1
      NLEFT = NBIT
      NCONV = NBITWD / 16
C                                       Loop over input words
      DO 100 LOOP = 1,NWORDS
         NBITS = NBITWD
         IF (NBITS.GT.NLEFT) NBITS = NLEFT
C                                       Input bits are 16 per
C                                       integer:
C                                       Convert to local integers.
         CALL ZI16IL (NCONV, 1, INB(LOOP), IWORK)
C                                       Extract the bits 16 per word.
         IPOINT = NBITWD - 15
         NLOOP = (NBITS-1) / 16 + 1
         DO 20 ILOOP = 1,NLOOP
C                                       Trap local integer > 16 bits
C                                       and sign bit turned on.
            IF ((NBITWD.GT.16) .AND. (IWORK(ILOOP).LT.0))
     *         IWORK(ILOOP) = 65536 + IWORK(ILOOP)
            CALL ZGTBIT (16, IWORK(ILOOP), IBITS(IPOINT))
            IPOINT = IPOINT - 16
 20         CONTINUE
C                                       Reverse bit order.
         DO 40 ILOOP = 1,NBITS
            OBITS(ILOOP) = IBITS(NBITWD-ILOOP+1)
 40         CONTINUE
C                                       Zero remaining output bits
         J = NBITS+1
         DO 60 ILOOP = J,NBITWD
            OBITS(ILOOP) = 0
 60         CONTINUE
C                                       Output bytes NBITWD per integer
         CALL ZPTBIT (NBITWD, OUTB(LOOP), OBITS)
         NLEFT = NLEFT - NBITS
 100     CONTINUE
C
 999  RETURN
      END

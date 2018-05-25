      SUBROUTINE ZBYTF2 (NWORDS, BYTFLP, NBITWD, INBUF, OUTBUF)
C----------------------------------------------------------------------
C! interchange bytes in buffer if needed to go between local & standard
C# Z2 Tape FITS
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
C----------------------------------------------------------------------
C   ZBYTF2 will interchange the low order and high order bytes for all
C   words in the input buffer and put the results in an output buffer.
C   The input buffer is unchanged except in the special case where the
C   input buffer has the same starting address as the output buffer.
C   Inputs:
C      NWORDS   I       The length of the input buffer in 16-BIT words.
C      BYTFLP   I       Byte flip code: 0 none, 1 bytes, 2 word, 3 both
C      NBITWD   I       Number bits / word
C      INBUF    I*2(*)  Input buffer
C   Output:
C      OUTBUF   I*2(*)  Buffer containing byte swapped words
C   Generic version - stub
C----------------------------------------------------------------------
      INTEGER   NWORDS, BYTFLP, NBITWD
      INTEGER*2 INBUF(*), OUTBUF(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'ZBYTF2 REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (10)
      IF (NLUSER.GT.0) STOP 'ZBYTF2 missing'
C
 999  RETURN
      END

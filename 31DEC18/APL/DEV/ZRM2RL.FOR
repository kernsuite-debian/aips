      SUBROUTINE ZRM2RL (NWORDS, INBUF, OUTBUF)
C-----------------------------------------------------------------------
C! convert Modcomp to local single precision floating point
C# Z Binary Tape
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
C   Convert Modcomp single precision floating point data into local
C   single precision floating point.
C   Inputs:
C      NWORDS   I      Length of the input buffer in words
C      INBUF    R(*)   Input buffer containing Modcomp R*4 data
C   Output:
C      OUTBUF   R      Output buffer containing local REAL data
C
C   Notes:
C
C      Before call, input buffer should have its bytes flipped via
C      ZI32IL which will leave the values in one local 32-bit integer
C
C      Expects, after word flip, sign bit in bit 31 (1=>negative),
C      bits 22:30 are the  exponent biased by 256(?), bits 0:21 are
C      the normalized fraction.  Negative values are obtained by 2's
C      complement of the whole word.
C
C      Should work inplace.
C
C   Generic version - stub
C-----------------------------------------------------------------------
      INTEGER   NWORDS
      REAL      INBUF(*), OUTBUF(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'ZRM2RL REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (8)
      IF (NLUSER.GT.0) STOP 'ZRM2RL missing'
C
 999  RETURN
      END

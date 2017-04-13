      SUBROUTINE ZBYMOV (NMOVE, INP, INB, OUTP, OUTB)
C-----------------------------------------------------------------------
C! move 8-bit bytes from in-buffer to out-buffer
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
C   Moves a string of bytes from INB to OUTB. Byte = 8 bits (NOT an AIPS
C   half-integer byte).  This is used with tape input and output only.
C   Inputs:
C      NMOVE   I      Number of 8-bit bytes bytes
C      INP     I      First value in INB to move (8-bit byte offset)
C      INB     I(*)   Input buffer.
C      OUTP    I      Location in OUTB to put first 8-bit byte  (8-bit
C                     byte offset)
C   Output:
C      OUTB    I(*)   Output buffer.
C   Generic version - stub.
C-----------------------------------------------------------------------
      INTEGER   NMOVE, INP, OUTP, INB(*), OUTB(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'ZBYMOV: REQUIRES LOCAL DEVELOPMENT - STOPPING'
      CALL MSGWRT (10)
      IF (NLUSER.GT.0) STOP 'ZBYMOV missing'
C
 999  RETURN
      END

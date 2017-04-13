      SUBROUTINE ZVTPX2 (FCB, BUFSW, BUFSR, BUFFER, IERR)
C-----------------------------------------------------------------------
C! writes/reads to/from server for the client (virtual tape) machine
C# Tape Z2
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
C   ZVTPX2 transfers data between the clent (virtual tape) machine and
C   the server (actual tape) computer
C   Inputs:
C      FCB     I(*)   File descriptor
C      BUFSW   I      Number of data words (each 32 bits) to send beyond
C                     the header.
C   In/out:
C      BUFFER  I(*)   Data buffer: header + data
C   Output:
C      BUFSR   I      Number of data words (each 32 bits) read back
C                     beyond the header
C      IERR    I      Error: 0 => okay
C   Generic version - stub.
C   The header format is, in 32-bit words:
C       1,2    H(2)   Subroutine name
C       3      I      BUFSW
C       4      I      BUFSR
C       5      I      IERR
C   Data begins in the 6th 32-bit word in any format agreed between
C   TPMON and the relevant subroutine.  E.g.,
C      ZTAPE:  header   = 'ZTAPE    ', 3, 0, 0
C              data in  = OPcode, LUN, Count
C              out      = only header word 5 (IERR) used
C      ZTPMIO: header   = 'ZTPMIO  ',  4, 2+n, 0              (read)
C              data in  = 'READ', LUN, Nbytes, Buf#
C              data out = SysError, BytesXfer, buffer(N)
C      ZTPMIO: header   = 'ZTPMIO  ',  4+n, 2, 0              (write)
C              data in  = 'WRIT', LUN, Nbytes, Buf#, buffer(N)
C              data out = SysError, BytesXfer
C              where N = # local words to Xfer Nbytes of data
C                    n = # 32-bit words to Xfer Nbytes of data
C              header word 5 (IERR) also used on output.
C-----------------------------------------------------------------------
      INTEGER   FCB(*), BUFSW, BUFSR, BUFFER(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 8
      MSGTXT = 'ZVTPX2: REQUIRES DEVELOPMENT; GENERIC IS A STUB'
      CALL MSGWRT (8)
C
 999  RETURN
      END

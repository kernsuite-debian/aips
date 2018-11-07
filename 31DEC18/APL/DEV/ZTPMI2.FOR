      SUBROUTINE ZTPMI2 (OP, FCB, BUFF, NBYTES, IERR)
C-----------------------------------------------------------------------
C! tape read/write
C# Z2 Tape
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
C   Low level SEQUENTIAL access, large record, double buffered tape I/O.
C   Inputs:
C      OP       C*4    Operation - 'READ', 'WRIT'
C      FCB      I(*)   I/O control block
C      NBYTES   I      Number of bytes to transfer.
C   In/out:
C      BUFF     I(*)   The i/o buffer.
C   Outputs:
C      IERR     I      Error return code: 0 = Success.
C                         1 = File not open.
C                         2 = Operation incorrectly specified.
C                         3 = I/O error.
C                         4 = end of file (no messages)
C                         9 = record was longer than requested
C   Generic version - stub
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   FCB(*), NBYTES, BUFF(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZTPMI2 REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END

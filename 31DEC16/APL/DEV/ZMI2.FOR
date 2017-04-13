      SUBROUTINE ZMI2 (OPER, FCB, BUFF, NBLOCK, NBYTES, IERR)
C-----------------------------------------------------------------------
C! read/write large blocks of data from/to disk, quick return
C# Z2 IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2004
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
C   Read/write "nbytes" of data from/to disk starting at virtual block
C   "nblock" using "map" I/O (i.e., double buffered, asynchronous I/O).
C   Block sizes are determined by the value assigned to NBPS as
C   AIPS-bytes in ZDCHIN which is converted to bytes in ZDCHI2 and
C   assigned to Z_nbps.
C   Inputs:
C     OPER      C*4    Operation code "READ" or "WRIT" - real CHARACTER
C                      for VMS, HOLLERITH otherwise
C      FCB      I(*)   File control block for open map disk file
C                      and buffer involved
C      NBLOCK   I      Virtual starting block (1-relative)
C      NBYTES   I      Number of AIPS-bytes (1/2 of a local integer)
C                      to be transferred
C   In/out:
C      BUFF     I(*)   I/O buffer
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         2 => bad opcode
C                         3 => I/O error (also see ZWAI2)
C                         4 => end of file (see ZWAI2)
C   Generic version - stub
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   FCB(*), NBLOCK, NBYTES, BUFF(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZMI2 REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END
